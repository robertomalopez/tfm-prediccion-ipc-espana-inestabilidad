# ============================================================
# PIPELINE AUTOMÁTICO PARA SELECCIÓN SARIMA
# CON VENTANAS TRAIN/TEST ESPECÍFICAS POR EVENTO
# ============================================================

library(tidyverse)
library(tsibble)
library(feasts)
library(fable)
library(fabletools)
library(tseries)
library(lubridate)
library(rlang)
library(readxl)
library(dplyr)
library(ggplot2)


# ============================================================
# 1. FUNCIÓN AUXILIAR: TEST ADF + KPSS
# ============================================================

evaluar_estacionariedad <- function(x, alpha = 0.05) {
  
  x <- na.omit(as.numeric(x))
  
  adf_res <- tryCatch(
    adf.test(x),
    error = function(e) NULL
  )
  
  kpss_res <- tryCatch(
    kpss.test(x),
    error = function(e) NULL
  )
  
  p_adf <- if (!is.null(adf_res)) adf_res$p.value else NA_real_
  p_kpss <- if (!is.null(kpss_res)) kpss_res$p.value else NA_real_
  
  decision <- case_when(
    p_adf < alpha & p_kpss >= alpha ~ "estacionaria",
    p_adf >= alpha & p_kpss < alpha ~ "no_estacionaria",
    TRUE ~ "ambigua"
  )
  
  tibble(
    p_adf = p_adf,
    p_kpss = p_kpss,
    decision_estacionariedad = decision
  )
}



# ============================================================
# 1B. FUNCIÓN AUXILIAR: CRITERIO "MODELO_LIMPIO"
# ============================================================
# Este criterio prioriza la capacidad predictiva fuera de muestra:
#   1. RMSFE como criterio principal.
#   2. MAE como contraste de robustez dentro de modelos con RMSFE similar.
#   3. AICc como desempate secundario.
#   4. Ljung-Box como validación de residuos, no como filtro absoluto.
#   5. Diebold-Mariano solo se informa cuando hay datos suficientes.
#
# Nota metodológica:
# Con un único origen de predicción y horizonte de 6 meses, el test de
# Diebold-Mariano normalmente NO es recomendable, porque hay muy pocas
# observaciones de error. Por eso se deja documentado como "no_aplicable"
# salvo que haya al menos min_obs_dm errores comparables por modelo.
# ============================================================

aplicar_criterio_modelo_limpio <- function(tabla_final,
                                           predicciones_vs_real,
                                           alpha = 0.05,
                                           margen_rmsfe = 0.10,
                                           min_obs_dm = 10) {

  if (nrow(tabla_final) == 0) {
    stop("tabla_final está vacía: no hay modelos candidatos.")
  }

  if (!all(c(".model", "RMSFE", "MAE", "AICc", "diagnostico_ljung") %in% names(tabla_final))) {
    stop("tabla_final no contiene las columnas mínimas requeridas para aplicar modelo_limpio.")
  }

  rmsfe_minimo <- min(tabla_final$RMSFE, na.rm = TRUE)
  limite_rmsfe_candidato <- rmsfe_minimo * (1 + margen_rmsfe)

  modelo_menor_rmsfe <- tabla_final %>%
    arrange(RMSFE, MAE, AICc, BIC) %>%
    slice(1) %>%
    pull(.model)

  # ----------------------------------------------------------
  # Diagnóstico Diebold-Mariano aproximado frente al modelo con
  # menor RMSFE. Se usa únicamente si hay suficientes errores.
  # En ventanas de test de 6 meses quedará normalmente como NA.
  # ----------------------------------------------------------

  columna_fecha_pred <- setdiff( #elimina las columas que se indiquen
    names(predicciones_vs_real), #nombres de columnas de la tabla 
    c(".model", "prediccion", "real", "error", "abs_error", "sq_error")
  )[[1]] 
  
  #Como setdiff() elimina las columnas: 
  #.model
  #prediccion
  #real
  #error
  #abs_error
  #sq_error
  #entonces solo queda la columna con la fecha, que se selecciona haciendo
  # [[1]]. Así, no se depende del nombre de la columna de fecha para poder
  #extraer la columna con la fecha. 

  errores_ancho <- predicciones_vs_real %>%
    select(all_of(columna_fecha_pred), .model, error) %>%
    #dado que se tiene la tabla con tres columnas (formato largo), se hace 
    #pivot y así quedan las columans en filas, cada tipo de modelo en columna,
    #y los errores los valores del cruce de estas dos variables
    tidyr::pivot_wider(
      names_from = .model, #cada modelo constituirá una columna
      values_from = error  #los errores serán los valores
    )
  
  #La siguiente función, recibe "SARIMA" o "ETS", es decir, un objeto tipo 
  #cadena de caracteres con el tipo de modelo; y devuelve una tabla de 1 fila
  #con el resultado del diagnóstico del test DM de cada modelo con respecto 
  #al modelo con el menor RMSFE

  calcular_dm_aproximado <- function(nombre_modelo) {
    
    #En primer lugar, se asignan valores nulos los casos en los que justo 
    #nombre_modelo == modelo_menor_rmsfe, evitando así comparar SARIMA 
    #contra SARIMA por ejemplo. Con esto se asegura que las comparaciones 
    #solo se hagan entre modelos de distinto tipo. 

    if (is.na(nombre_modelo) || nombre_modelo == modelo_menor_rmsfe) {
      return(tibble(
        .model = nombre_modelo,
        dm_stat_vs_mejor_rmsfe = NA_real_,
        dm_p_value_vs_mejor_rmsfe = NA_real_,
        diagnostico_dm = "modelo_referencia_menor_RMSFE"
      ))
    }
    
    #También se asegura que tanto el modelo que se quiere comparar (nombre_modelo)
    #como el modelo de referencia (modelo_menor_rmsfe) tengan una columna de
    #errores creada en errores_ancho, para evitar erorres porque no se haya 
    #generado esta columna. 

    if (!(modelo_menor_rmsfe %in% names(errores_ancho)) || !(nombre_modelo %in% names(errores_ancho))) {
      return(tibble(
        .model = nombre_modelo,
        dm_stat_vs_mejor_rmsfe = NA_real_,
        dm_p_value_vs_mejor_rmsfe = NA_real_,
        diagnostico_dm = "errores_no_disponibles"
      ))
    }
    
    #En este punto ya se pueden crear las variables e_ref y e_mod, con los 
    #errores del modelo con menor RMSFE y el que se quiere comparar.

    e_ref <- errores_ancho[[modelo_menor_rmsfe]]
    e_mod <- errores_ancho[[nombre_modelo]]
    
    #Se calculan las pérdidas al cuadrado 

    perdidas_ref <- e_ref^2
    perdidas_mod <- e_mod^2
    
    ##Se calcula la diferencia de pérdidas
    diferencial <- perdidas_mod - perdidas_ref
    
    #Se asegura que exista, para evitar errores
    diferencial <- diferencial[is.finite(diferencial)]
    
    #n_dm: vble con la diferencia de pérdidas al cuadrado
    n_dm <- length(diferencial)
    
    #Hay que asegurar que n_dm sea menor que el mínimo de observaciones que se
    #especifica como argumento de entrada en la función. Es porque si el número
    #de observaciones del error es demasiado pequeño, entonces no es 
    #recomendable utilizar el test. 
    if (n_dm < min_obs_dm) {
      return(tibble(
        .model = nombre_modelo,
        dm_stat_vs_mejor_rmsfe = NA_real_,
        dm_p_value_vs_mejor_rmsfe = NA_real_,
        diagnostico_dm = paste0("no_aplicable_n_menor_", min_obs_dm)
      ))
    }
    
    #Desviación típica del diferencial de pérdidas
    sd_diferencial <- stats::sd(diferencial, na.rm = TRUE)
    
    #Se asegura que no sea nulo o 0 antes de proseguir. 
    if (is.na(sd_diferencial) || sd_diferencial == 0) {
      return(tibble(
        .model = nombre_modelo,
        dm_stat_vs_mejor_rmsfe = NA_real_,
        dm_p_value_vs_mejor_rmsfe = NA_real_,
        diagnostico_dm = "no_aplicable_varianza_nula"
      ))
    }
    
    #En este punto, se utilizan las vbles: 
    # diferencial
    # sd_diferencial
    # n_dm
    # para obtener el valor del estadístico 

    dm_stat <- mean(diferencial, na.rm = TRUE) / (sd_diferencial / sqrt(n_dm))
    
    #se obtiene el p-valor a partir del estadístico
    dm_p_value <- 2 * stats::pnorm(-abs(dm_stat))
    
    #Salida de la función calcular_dm_aproximado, un objeto tibble que incluye
    #el nombre del modelo, el valor del estadístico, el p-valor, y el
    #diagnóstico del test, que aplica el siguiente criterio ¡: 
    
    ####Si el p-valor < alpha, hay evidencia significativa de que existe una
    #diferencia predictiva entre el modelo y el de referencia
    
    ####Si el p-valor > alpha, entonces se acepta la hipótesis nula, y no hay 
    #evidencia significativa de una diferencia predictiva entre los modelos
    
  
    tibble( 
      .model = nombre_modelo,
      dm_stat_vs_mejor_rmsfe = dm_stat,
      dm_p_value_vs_mejor_rmsfe = dm_p_value,
      diagnostico_dm = case_when(
        dm_p_value < alpha ~ "diferencia_predictiva_significativa",
        dm_p_value >= alpha ~ "sin_diferencia_predictiva_significativa",
        TRUE ~ "no_calculable"
      )
    )
  }
  
  #Se aplica la anterior función a todos los modelos que aparecen en tabla_final

  tabla_dm <- tabla_final %>%
    pull(.model) %>%
    purrr::map_dfr(calcular_dm_aproximado)
  
  #Se unen las columnas incluidas en tabla_dm a tabla_final, creándose así una
  #nueva tabla que se denomina tabla_final_limpia. En esta tabla se incorporan 
  #algunas nuevas columnas que contienen información sobre las variables que 
  #se han ido definiendo en la función. 

  tabla_final_limpia <- tabla_final %>%
    left_join(tabla_dm, by = ".model") %>%
    mutate(
      rmsfe_minimo = rmsfe_minimo, #valor mínimo de RMSFE entre los modelos
      margen_rmsfe = margen_rmsfe, #el valor de margen que se dio como entrada 
      limite_rmsfe_candidato = limite_rmsfe_candidato,#vble definida
      diferencia_relativa_RMSFE = (RMSFE / rmsfe_minimo) - 1,
      candidato_modelo_limpio = RMSFE <= limite_rmsfe_candidato, #aquellos que
      #dan alguna info según el test de Diebold-Mariano 
      residuos_ok_modelo_limpio = diagnostico_ljung == "residuos_ok",
      prioridad_residuos_modelo_limpio = case_when( #clasificación de modelos
        #según su diagnostico_ljung
        diagnostico_ljung == "residuos_ok" ~ 1L,
        diagnostico_ljung == "no_calculable" ~ 2L,
        diagnostico_ljung == "autocorrelacion_residual" ~ 3L,
        TRUE ~ 4L
      ),
      
      #clasificación de modelos según el test DM y sus residuos, distinguiendo: 
      
      #1 modelos cuyo RMSFE es similar al menor y sus residuos se comportan bien
      #2 modelos cuyo RMSFE es similar al menor y sus residuos no se comportan bien
      #3 modelos que están fuera del margen del menor RMSFE, por lo que carece de
      #sentido tenerlo en cuenta. 
      
      
      criterio_modelo_limpio = case_when( #
        candidato_modelo_limpio & residuos_ok_modelo_limpio ~ "candidato_RMSFE_similar_residuos_ok",
        candidato_modelo_limpio & !residuos_ok_modelo_limpio ~ "candidato_RMSFE_similar_residuos_no_ok",
        !candidato_modelo_limpio ~ "fuera_margen_RMSFE",
        TRUE ~ "sin_clasificar"
      )
    )
  
  #Se crea la tabla candidatos, que contiene únicamente aquellos que en el test DM
  #han sido clasificados como cercanos al menor RMSFE, con evidencias significativas
  candidatos <- tabla_final_limpia %>%
    filter(candidato_modelo_limpio)

  #Se tiene en  cuenta el caso en el que no existiese ningún candidato, en cuyo 
  #caso, se toma la tabla_final_limpia con el resto de modelos
  if (nrow(candidatos) == 0) {
    candidatos <- tabla_final_limpia
  }

  # Selección jerárquica:
  # 1) Solo candidatos con RMSFE dentro del margen del mejor.
  # 2) Dentro de ellos, se prefiere MAE menor.
  # 3) Luego residuos mejores.
  # 4) Luego AICc menor.
  # 5) Luego RMSFE menor como cierre.
  mejor_modelo_limpio <- candidatos %>%
    arrange( #se ordenan los modelos en fucnión de las siguientes métricas
      #según el orden de aparición:
      MAE,
      prioridad_residuos_modelo_limpio,
      AICc,
      RMSFE,
      BIC
    ) %>%
    slice(1)

  # Regla de sustitución por residuos:
  # Si el modelo seleccionado por MAE no tiene residuos OK, se sustituye
  # solo si existe un candidato con residuos OK y RMSFE similar.
  candidatos_residuos_ok <- candidatos %>%
    filter(residuos_ok_modelo_limpio) #puesto que se ha efectuado esta
  #clasificación antes
  
  #A continuación, es cuando se impone la condición. Cuando exsite algún
  #candidato (RMSFE similar al menor) con residuos de buen comportamiento,
  #se toma la tabla candidatos_residuos_okay automáticamente para tomar
  #aquel modelo que tenga el menor MAE.

  if (
    nrow(candidatos_residuos_ok) > 0 &&
    mejor_modelo_limpio$diagnostico_ljung[[1]] != "residuos_ok"
  ) {
    mejor_modelo_limpio <- candidatos_residuos_ok %>%
      arrange(MAE, AICc, RMSFE, BIC) %>%
      slice(1) %>%
      mutate( #se añade una nota que indica que se ha seleccionado mediante
        #este criterio. 
        criterio_modelo_limpio = paste0(
          criterio_modelo_limpio,
          "_seleccionado_por_residuos_ok_con_RMSFE_similar"
        )
      )
  }
  
  #se añade a la tabla_final_limpia una columna que indica TRUE en el caso de 
  #que el modelo haya sido escogido como mejor modelo. Esto sirve para realizar
  #comparaciones entre modelos en otros escenarios. 

  tabla_final_limpia <- tabla_final_limpia %>%
    mutate(
      seleccionado_modelo_limpio = .model == mejor_modelo_limpio$.model[[1]]
    ) %>%
    arrange( #se ordenan 
      desc(seleccionado_modelo_limpio), #el mejor modelo aparece el primero 
      candidato_modelo_limpio == FALSE, #aseguramos que si hay nulos aparezcan
      #al final
      MAE, #luego por MAE
      prioridad_residuos_modelo_limpio,#luego por residuos_ok
      AICc,
      RMSFE,
      BIC
    )
  
  #A continuación, se presenta la salida de la función 
  #"aplicar_criterio_modelo_limpio". Se accederá a los objetos meidante las 
  #propiedades de un objeto de tipo lista. 
  
  #Tabla final de resultados con todos los modelos. 
  #La tabla con el mejor modelo. 
  #El nombre del mejor modelo.
  #La tabla con el modelo con menor RMSFE.
  #el valor del margen_rmsfe utilizado en la propia entrada de la función. 

  list( 
    tabla_final = tabla_final_limpia,
    mejor_modelo = mejor_modelo_limpio,
    nombre_mejor_modelo = mejor_modelo_limpio$.model[[1]],
    modelo_referencia_menor_rmsfe = modelo_menor_rmsfe,
    margen_rmsfe = margen_rmsfe
  )
}


# ============================================================
# 2. FUNCIÓN PRINCIPAL: SELECCIÓN AUTOMÁTICA SARIMA
# ============================================================

seleccionar_sarima_automatico <- function(train_data,
                                          test_data,
                                          variable = "y_ipc_general",
                                          fecha = "fecha_mensual",
                                          m = 12,
                                          alpha = 0.05,
                                          ljung_lag = 24) {
  
  # ----------------------------------------------------------
  # 1. Preparación básica
  # ----------------------------------------------------------
  
  train_data <- train_data %>%
    arrange(.data[[fecha]])
  
  test_data <- test_data %>%
    arrange(.data[[fecha]])
  
  y_train <- train_data[[variable]]
  
  #Protección contra muestras de entrenamiento demasiado cortas
  
  if (nrow(train_data) <= m + 2) {
    stop("La muestra de entrenamiento es demasiado corta para estimar un SARIMA estacional.")
  }
  
  #Protección contra muestras de test mal generadas 
  
  if (nrow(test_data) == 0) {
    stop("La muestra de test está vacía.")
  }
  
  # ----------------------------------------------------------
  # 2. EDA automático básico
  # ----------------------------------------------------------
  
  #Se grafica la serie
  grafico_serie <- train_data %>% 
    autoplot(.data[[variable]]) +
    labs(
      title = "Serie de entrenamiento",
      x = "Fecha",
      y = variable
    )
  
  #Se grafica una línea mostrando la tendencia
  grafico_tendencia <- train_data %>%
    ggplot(aes(x = .data[[fecha]], y = .data[[variable]])) +
    geom_line() +
    geom_smooth(method = "loess", se = FALSE) +
    labs(
      title = "Evaluación visual de tendencia",
      x = "Fecha",
      y = variable
    )
  
  #Se grafica la función de autocorrelación
  grafico_acf <- train_data %>%
    ACF(.data[[variable]]) %>%
    autoplot() +
    labs(
      title = "ACF de la serie de entrenamiento",
      x = "Rezago",
      y = "Autocorrelación"
    )
  #También la de autocorrelación parcial 
  grafico_pacf <- train_data %>%
    PACF(.data[[variable]]) %>%
    autoplot() +
    labs(
      title = "PACF de la serie de entrenamiento",
      x = "Rezago",
      y = "Autocorrelación parcial"
    )
  
  # ----------------------------------------------------------
  # 3. Diferenciación estacional fija: D = 1
  # ----------------------------------------------------------
  
  
  y_train_D1 <- diff(y_train, lag = m, differences = 1)
  y_train_D1 <- na.omit(y_train_D1)
  
  diagnostico_estacionariedad <- evaluar_estacionariedad(
    y_train_D1,
    alpha = alpha
  )
  
  # ----------------------------------------------------------
  # 4. Decisión automática sobre d
  # ----------------------------------------------------------
  
  decision <- diagnostico_estacionariedad$decision_estacionariedad
  
  d_elegido <- case_when(
    decision == "estacionaria" ~ 0, #si tras la diferenciación con m=12
    #hay estacionariedad, no se vuelve a diferenciar 
    decision == "no_estacionaria" ~ 1, #si no hay estacionariedad, entonces se 
    #diferencia en una unidad. 
    decision == "ambigua" ~ 0
  )
  
  #En caso de que la decisión sea "ambigua", revision_visual tiene valor TRUE, 
  #si no, tiene valor FALSE. 
  
  revision_visual <- if_else(decision == "ambigua", TRUE, FALSE)
  
  # ----------------------------------------------------------
  # 5. Crear todas las combinaciones SARIMA
  # ----------------------------------------------------------
  
  grid_modelos <- crossing( #grid_modelos indica todos los modelos que se van
    #a tener en cuenta. 
    p = 0:2,
    q = 0:2,
    P = 0:1,
    Q = 0:1
  ) %>%
    mutate(
      d = d_elegido,
      D = 1,
      m = m,
      nombre_modelo = paste0(
        "SARIMA_",
        p, d, q, "_",
        P, D, Q
      ),
      n_parametros = p + q + P + Q
    )
  
  # ----------------------------------------------------------
  # 6. Ajuste automático de todos los modelos
  # ----------------------------------------------------------
  
  modelos_sarima_lista <- grid_modelos %>%
    mutate( #se añade una columna para cada modelo 
      ajuste = pmap( #se recorrerán todas las combinaciones de grid_modelos
        list(nombre_modelo, p, d, q, P, D, Q, m), #para estas columnas
        function(nombre_modelo, p, d, q, P, D, Q, m) {
          
          formula_modelo <- as.formula( #se crea la fórmula como una cadena de 
            #caracteres
            paste0(
              variable,
              " ~ 0 + ",
              "pdq(", p, ",", d, ",", q, ") + ",
              "PDQ(", P, ",", D, ",", Q, ", period = ", m, ")"
            )
          )
          
          tryCatch(
            {
              mod <- train_data %>% #se toman los datos de entrenamiento 
                model(modelo_tmp = ARIMA(formula_modelo)) #se entrena cada modelo
              
              names(mod)[names(mod) == "modelo_tmp"] <- nombre_modelo #se asigna
              #el nombre correspondiente a cada modelo
              mod #la función "modelos_sarima_lista" devuelve mod
            },
            error = function(e) { # en caso de error, se creará un valor nulo
              NULL
            }
          )
        }
      )
    )
  
  #A continuación, se muestra el código que elimina aquellos modelos que no 
  #han podido ajustarse (dando lugar a valores nulos)
  
  ajustes_validos <- modelos_sarima_lista %>%
    pull(ajuste) %>%
    compact()
  
  #Protección frente a que todos los modelos sean nulos. En ese caso la función
  #para e informa de que no se ha podido ajustar ningún modelo
  
  if (length(ajustes_validos) == 0) {
    stop("Ningún modelo SARIMA pudo estimarse correctamente para esta ventana.")
  }
  
  #La vble modelos_sarima incluye los objetos de tipo "model" que se han podido 
  #ajustar en uan tabla donde cada columna es un modelo. 
  
  modelos_sarima <- ajustes_validos %>%
    reduce(bind_cols)
  
  # ----------------------------------------------------------
  # 7. Tabla de bondad de ajuste: AICc y BIC
  # ----------------------------------------------------------
  
  tabla_ajuste <- modelos_sarima %>%
    glance() %>% #calcula los criterios de información de cada modelo y lo
    #devuelve en formato de tabla
    select(.model, AICc, BIC) %>% #solo se utilizarán el AICc y el BIC
    filter(
      !is.na(AICc), #fuera nulos 
      !is.na(BIC),
      is.finite(AICc), #fuera infinitos o valores que no existen 
      is.finite(BIC)
    ) %>%
    arrange(AICc) %>% #la tabla se ordena por AICc
    mutate( #se guarda el puesto que ha ocupado cada modelo según su AICc es
      #menor
      ranking_AICc = row_number()
    )
  
  modelos_validos <- tabla_ajuste %>%
    pull(.model) #se crea una lista con los modelos 
  
  #La función para en caso de no detectarse ningún modelo con AICc y BIC válido
  if (length(modelos_validos) == 0) {
    stop("Ningún modelo SARIMA tiene AICc y BIC válidos.")
  }
  
  # ----------------------------------------------------------
  # 8. Test de Ljung-Box para todos los modelos
  # ----------------------------------------------------------
  
  #augment() toma los modelos SARIMA ajustados dentro de modelos_sarima y 
  #devuelve una tabla “expandida” con la información del ajuste para cada 
  #observación temporal usada en el entrenamiento
  
  residuos_modelos <- modelos_sarima %>%
    select(all_of(modelos_validos)) %>%
    augment() 
  
  tabla_ljung <- residuos_modelos %>% #se toman las observaciones
    as_tibble() %>% #se transforman en un objeto de tipo tibble()
    left_join( #se une a grid_modelos
      grid_modelos %>%
        select(nombre_modelo, n_parametros), #que contiene la suma de los parámetros
      by = c(".model" = "nombre_modelo") ##se unen según nombre_modelo
    ) %>%
    group_by(.model, n_parametros) %>% #se agrupan los modelos según n_parametros
    #lo que permite que los cálculos de Ljung-Box más adelante no sean tan repetitivos
    summarise(
      n_residuos_validos = sum(!is.na(.resid)), #se toman los residuos válidos
      ljung_p_value = { #se obtiene el p-valor
        residuos_validos <- na.omit(.resid)
        
        if (length(residuos_validos) > ljung_lag) { #se emplea el lag como 
          #criterio antes de efectuar el test
          Box.test(
            residuos_validos,
            lag = ljung_lag,
            type = "Ljung-Box",
            fitdf = first(n_parametros)
          )$p.value 
        } else {
          NA_real_
        }
      },
      .groups = "drop"
    ) %>%
    mutate(
      diagnostico_ljung = case_when(
        is.na(ljung_p_value) ~ "no_calculable",
        ljung_p_value < alpha ~ "autocorrelacion_residual",
        ljung_p_value >= alpha ~ "residuos_ok"
      )
    )
  
  # ----------------------------------------------------------
  # 9. Forecasts para todos los modelos
  # ----------------------------------------------------------
  
  #Se crea una lista que contiene los modelos ordenados según menor AICc:
  
  modelos_ordenados_aicc <- tabla_ajuste %>%
    arrange(AICc) %>%
    pull(.model)
  
  #Variable que aplica la función forecast para cada modelo de modelos_sarima
  #y los filtra según el orden de la lista modelos_ordenados_aicc. se predecirá
  #un valor para cada uno de los datos de test_data. 
  
  forecasts_sarima <- modelos_sarima %>%
    select(all_of(modelos_ordenados_aicc)) %>%
    forecast(new_data = test_data)
  
  # ----------------------------------------------------------
  # 10. Métricas predictivas robustas
  # ----------------------------------------------------------
  
  
  #Se crea una tabla que permite contrastar los valores de las predicciones con
  #los valores reales, y se añaden: 
  
  # valor del error relativo 
  # valor del error absoluto
  # valor del error cuadrático 
  
  predicciones_vs_real <- forecasts_sarima %>%
    as_tibble() %>%
    select(
      all_of(fecha),
      .model,
      prediccion = .mean
    ) %>%
    left_join(
      test_data %>%
        as_tibble() %>%
        transmute(
          !!fecha := .data[[fecha]],
          real = .data[[variable]]
        ),
      by = fecha
    ) %>%
    filter(
      !is.na(real),
      !is.na(prediccion),
      is.finite(real),
      is.finite(prediccion)
    ) %>%
    mutate(
      error = real - prediccion,
      abs_error = abs(error),
      sq_error = error^2
    )
  
  if (nrow(predicciones_vs_real) == 0) {
    stop("No se han obtenido predicciones válidas contra valores reales en test_data.")
  }
  
  y_train_num <- as.numeric(train_data[[variable]])
  
  #A continuación, se calculan las métricas RMSFE, MAE Y MASE, que se añadirán
  #a la tabla predicciones_vs_real, creando la nueva tabla "tabla_metricas"
  
  
  denominador_mase <- mean(
    abs(y_train_num[(m + 1):length(y_train_num)] - y_train_num[1:(length(y_train_num) - m)]),
    na.rm = TRUE
  )
  
  tabla_metricas <- predicciones_vs_real %>%
    group_by(.model) %>%
    summarise(
      RMSFE = sqrt(mean(sq_error, na.rm = TRUE)),
      MAE = mean(abs_error, na.rm = TRUE),
      MASE = MAE / denominador_mase,
      .groups = "drop"
    ) %>%
    filter(
      !is.na(RMSFE),
      !is.na(MAE),
      is.finite(RMSFE),
      is.finite(MAE)
    ) %>%
    arrange(RMSFE, MAE, MASE) %>% #tabla_metricas se ordena según RMSFE, 
    #en caso de empate, según MAE y en caso de empate de las dos anteriores, 
    #según MASE. 
    mutate(
      #Se añaden variables que guardan el puesto que coupa cada modelo en 
      #el ranking de cada métrica. 
      ranking_RMSFE = row_number(),
      ranking_MAE = rank(MAE, ties.method = "first"),
      ranking_MASE = rank(MASE, ties.method = "first")
    )
  
  # ----------------------------------------------------------
  # 11. Tabla conjunta: ajuste + residuos + predicción
  # ----------------------------------------------------------
  
  #Es la tabla que ya contiene la información de cada modelo. Es aquella que
  #utiliza la función seleccion_modelo_limpio implementada anteriormente
  
  
  tabla_final <- tabla_ajuste %>%
    left_join(tabla_ljung, by = ".model") %>%
    inner_join(tabla_metricas, by = ".model") %>%
    arrange(ranking_RMSFE, ranking_MAE, ranking_AICc, BIC)

  if (nrow(tabla_final) == 0) {
    stop("No se ha podido construir tabla_final: ningún modelo tiene métricas predictivas válidas.")
  }

  # ----------------------------------------------------------
  # 12. Selección automática del mejor modelo: modelo_limpio
  # ----------------------------------------------------------

  seleccion_modelo_limpio <- aplicar_criterio_modelo_limpio(
    tabla_final = tabla_final,
    predicciones_vs_real = predicciones_vs_real,
    alpha = alpha,
    margen_rmsfe = 0.10,
    min_obs_dm = 10
  )
  
  #Se renombra la variable tabla_final según lo que devuelve la función 
  #seleccion_modelo_limpio. En particular, se toma de la salida de la función: 

  tabla_final <- seleccion_modelo_limpio$tabla_final
  mejor_modelo <- seleccion_modelo_limpio$mejor_modelo
  nombre_mejor_modelo <- seleccion_modelo_limpio$nombre_mejor_modelo
  modelo_limpio <- mejor_modelo

  if (nrow(mejor_modelo) == 0) {
    stop("No se ha seleccionado ningún modelo final. Revisa tabla_final y tabla_metricas.")
  }

  # ----------------------------------------------------------
  # 13. Tabla de predicción vs valor real del mejor modelo
  # ----------------------------------------------------------
  
  #Los gráficos solo se efectúan para el mejor modelo
  comparacion_pred_real <- predicciones_vs_real %>%
    filter(.model == nombre_mejor_modelo)
  
  top3_modelos_limpios <- tabla_final %>%
    slice_head(n = 3) %>%
    pull(.model)
  
  predicciones_top3_modelos_limpios <- predicciones_vs_real %>%
    filter(.model %in% top3_modelos_limpios)
  
  if (nrow(comparacion_pred_real) == 0) {
    stop("No hay predicciones válidas para el modelo seleccionado.")
  }
  
  grafico_pred_real <- comparacion_pred_real %>% 
    ggplot(aes(x = .data[[fecha]])) +
    geom_line(aes(y = real, colour = "Valor real"), linewidth = 1) +
    geom_line(aes(y = prediccion, colour = "Predicción"), linewidth = 1, linetype = "dashed") +
    labs(
      title = paste("Valores reales vs predicciones -", nombre_mejor_modelo),
      x = "Fecha",
      y = variable,
      colour = ""
    )
  
  grafico_error <- comparacion_pred_real %>%
    ggplot(aes(x = .data[[fecha]], y = error)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line() +
    geom_point() +
    labs(
      title = paste("Error de predicción -", nombre_mejor_modelo),
      x = "Fecha",
      y = "Error = real - predicción"
    )
  
  # ----------------------------------------------------------
  # 14. Resumen en pantalla y salida final
  # ----------------------------------------------------------
  
  cat("\n")
  cat("============================================================\n")
  cat("RESUMEN DEL PIPELINE SARIMA AUTOMÁTICO\n")
  cat("============================================================\n\n")
  
  cat("Variable analizada:", variable, "\n")
  cat("Frecuencia estacional m:", m, "\n")
  cat("Fechas train:", as.character(min(train_data[[fecha]])), "a", as.character(max(train_data[[fecha]])), "\n")
  cat("Fechas test:", as.character(min(test_data[[fecha]])), "a", as.character(max(test_data[[fecha]])), "\n")
  cat("Observaciones train:", nrow(train_data), "\n")
  cat("Observaciones test:", nrow(test_data), "\n")
  cat("Diferenciación estacional fijada D:", 1, "\n")
  cat("Diferenciación regular elegida d:", d_elegido, "\n\n")
  
  cat("------------------------------------------------------------\n")
  cat("1. Diagnóstico de estacionariedad tras aplicar D = 1\n")
  cat("------------------------------------------------------------\n")
  print(diagnostico_estacionariedad)
  
  if (revision_visual) {
    cat("\nResultado ambiguo: se recomienda revisar visualmente la serie en esta ventana.\n")
  } else {
    cat("\nResultado no ambiguo: la decisión sobre d se ha tomado automáticamente.\n")
  }
  
  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("2. Número de modelos SARIMA considerados\n")
  cat("------------------------------------------------------------\n")
  cat("Modelos teóricos en la parrilla:", nrow(grid_modelos), "\n")
  cat("Modelos estimados correctamente:", length(modelos_validos), "\n\n")
  
  cat("------------------------------------------------------------\n")
  cat("3. Mejores modelos por bondad de ajuste, menor AICc\n")
  cat("------------------------------------------------------------\n")
  print(
    tabla_ajuste %>%
      select(.model, AICc, BIC, ranking_AICc) %>%
      slice_head(n = 10)
  )
  
  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("4. Diagnóstico Ljung-Box de residuos\n")
  cat("------------------------------------------------------------\n")
  print(
    tabla_ljung %>%
      arrange(desc(ljung_p_value)) %>%
      select(.model, n_residuos_validos, ljung_p_value, diagnostico_ljung) %>%
      slice_head(n = 10)
  )
  
  cat("\nResumen Ljung-Box:\n")
  print(
    tabla_ljung %>%
      count(diagnostico_ljung)
  )
  
  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("5. Mejores modelos por precisión predictiva\n")
  cat("------------------------------------------------------------\n")
  print(
    tabla_metricas %>%
      select(.model, RMSFE, MAE, MASE, ranking_RMSFE, ranking_MAE, ranking_MASE) %>%
      slice_head(n = 10)
  )
  
  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("6. Ranking final con criterio modelo_limpio\n")
  cat("------------------------------------------------------------\n")
  cat("Criterio aplicado:\n")
  cat("  1) RMSFE define el conjunto de candidatos dentro del 10% del menor RMSFE.\n")
  cat("  2) MAE decide entre candidatos con RMSFE similar.\n")
  cat("  3) Ljung-Box valida residuos; solo sustituye si hay RMSFE similar.\n")
  cat("  4) AICc se usa como desempate secundario.\n")
  cat("  5) Diebold-Mariano se informa solo si hay suficientes errores.\n\n")
  print(
    tabla_final %>%
      select(
        .model,
        AICc,
        BIC,
        RMSFE,
        MAE,
        MASE,
        ljung_p_value,
        diagnostico_ljung,
        ranking_AICc,
        ranking_RMSFE,
        ranking_MAE,
        candidato_modelo_limpio,
        diferencia_relativa_RMSFE,
        dm_p_value_vs_mejor_rmsfe,
        diagnostico_dm,
        seleccionado_modelo_limpio
      ) %>%
      slice_head(n = 10)
  )
  
  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("7. Modelo seleccionado automáticamente\n")
  cat("------------------------------------------------------------\n")
  print(mejor_modelo)
  
  cat("\n")
  cat("Modelo final seleccionado:", nombre_mejor_modelo, "\n")
  
  if (mejor_modelo$diagnostico_ljung == "residuos_ok") {
    cat("Diagnóstico de residuos: aceptable según Ljung-Box.\n")
  } else if (mejor_modelo$diagnostico_ljung == "autocorrelacion_residual") {
    cat("Diagnóstico de residuos: posible autocorrelación residual.\n")
  } else {
    cat("Diagnóstico de residuos: Ljung-Box no calculable.\n")
  }
  
  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("8. Top 3 modelos según criterio modelo_limpio\n")
  cat("------------------------------------------------------------\n")
  print(
    tabla_final %>%
      filter(.model %in% top3_modelos_limpios) %>%
      select(.model, RMSFE, MAE, MASE, diagnostico_ljung, seleccionado_modelo_limpio) %>%
      slice_head(n = 3)
  )
  
  cat("\n")
  cat("============================================================\n")
  cat("FIN DEL RESUMEN\n")
  cat("============================================================\n\n")
  
  
  #Esta es la salida de la función global. Se trata de un objeto de tipo lista.
  
  
  list(
    diagnostico_estacionariedad = diagnostico_estacionariedad,
    d_elegido = d_elegido,
    D_elegido = 1,
    revision_visual = revision_visual,
    grid_modelos = grid_modelos,
    modelos_sarima = modelos_sarima,
    tabla_ajuste = tabla_ajuste,
    tabla_ljung = tabla_ljung,
    forecasts_sarima = forecasts_sarima,
    predicciones_vs_real = predicciones_vs_real,
    tabla_metricas = tabla_metricas,
    tabla_final = tabla_final,
    mejor_modelo = mejor_modelo,
    modelo_limpio = modelo_limpio,
    seleccion_modelo_limpio = seleccion_modelo_limpio,
    nombre_mejor_modelo = nombre_mejor_modelo,
    comparacion_pred_real = comparacion_pred_real,
    top3_modelos_limpios = top3_modelos_limpios,
    predicciones_top3_modelos_limpios = predicciones_top3_modelos_limpios,
    graficos_eda = list(
      serie = grafico_serie,
      tendencia = grafico_tendencia,
      acf = grafico_acf,
      pacf = grafico_pacf
    ),
    grafico_pred_real = grafico_pred_real,
    grafico_error = grafico_error
  )
}


# ============================================================
# 3. FUNCIÓN NUEVA: CREAR TRAIN/TEST DE 6 MESES POR EVENTO
# ============================================================

crear_train_test_evento <- function(datos_completos,
                                    fecha_corte,
                                    fecha = "fecha_mensual",
                                    horizonte = 6,
                                    incluir_mes_corte_en_train = FALSE) {
  
  #Se asegura el orden de los datos
  datos_completos <- datos_completos %>%
    arrange(.data[[fecha]])
  
  #Se toma el argumento introducido como fecha de corte en la función 
  fecha_corte <- yearmonth(fecha_corte)
  
  #Se crea un condicional para poder escoger en los argumentos de la función, 
  #en específico, el argumento "incluir_mes_corte_en_train"
  
  if (incluir_mes_corte_en_train) {
    train_evento <- datos_completos %>%
      filter(.data[[fecha]] <= fecha_corte)
    
    fecha_inicio_test <- fecha_corte + 1
    fecha_fin_test <- fecha_corte + horizonte
  } else {
    train_evento <- datos_completos %>%
      filter(.data[[fecha]] < fecha_corte)
    
    fecha_inicio_test <- fecha_corte + 1
    fecha_fin_test <- fecha_corte + horizonte
  }
  
  test_evento <- datos_completos %>%
    filter(
      .data[[fecha]] >= fecha_inicio_test,
      .data[[fecha]] <= fecha_fin_test
    )
  
  #Protección contra un horizonte más largo que la posibilidad de datos de test:
  if (nrow(test_evento) < horizonte) {
    warning(
      paste0(
        "La ventana de test para ", as.character(fecha_corte),
        " tiene ", nrow(test_evento),
        " observaciones, aunque se esperaban ", horizonte, "."
      )
    )
  }
  
  #La función devuelve las siguientes vbles en una lista
  list(
    fecha_corte = fecha_corte,
    fecha_inicio_test = fecha_inicio_test,
    fecha_fin_test = fecha_fin_test,
    train_evento = train_evento,
    test_evento = test_evento
  )
}


# ============================================================
# 4. FUNCIÓN NUEVA: SELECCIÓN SARIMA PARA UN EVENTO
# ============================================================

#Esta función ejecuta la función "seleccionar_sarima_automatico" para una fecha
#de ruptura específica, para lo que se necesitan datos de entrenamiento y test
#que se generan mediante la función "crear_train_test_evento" 

seleccionar_sarima_evento_6m <- function(datos_completos,
                                         fecha_corte,
                                         variable = "y_ipc_general",
                                         fecha = "fecha_mensual",
                                         m = 12,
                                         alpha = 0.05,
                                         ljung_lag = 24,
                                         horizonte = 6,
                                         incluir_mes_corte_en_train = FALSE,
                                         nombre_evento = NULL) {
  
  particion <- crear_train_test_evento(
    datos_completos = datos_completos,
    fecha_corte = fecha_corte,
    fecha = fecha,
    horizonte = horizonte,
    incluir_mes_corte_en_train = incluir_mes_corte_en_train
  )
  
  cat("\n")
  cat("############################################################\n")
  cat("EVENTO:", ifelse(is.null(nombre_evento), as.character(particion$fecha_corte), nombre_evento), "\n")
  cat("Fecha de corte:", as.character(particion$fecha_corte), "\n")
  cat("Train:", as.character(min(particion$train_evento[[fecha]])), "a", as.character(max(particion$train_evento[[fecha]])), "\n")
  cat("Test:", as.character(min(particion$test_evento[[fecha]])), "a", as.character(max(particion$test_evento[[fecha]])), "\n")
  cat("############################################################\n")
  
  resultado <- seleccionar_sarima_automatico(
    train_data = particion$train_evento,
    test_data = particion$test_evento,
    variable = variable,
    fecha = fecha,
    m = m,
    alpha = alpha,
    ljung_lag = ljung_lag
  )
  
  resultado$nombre_evento <- nombre_evento
  resultado$fecha_corte <- particion$fecha_corte
  resultado$fecha_inicio_test <- particion$fecha_inicio_test
  resultado$fecha_fin_test <- particion$fecha_fin_test
  resultado$train_evento <- particion$train_evento
  resultado$test_evento <- particion$test_evento
  
  resultado #Se devuelve la lista con toda la información del modelo
  #seleccionado como el mejor de entre los modelos de tipo SARIMA. 
}


# ============================================================
# 5. FUNCIÓN NUEVA: EVALUAR VARIOS EVENTOS AUTOMÁTICAMENTE
# ============================================================

evaluar_eventos_sarima_6m <- function(datos_completos,
                                      tabla_eventos,
                                      variable = "y_ipc_general",
                                      fecha = "fecha_mensual",
                                      m = 12,
                                      alpha = 0.05,
                                      ljung_lag = 24,
                                      horizonte = 6,
                                      incluir_mes_corte_en_train = FALSE) {
  
  #Se recorre tabla_eventos fila a fila y, para cada combinación de evento y 
  #fecha_corte, se ejecuta seleccionar_sarima_evento_6m() con esos datos y los 
  #parámetros generales de la función.
  
  resultados_eventos <- tabla_eventos %>%
    mutate(
      resultado = pmap(
        list(evento, fecha_corte),
        function(evento, fecha_corte) {
          seleccionar_sarima_evento_6m(
            datos_completos = datos_completos,
            fecha_corte = fecha_corte,
            variable = variable,
            fecha = fecha,
            m = m,
            alpha = alpha,
            ljung_lag = ljung_lag,
            horizonte = horizonte,
            incluir_mes_corte_en_train = incluir_mes_corte_en_train,
            nombre_evento = evento
          )
        }
      )
    )
  
  tabla_resumen <- resultados_eventos %>%
    transmute( #se queda únicamente con las vbles de a continuación
      evento,
      fecha_corte,
      fecha_inicio_test = map_chr(resultado, ~ as.character(.x$fecha_inicio_test)),
      fecha_fin_test = map_chr(resultado, ~ as.character(.x$fecha_fin_test)),
      n_train = map_int(resultado, ~ nrow(.x$train_evento)),
      n_test = map_int(resultado, ~ nrow(.x$test_evento)),
      nombre_mejor_modelo = map_chr(resultado, ~ .x$nombre_mejor_modelo),
      mejor_modelo = map(resultado, ~ .x$mejor_modelo)
    ) %>%
    unnest(mejor_modelo) #desempaqueta la columna-lista mejor_modelo: si cada 
  #celda contiene una tabla con los datos del modelo ganador, sus columnas se 
  #expanden y pasan a formar parte de tabla_resumen
  
  #Salida de la función: 
  
  list(
    resultados_eventos = resultados_eventos,
    tabla_resumen = tabla_resumen
  )
}


# ============================================================


# ============================================================
# 6. EXTENSIÓN 9/10: VALIDACIÓN ROLLING EX ANTE + TEST LIMPIO
# ============================================================
# Esta sección mantiene intacto el motor original:
#   - seleccionar_sarima_automatico()
#   - crear_train_test_evento()
#
# La mejora metodológica consiste en cambiar el orden del procedimiento:
#   1. Se crea el train/test del evento.
#   2. Dentro del train del evento se crean varias ventanas rolling de validación.
#   3. En cada ventana se evalúa la parrilla SARIMA original.
#   4. Se selecciona un modelo ex ante por rendimiento medio en validación rolling.
#   5. Se reestima/evalúa ese modelo con todo el train del evento y el test limpio.
#   6. Se calcula además el mejor modelo ex post del evento, solo como referencia 
#      retrospectiva.
# ============================================================

crear_ventanas_rolling_validacion_sarima <- function(train_evento,
                                                     fecha = "fecha_mensual",
                                                     horizonte_validacion = 6,
                                                     min_train_validacion = 72,
                                                     paso_validacion = 6,
                                                     max_ventanas_validacion = 8) {
  #son todos los datos disponibles que son snteriores al corte del evento
  train_evento <- train_evento %>%
    arrange(.data[[fecha]]) #además, se ordenan por fecha
  
  
  n_total <- nrow(train_evento)
  ultimo_fin_train_posible <- n_total - horizonte_validacion
  
  if (ultimo_fin_train_posible < min_train_validacion) {
    stop( #puesto que no se tendrán suficientes observaciones para tener en 
      #consideración un conjunto de validación
      paste0(
        "No hay observaciones suficientes para validación rolling. ",
        "n_total = ", n_total,
        ", min_train_validacion = ", min_train_validacion,
        ", horizonte_validacion = ", horizonte_validacion, "."
      )
    )
  }
  
  #Se crean varios conjuntos de validación de 6 observaciones cada uno. Estos
  #quedan caracterizados por fines_train. 
  
  fines_train <- seq(
    from = min_train_validacion,
    to = ultimo_fin_train_posible,
    by = paso_validacion
  )
  
  
  #Aún así, en los argumentos de la función se puede fijar un máximo de ventanas
  #de validación. Por defecto se emplearán 8 ventanas de validación. 
  if (length(fines_train) > max_ventanas_validacion) {
    fines_train <- tail(fines_train, max_ventanas_validacion)
  }
  
  ventanas <- map_dfr(
    seq_along(fines_train),
    function(i) {
      fin_train <- fines_train[[i]]
      inicio_validacion <- fin_train + 1
      fin_validacion <- fin_train + horizonte_validacion
      
      #Se crea un registro en una tabla para cada elemento de fines_train. Es
      #decir, un registro por cada ventana. La tabla se llama "ventanas"
      
      tibble( 
        id_ventana = paste0("VAL", sprintf("%02d", i)),
        fila_fin_train = fin_train,
        fila_inicio_validacion = inicio_validacion,
        fila_fin_validacion = fin_validacion,
        fecha_fin_train = train_evento[[fecha]][[fin_train]],
        fecha_inicio_validacion = train_evento[[fecha]][[inicio_validacion]],
        fecha_fin_validacion = train_evento[[fecha]][[fin_validacion]],
        n_train = fin_train,
        n_validacion = horizonte_validacion
      )
    }
  )
  
  #"ventanas" es la salida de la función
  
  ventanas
}


#La siguiente función recibe una tabla con todas las ventanas de validación
#que se han creado para cada modelo, y obtiene la media de sus métricas,
#criterios y más información para cada modelo. Efectuándose de esta manera una 
#selección del modelo antes de observar cómo predice fuera de muestra. 

agregar_validacion_rolling_sarima <- function(tablas_validacion,
                                              margen_rmsfe = 0.10) {
  
  if (nrow(tablas_validacion) == 0) {
    stop("No hay resultados válidos de validación rolling.")
  }
  
  
  
  tabla_validacion_modelos <- tablas_validacion %>%
    group_by(.model) %>%
    summarise(
      n_ventanas_validacion = n_distinct(id_ventana),
      RMSFE_validacion_medio = mean(RMSFE, na.rm = TRUE),
      MAE_validacion_medio = mean(MAE, na.rm = TRUE),
      MASE_validacion_medio = mean(MASE, na.rm = TRUE),
      AICc_validacion_medio = mean(AICc, na.rm = TRUE),
      BIC_validacion_medio = mean(BIC, na.rm = TRUE),
      sd_RMSFE_validacion = sd(RMSFE, na.rm = TRUE),
      prop_residuos_ok_validacion = mean(diagnostico_ljung == "residuos_ok", na.rm = TRUE),
      prop_top3_RMSFE_validacion = mean(ranking_RMSFE <= 3, na.rm = TRUE),
      p = first(p),
      d = first(d),
      q = first(q),
      P = first(P),
      D = first(D),
      Q = first(Q),
      .groups = "drop"
    ) %>%
    filter(
      !is.na(RMSFE_validacion_medio),
      !is.na(MAE_validacion_medio),
      is.finite(RMSFE_validacion_medio),
      is.finite(MAE_validacion_medio)
    )
  
  if (nrow(tabla_validacion_modelos) == 0) {
    stop("La validación rolling no produjo métricas agregadas válidas.")
  }
  
  rmsfe_minimo <- min(tabla_validacion_modelos$RMSFE_validacion_medio, na.rm = TRUE)
  limite_rmsfe <- rmsfe_minimo * (1 + margen_rmsfe)
  
  tabla_validacion_modelos <- tabla_validacion_modelos %>%
    mutate(
      rmsfe_minimo_validacion = rmsfe_minimo,
      margen_rmsfe_validacion = margen_rmsfe,
      diferencia_relativa_RMSFE_validacion = (RMSFE_validacion_medio / rmsfe_minimo) - 1,
      candidato_ex_ante = RMSFE_validacion_medio <= limite_rmsfe,
      prop_residuos_ok_validacion = replace_na(prop_residuos_ok_validacion, 0),
      sd_RMSFE_validacion = replace_na(sd_RMSFE_validacion, 0)
    ) %>%
    arrange( #se ordenan los modelos según el mismo criterio que se ha utilizado
      #para la selección del modelo ex-post. De manera que se asegura que solo
      #aquellos que presentan un RMSFE similar al del menor RMSFE aparecen más
      #arriba, y que luego se prioriza el MAE, que los residuos tengan un buen 
      #comportamiento... 
      desc(candidato_ex_ante),
      MAE_validacion_medio,
      desc(prop_residuos_ok_validacion),
      sd_RMSFE_validacion,
      AICc_validacion_medio,
      RMSFE_validacion_medio,
      BIC_validacion_medio
    )
  
  mejor_modelo_ex_ante <- tabla_validacion_modelos %>%
    filter(candidato_ex_ante) %>%
    arrange(
      MAE_validacion_medio,
      desc(prop_residuos_ok_validacion),
      sd_RMSFE_validacion,
      AICc_validacion_medio,
      RMSFE_validacion_medio,
      BIC_validacion_medio
    ) %>%
    slice(1)
  
  if (nrow(mejor_modelo_ex_ante) == 0) {
    mejor_modelo_ex_ante <- tabla_validacion_modelos %>%
      slice(1)
  }
  
  nombre_mejor_modelo_ex_ante <- mejor_modelo_ex_ante$.model[[1]]
  
  tabla_validacion_modelos <- tabla_validacion_modelos %>%
    mutate(seleccionado_ex_ante = .model == nombre_mejor_modelo_ex_ante) %>%
    arrange(desc(seleccionado_ex_ante), MAE_validacion_medio, RMSFE_validacion_medio)
  
  #La salida de la función, de igual forma que en las funciones anteriores, devuelve
  #una lista con la información relevante. 
  
  list( 
    tabla_validacion_modelos = tabla_validacion_modelos,
    mejor_modelo_ex_ante_validacion = mejor_modelo_ex_ante,
    nombre_mejor_modelo_ex_ante = nombre_mejor_modelo_ex_ante
  )
}

#Esta función utiliza las dos funciones anteriores. Es decir, unifica el 
#proceso que consiste en: 

#1Crea ventanas de validación.
#2Recorre cada ventana.
#3Entrena y evalúa SARIMA en cada una.
#4Guarda las métricas.
#5Agrega resultados.
#6Selecciona el mejor modelo ex ante.
#7Devuelve todos los objetos relevantes.

validar_rolling_sarima_9_10 <- function(train_evento,
                                        variable = "y_ipc_general",
                                        fecha = "fecha_mensual",
                                        m = 12,
                                        alpha = 0.05,
                                        ljung_lag = 24,
                                        horizonte_validacion = 6,
                                        min_train_validacion = 72,
                                        paso_validacion = 6,
                                        max_ventanas_validacion = 8,
                                        margen_rmsfe = 0.10) {
  
  ventanas <- crear_ventanas_rolling_validacion_sarima(
    train_evento = train_evento,
    fecha = fecha,
    horizonte_validacion = horizonte_validacion,
    min_train_validacion = min_train_validacion,
    paso_validacion = paso_validacion,
    max_ventanas_validacion = max_ventanas_validacion
  )
  
  resultados_ventanas <- vector("list", nrow(ventanas))
  tablas_validacion <- vector("list", nrow(ventanas))
  errores_ventanas <- list()
  
  for (i in seq_len(nrow(ventanas))) {
    ventana_i <- ventanas[i, ]
    
    train_i <- train_evento %>%
      slice(1:ventana_i$fila_fin_train[[1]])
    
    validacion_i <- train_evento %>%
      slice(ventana_i$fila_inicio_validacion[[1]]:ventana_i$fila_fin_validacion[[1]])
    
    resultado_i <- tryCatch(
      {
        seleccionar_sarima_automatico(
          train_data = train_i,
          test_data = validacion_i,
          variable = variable,
          fecha = fecha,
          m = m,
          alpha = alpha,
          ljung_lag = ljung_lag
        )
      },
      error = function(e) {
        errores_ventanas[[length(errores_ventanas) + 1]] <<- tibble(
          id_ventana = ventana_i$id_ventana[[1]],
          mensaje_error = conditionMessage(e)
        )
        NULL
      }
    )
    
    resultados_ventanas[[i]] <- resultado_i
    
    if (!is.null(resultado_i) && !is.null(resultado_i$tabla_final) && nrow(resultado_i$tabla_final) > 0) {
      tablas_validacion[[i]] <- resultado_i$tabla_final %>%
        left_join(
          resultado_i$grid_modelos %>%
            select(
              .model = nombre_modelo,
              p, d, q, P, D, Q
            ),
          by = ".model"
        ) %>%
        mutate(
          id_ventana = ventana_i$id_ventana[[1]],
          fecha_fin_train = ventana_i$fecha_fin_train[[1]],
          fecha_inicio_validacion = ventana_i$fecha_inicio_validacion[[1]],
          fecha_fin_validacion = ventana_i$fecha_fin_validacion[[1]]
        )
    }
  }
  
  tablas_validacion <- compact(tablas_validacion)
  
  if (length(tablas_validacion) == 0) {
    stop("Todas las ventanas de validación rolling fallaron.")
  }
  
  tabla_validacion_completa <- bind_rows(tablas_validacion)
  agregacion <- agregar_validacion_rolling_sarima(
    tablas_validacion = tabla_validacion_completa,
    margen_rmsfe = margen_rmsfe
  )
  
  tabla_errores_validacion <- if (length(errores_ventanas) == 0) {
    tibble()
  } else {
    bind_rows(errores_ventanas)
  }
  
  list(
    ventanas_validacion_rolling = ventanas,
    resultados_ventanas = resultados_ventanas,
    tabla_validacion_completa = tabla_validacion_completa,
    tabla_validacion_modelos = agregacion$tabla_validacion_modelos,
    mejor_modelo_ex_ante_validacion = agregacion$mejor_modelo_ex_ante_validacion,
    nombre_mejor_modelo_ex_ante = agregacion$nombre_mejor_modelo_ex_ante,
    tabla_errores_validacion = tabla_errores_validacion
  )
}


crear_graficos_modelo_sarima_9_10 <- function(comparacion_pred_real,
                                              nombre_modelo,
                                              variable = "y_ipc_general",
                                              fecha = "fecha_mensual",
                                              titulo_base = "SARIMA") {
  
  grafico_pred_real <- comparacion_pred_real %>%
    ggplot(aes(x = .data[[fecha]])) +
    geom_line(aes(y = real, colour = "Valor real"), linewidth = 1) +
    geom_line(aes(y = prediccion, colour = "Predicción"), linewidth = 1, linetype = "dashed") +
    labs(
      title = paste(titulo_base, "- valores reales vs predicciones -", nombre_modelo),
      x = "Fecha",
      y = variable,
      colour = ""
    )
  
  grafico_error <- comparacion_pred_real %>%
    ggplot(aes(x = .data[[fecha]], y = error)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line() +
    geom_point() +
    labs(
      title = paste(titulo_base, "- error de predicción -", nombre_modelo),
      x = "Fecha",
      y = "Error = real - predicción"
    )
  
  list(
    grafico_pred_real = grafico_pred_real,
    grafico_error = grafico_error
  )
}

#FUNCIÓN IMPORTANTE. Unifica las anteriores funciones, efectuando una selección
#ex ante y una selección ex post, y devolviendo los resultados relevantes. 
#Primero parte la serie en entrenamiento y test alrededor de un evento; después 
#usa validación rolling dentro del entrenamiento para elegir un modelo SARIMA 
#sin mirar el test; finalmente evalúa ese modelo en el test real del evento y, 
#además, calcula cuál habría sido el mejor modelo ex post.

seleccionar_sarima_evento_6m_9_10 <- function(datos_completos,
                                             fecha_corte,
                                             variable = "y_ipc_general",
                                             fecha = "fecha_mensual",
                                             m = 12,
                                             alpha = 0.05,
                                             ljung_lag = 24,
                                             horizonte = 6,
                                             incluir_mes_corte_en_train = FALSE,
                                             nombre_evento = NULL,
                                             horizonte_validacion = 6,
                                             min_train_validacion = 72,
                                             paso_validacion = 6,
                                             max_ventanas_validacion = 8,
                                             margen_rmsfe_validacion = 0.10) {
  
  particion <- crear_train_test_evento(
    datos_completos = datos_completos,
    fecha_corte = fecha_corte,
    fecha = fecha,
    horizonte = horizonte,
    incluir_mes_corte_en_train = incluir_mes_corte_en_train
  )
  
  cat("\n")
  cat("############################################################\n")
  cat("SARIMA 9/10 - EVENTO:", ifelse(is.null(nombre_evento), as.character(particion$fecha_corte), nombre_evento), "\n")
  cat("Fecha de corte:", as.character(particion$fecha_corte), "\n")
  cat("Train del evento:", as.character(min(particion$train_evento[[fecha]])), "a", as.character(max(particion$train_evento[[fecha]])), "\n")
  cat("Test limpio:", as.character(min(particion$test_evento[[fecha]])), "a", as.character(max(particion$test_evento[[fecha]])), "\n")
  cat("############################################################\n")
  
  # ----------------------------------------------------------
  # A. Selección ex ante: solo usa datos anteriores al evento.
  # ----------------------------------------------------------
  validacion_rolling <- validar_rolling_sarima_9_10(
    train_evento = particion$train_evento,
    variable = variable,
    fecha = fecha,
    m = m,
    alpha = alpha,
    ljung_lag = ljung_lag,
    horizonte_validacion = horizonte_validacion,
    min_train_validacion = min_train_validacion,
    paso_validacion = paso_validacion,
    max_ventanas_validacion = max_ventanas_validacion,
    margen_rmsfe = margen_rmsfe_validacion
  )
  
  nombre_mejor_modelo_ex_ante <- validacion_rolling$nombre_mejor_modelo_ex_ante
  nombres_top3_modelos_ex_ante <- validacion_rolling$tabla_validacion_modelos %>%
    slice_head(n = 3) %>%
    pull(.model)
  
  # ----------------------------------------------------------
  # B. Evaluación final en el test del evento.
  #    Se estiman todos los modelos para obtener la fila del
  #    modelo ex ante y, separadamente, el mejor ex post.
  # ----------------------------------------------------------
  resultado_test <- seleccionar_sarima_automatico(
    train_data = particion$train_evento,
    test_data = particion$test_evento,
    variable = variable,
    fecha = fecha,
    m = m,
    alpha = alpha,
    ljung_lag = ljung_lag
  )
  
  fila_ex_ante_test <- resultado_test$tabla_final %>%
    filter(.model == nombre_mejor_modelo_ex_ante)
  
  if (nrow(fila_ex_ante_test) == 0) {
    warning(
      paste0(
        "El modelo seleccionado ex ante ('", nombre_mejor_modelo_ex_ante,
        "') no aparece en la parrilla final del test. Se seleccionará el modelo disponible más parecido por especificación si es posible."
      )
    )
    fila_ex_ante_test <- resultado_test$tabla_final %>%
      slice(1)
    nombre_mejor_modelo_ex_ante <- fila_ex_ante_test$.model[[1]]
  }
  
  comparacion_ex_ante <- resultado_test$predicciones_vs_real %>%
    filter(.model == nombre_mejor_modelo_ex_ante)
  comparacion_top3_ex_ante <- resultado_test$predicciones_vs_real %>%
    filter(.model %in% nombres_top3_modelos_ex_ante)
  
  # ----------------------------------------------------------
  # Tabla completa de métricas de test, pero ordenada según
  # el criterio de selección ex ante.
  # ----------------------------------------------------------
  
  tabla_orden_ex_ante <- validacion_rolling$tabla_validacion_modelos %>%
    mutate(
      orden_ex_ante = row_number()
    ) %>%
    select(
      .model,
      orden_ex_ante,
      seleccionado_ex_ante,
      candidato_ex_ante,
      n_ventanas_validacion,
      RMSFE_validacion_medio,
      MAE_validacion_medio,
      MASE_validacion_medio,
      AICc_validacion_medio,
      BIC_validacion_medio,
      sd_RMSFE_validacion,
      prop_residuos_ok_validacion,
      diferencia_relativa_RMSFE_validacion,
      p, d, q, P, D, Q
    )
  
  tabla_final_ex_ante_test <- resultado_test$tabla_final %>%
    left_join(
      tabla_orden_ex_ante,
      by = ".model"
    ) %>%
    mutate(
      tipo_seleccion = "ex_ante",
      periodo_metricas = "test",
      seleccionado_ex_ante_test = .model == nombre_mejor_modelo_ex_ante,
      top3_ex_ante_test = .model %in% nombres_top3_modelos_ex_ante
    ) %>%
    arrange(
      is.na(orden_ex_ante),
      orden_ex_ante
    )
  
  metricas_top3_ex_ante_test <- tabla_final_ex_ante_test %>%
    filter(top3_ex_ante_test) %>%
    arrange(orden_ex_ante)
  
  graficos_ex_ante <- crear_graficos_modelo_sarima_9_10(
    comparacion_pred_real = comparacion_ex_ante,
    nombre_modelo = nombre_mejor_modelo_ex_ante,
    variable = variable,
    fecha = fecha,
    titulo_base = "SARIMA ex ante"
  )
  
  nombre_mejor_modelo_ex_post <- resultado_test$nombre_mejor_modelo
  mejor_modelo_ex_post <- resultado_test$mejor_modelo
  comparacion_ex_post <- resultado_test$comparacion_pred_real
  
  nombres_top3_modelos_ex_post <- resultado_test$tabla_final %>%
    slice_head(n = 3) %>%
    pull(.model)
  
  comparacion_top3_ex_post <- resultado_test$predicciones_vs_real %>%
    filter(.model %in% nombres_top3_modelos_ex_post)
  
  metricas_top3_ex_post <- resultado_test$tabla_final %>%
    filter(.model %in% nombres_top3_modelos_ex_post) %>%
    mutate(
      orden_ex_post = match(.model, nombres_top3_modelos_ex_post)
    ) %>%
    arrange(orden_ex_post)
  
  graficos_ex_post <- crear_graficos_modelo_sarima_9_10(
    comparacion_pred_real = comparacion_ex_post,
    nombre_modelo = nombre_mejor_modelo_ex_post,
    variable = variable,
    fecha = fecha,
    titulo_base = "SARIMA ex post"
  )
  
  # ----------------------------------------------------------
  # C. Salida estructurada y compatible.
  # ----------------------------------------------------------
  list(
    evento = nombre_evento,
    fecha_corte = particion$fecha_corte,
    fecha_inicio_test = particion$fecha_inicio_test,
    fecha_fin_test = particion$fecha_fin_test,
    horizonte = horizonte,
    train_evento = particion$train_evento,
    test_evento = particion$test_evento,
    diagnostico_estacionariedad = resultado_test$diagnostico_estacionariedad,
    d_elegido = resultado_test$d_elegido,
    D_elegido = resultado_test$D_elegido,
    validacion_rolling = validacion_rolling,
    tabla_validacion_rolling = validacion_rolling$tabla_validacion_modelos,
    tabla_validacion_completa = validacion_rolling$tabla_validacion_completa,
    ventanas_validacion_rolling = validacion_rolling$ventanas_validacion_rolling,
    nombre_mejor_modelo_ex_ante = nombre_mejor_modelo_ex_ante,
    mejor_modelo_ex_ante_validacion = validacion_rolling$mejor_modelo_ex_ante_validacion,
    mejor_modelo_ex_ante_test = fila_ex_ante_test,
    comparacion_pred_real_ex_ante = comparacion_ex_ante,
    nombres_top3_modelos_ex_ante = nombres_top3_modelos_ex_ante,
    tabla_final_ex_ante_test = tabla_final_ex_ante_test,
    metricas_top3_ex_ante_test = metricas_top3_ex_ante_test,
    comparacion_top3_ex_ante = comparacion_top3_ex_ante,
    grafico_pred_real_ex_ante = graficos_ex_ante$grafico_pred_real,
    grafico_error_ex_ante = graficos_ex_ante$grafico_error,
    nombre_mejor_modelo_ex_post = nombre_mejor_modelo_ex_post,
    nombres_top3_modelos_ex_post = nombres_top3_modelos_ex_post,
    metricas_top3_ex_post = metricas_top3_ex_post,
    comparacion_top3_ex_post = comparacion_top3_ex_post,
    mejor_modelo_ex_post = mejor_modelo_ex_post,
    tabla_final_ex_post = resultado_test$tabla_final,
    comparacion_pred_real_ex_post = comparacion_ex_post,
    grafico_pred_real_ex_post = graficos_ex_post$grafico_pred_real,
    grafico_error_ex_post = graficos_ex_post$grafico_error,
    resultado_test_completo = resultado_test,
    # Alias compatibles con el script original: por defecto se devuelven los resultados ex post.
    tabla_final = resultado_test$tabla_final,
    mejor_modelo = resultado_test$mejor_modelo,
    nombre_mejor_modelo = resultado_test$nombre_mejor_modelo,
    comparacion_pred_real = resultado_test$comparacion_pred_real,
    grafico_pred_real = resultado_test$grafico_pred_real,
    grafico_error = resultado_test$grafico_error
  )
}


#Esta función implementa la anterior, permitiendo incluir más de una fecha de 
#ruptura. 

evaluar_eventos_sarima_6m_9_10 <- function(datos_completos,
                                           tabla_eventos,
                                           variable = "y_ipc_general",
                                           fecha = "fecha_mensual",
                                           m = 12,
                                           alpha = 0.05,
                                           ljung_lag = 24,
                                           horizonte = 6,
                                           incluir_mes_corte_en_train = FALSE,
                                           horizonte_validacion = 6,
                                           min_train_validacion = 72,
                                           paso_validacion = 6,
                                           max_ventanas_validacion = 8,
                                           margen_rmsfe_validacion = 0.10,
                                           continuar_si_error = TRUE) {
  
  resultados <- vector("list", nrow(tabla_eventos))
  errores <- list()
  
  for (i in seq_len(nrow(tabla_eventos))) {
    evento_i <- tabla_eventos$evento[[i]]
    fecha_corte_i <- tabla_eventos$fecha_corte[[i]]
    
    resultado_i <- tryCatch(
      {
        seleccionar_sarima_evento_6m_9_10(
          datos_completos = datos_completos,
          fecha_corte = fecha_corte_i,
          variable = variable,
          fecha = fecha,
          m = m,
          alpha = alpha,
          ljung_lag = ljung_lag,
          horizonte = horizonte,
          incluir_mes_corte_en_train = incluir_mes_corte_en_train,
          nombre_evento = evento_i,
          horizonte_validacion = horizonte_validacion,
          min_train_validacion = min_train_validacion,
          paso_validacion = paso_validacion,
          max_ventanas_validacion = max_ventanas_validacion,
          margen_rmsfe_validacion = margen_rmsfe_validacion
        )
      },
      error = function(e) {
        if (!continuar_si_error) {
          stop(e)
        }
        errores[[length(errores) + 1]] <<- tibble(
          evento = evento_i,
          fecha_corte = fecha_corte_i,
          mensaje_error = conditionMessage(e)
        )
        NULL
      }
    )
    
    resultados[[i]] <- resultado_i
  }
  
  nombres_eventos <- tabla_eventos$evento
  names(resultados) <- nombres_eventos
  resultados_validos <- resultados[!map_lgl(resultados, is.null)]
  
  tabla_resumen_eventos <- imap_dfr(
    resultados_validos,
    function(res, evento_nm) {
      rmsfe_ex_ante <- res$mejor_modelo_ex_ante_test$RMSFE[[1]]
      rmsfe_ex_post <- res$mejor_modelo_ex_post$RMSFE[[1]]
      
      tibble(
        evento = evento_nm,
        fecha_corte = as.character(res$fecha_corte),
        fecha_inicio_test = as.character(res$fecha_inicio_test),
        fecha_fin_test = as.character(res$fecha_fin_test),
        n_train = nrow(res$train_evento),
        n_test = nrow(res$test_evento),
        nombre_mejor_modelo_ex_ante = res$nombre_mejor_modelo_ex_ante,
        RMSFE_ex_ante_test = rmsfe_ex_ante,
        MAE_ex_ante_test = res$mejor_modelo_ex_ante_test$MAE[[1]],
        MASE_ex_ante_test = res$mejor_modelo_ex_ante_test$MASE[[1]],
        diagnostico_ljung_ex_ante = res$mejor_modelo_ex_ante_test$diagnostico_ljung[[1]],
        nombre_mejor_modelo_ex_post = res$nombre_mejor_modelo_ex_post,
        RMSFE_ex_post_test = rmsfe_ex_post,
        MAE_ex_post_test = res$mejor_modelo_ex_post$MAE[[1]],
        MASE_ex_post_test = res$mejor_modelo_ex_post$MASE[[1]],
        diagnostico_ljung_ex_post = res$mejor_modelo_ex_post$diagnostico_ljung[[1]],
        diferencia_RMSFE_ex_ante_vs_ex_post = rmsfe_ex_ante - rmsfe_ex_post
      )
    }
  )
  
  tabla_errores_eventos <- if (length(errores) == 0) {
    tibble()
  } else {
    bind_rows(errores)
  }
  
  list(
    resultados_eventos = resultados_validos,
    tabla_resumen_eventos = tabla_resumen_eventos,
    tabla_errores_eventos = tabla_errores_eventos,
    nota_metodologica = "Modelo ex ante seleccionado por validación rolling histórica dentro del train; test del evento reservado para evaluación limpia. Modelo ex post incluido solo como referencia retrospectiva."
  )
}


# ============================================================
# 7. EJEMPLO DE USO SARIMA 9/10
# ============================================================
 setwd("C:/Users/rober/Desktop/UNIR/TFM/ubicacion_datos/tablasR")
#
 df_ipc_m <- read_excel("df_ipc_m.xlsx") %>%
   mutate(fecha_mensual = yearmonth(fecha_mensual)) %>%
   arrange(fecha_mensual) %>%
   as_tsibble(index = fecha_mensual)

 tabla_eventos <- tibble(
   evento = c(
     "Post-confinamiento",
     "Repunte inflacionario de 2021",
     "Inicio invasión rusa en Ucrania"
   ),
   fecha_corte = c(
     "2020 Mar",
     "2021 Mar",
     "2022 Feb"
   )
 )

 resultados_sarima_9_10 <- evaluar_eventos_sarima_6m_9_10(
   datos_completos = df_ipc_m,
   tabla_eventos = tabla_eventos,
   variable = "y_ipc_general",
   fecha = "fecha_mensual",
   m = 12,
   alpha = 0.05,
   ljung_lag = 24,
   horizonte = 6,
   incluir_mes_corte_en_train = FALSE,
   horizonte_validacion = 6,
   min_train_validacion = 72,
   paso_validacion = 6,
   max_ventanas_validacion = 8
 )
 
#Tablas necesarias para la combinación de predicciones 
View(resultados_sarima_9_10$resultados_eventos$`Post-confinamiento`$tabla_final)
View(resultados_sarima_9_10$resultados_eventos$`Post-confinamiento`$metricas_top3_ex_post)



 View(resultados_sarima_9_10$tabla_resumen_eventos)
 resultado_post <- resultados_sarima_9_10$resultados_eventos$`Post-confinamiento`
 resultado_rep <- resultados_sarima_9_10$resultados_eventos$`Repunte inflacionario de 2021`
 resultado_ucr <- resultados_sarima_9_10$resultados_eventos$`Inicio invasión rusa en Ucrania`
 resultado_post$diagnostico_estacionariedad
 View(resultado_post$tabla_validacion_rolling)
 resultado_post$grafico_pred_real_ex_ante
 resultado_post$grafico_pred_real_ex_post
 resultado_rep$grafico_pred_real_ex_ante
 resultado_rep$grafico_pred_real_ex_post
 resultado_ucr$grafico_pred_real_ex_ante
 resultado_ucr$grafico_pred_real_ex_post
 
 tab_val_post <- resultado_post$tabla_validacion_rolling
 tab_val_rep <- resultado_rep$tabla_validacion_rolling
 tab_val_ucr <- resultado_ucr$tabla_validacion_rolling
 
 tab_expost_post <- resultado_post$tabla_final_ex_post
 tab_expost_rep <- resultado_rep$tabla_final_ex_post
 tab_expost_ucr <- resultado_ucr$tabla_final_ex_post
 
 View (resultado_post$tabla_validacion_completa)
 View(tab_expost_post)
 View(tab_val_rep)
 View(tab_val_ucr)
 
 
pred_2020_sarima_exante<-resultados_sarima_9_10$resultados_eventos$`Post-confinamiento`$comparacion_top3_ex_ante
pred_2020_sarima_expost<-resultados_sarima_9_10$resultados_eventos$`Post-confinamiento`$comparacion_top3_ex_post

pred_2021_sarima_exante<-resultados_sarima_9_10$resultados_eventos$`Repunte inflacionario de 2021`$comparacion_top3_ex_ante
pred_2021_sarima_expost<-resultados_sarima_9_10$resultados_eventos$`Repunte inflacionario de 2021`$comparacion_top3_ex_post

pred_2022_sarima_exante<-resultados_sarima_9_10$resultados_eventos$`Inicio invasión rusa en Ucrania`$comparacion_top3_ex_ante
pred_2022_sarima_expost<-resultados_sarima_9_10$resultados_eventos$`Inicio invasión rusa en Ucrania`$comparacion_top3_ex_post

#Tablas de métricas ex ante y ex post
View(resultados_sarima_9_10$resultados_eventos$`Post-confinamiento`$metricas_top3_ex_ante_test)
View(resultados_sarima_9_10$resultados_eventos$`Post-confinamiento`$metricas_top3_ex_post)

met_2020_exante<-resultados_sarima_9_10$resultados_eventos$`Post-confinamiento`$metricas_top3_ex_ante_test 
met_2020_expost<-resultados_sarima_9_10$resultados_eventos$`Post-confinamiento`$metricas_top3_ex_post

met_2021_exante<-resultados_sarima_9_10$resultados_eventos$`Repunte inflacionario de 2021`$metricas_top3_ex_ante_test 
met_2021_expost<-resultados_sarima_9_10$resultados_eventos$`Repunte inflacionario de 2021`$metricas_top3_ex_post

met_2022_exante<-resultados_sarima_9_10$resultados_eventos$`Inicio invasión rusa en Ucrania`$metricas_top3_ex_ante_test 
met_2022_expost<-resultados_sarima_9_10$resultados_eventos$`Inicio invasión rusa en Ucrania`$metricas_top3_ex_post


library(writexl)
ruta_salida <- "C:/Users/rober/Desktop/UNIR/TFM/proyecto/tablas_resultados_seg_version"
write_xlsx(met_2020_expost, file.path(ruta_salida, "met_2020_expost.xlsx"))

#Tabla expost con todos los modelos 
sarima_2020_expost_completa<-resultados_sarima_9_10$resultados_eventos$`Post-confinamiento`$tabla_final_ex_post
sarima_2021_expost_completa<-resultados_sarima_9_10$resultados_eventos$`Repunte inflacionario de 2021`$tabla_final_ex_post
sarima_2022_expost_completa<-resultados_sarima_9_10$resultados_eventos$`Inicio invasión rusa en Ucrania`$tabla_final_ex_post

library(writexl)
ruta_salida <- "C:/Users/rober/Desktop/UNIR/TFM/ubicacion_datos/tablas_para_visualizaciones"
write_xlsx(sarima_2022_expost_completa, file.path(ruta_salida, "sarima_2022_expost_completa.xlsx"))


#Tabla ex ante con todos los modelos
sarima_2020_exante_completa <- resultados_sarima_9_10$resultados_eventos$`Post-confinamiento`$tabla_final_ex_ante_test
sarima_2021_exante_completa <- resultados_sarima_9_10$resultados_eventos$`Repunte inflacionario de 2021`$tabla_final_ex_ante_test
sarima_2022_exante_completa <- resultados_sarima_9_10$resultados_eventos$`Inicio invasión rusa en Ucrania`$tabla_final_ex_ante_test


library(writexl)
ruta_salida <- "C:/Users/rober/Desktop/UNIR/TFM/ubicacion_datos/tablas_para_visualizaciones"

write_xlsx(
  sarima_2020_exante_completa,
  file.path(ruta_salida, "sarima_2020_exante_completa.xlsx")
)

write_xlsx(
  sarima_2021_exante_completa,
  file.path(ruta_salida, "sarima_2021_exante_completa.xlsx")
)

write_xlsx(
  sarima_2022_exante_completa,
  file.path(ruta_salida, "sarima_2022_exante_completa.xlsx")
)