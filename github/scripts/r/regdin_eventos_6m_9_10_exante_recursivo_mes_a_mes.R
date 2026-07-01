# ============================================================
# PIPELINE 9/10 PARA REGRESIÓN DINÁMICA CON ERRORES ARIMA
# ============================================================
# Mejoras frente a la versión anterior:
#   1. Comprueba estacionariedad de y y regresores dentro de cada evento.
#   2. Separa selección y evaluación:
#        - train_seleccion: estima candidatos.
#        - validacion_interna: selecciona modelo.
#        - test_evento: evalúa el modelo seleccionado.
#   3. No selecciona el modelo final usando los 6 meses del evento.
#   4. Permite retardos de regresores, por defecto 1:3.
#   5. Controla colinealidad aproximada entre regresores mediante correlación.
#   6. Evita especificaciones ARIMA con diferenciación forzada en el error.
#      La idea es que y y los regresores ya entren estacionarios.
#   7. Sustituye la evaluación de test de horizonte completo por una evaluación
#      ex ante recursiva one-step-ahead:
#        - para predecir abril se usan datos hasta marzo;
#        - para predecir mayo se usan datos hasta abril;
#        - y así sucesivamente.
#
# Nota importante:
# En esta versión los regresores contemporáneos del mes objetivo no se utilizan
# en la evaluación ex ante. Por eso el ejemplo usa lags_regresores = 1:3.
# Así, para predecir mayo solo pueden entrar regresores de abril o anteriores.
# ============================================================

library(tidyverse)
library(tsibble)
library(feasts)
library(fable)
library(fabletools)
library(lubridate)
library(rlang)
library(readxl)


# ============================================================
# 1. FUNCIÓN AUXILIAR: CRITERIO MODELO LIMPIO
# ============================================================
# Se usa sobre la validación interna, no sobre el test del evento.
# ============================================================

aplicar_criterio_modelo_limpio <- function(tabla_final,
                                           predicciones_vs_real,
                                           alpha = 0.05,
                                           margen_rmsfe = 0.10,
                                           min_obs_dm = 10) {

  if (nrow(tabla_final) == 0) {
    stop("tabla_final está vacía: no hay modelos candidatos.")
  }

  columnas_necesarias <- c(".model", "RMSFE", "MAE", "AICc", "BIC", "diagnostico_ljung")

  if (!all(columnas_necesarias %in% names(tabla_final))) {
    stop("tabla_final no contiene las columnas mínimas requeridas para aplicar modelo_limpio.")
  }

  rmsfe_minimo <- min(tabla_final$RMSFE, na.rm = TRUE)
  limite_rmsfe_candidato <- rmsfe_minimo * (1 + margen_rmsfe)

  modelo_menor_rmsfe <- tabla_final %>%
    arrange(RMSFE, MAE, AICc, BIC) %>%
    slice(1) %>%
    pull(.model)

  columna_fecha_pred <- setdiff(
    names(predicciones_vs_real),
    c(".model", "prediccion", "real", "error", "abs_error", "sq_error")
  )[[1]]

  errores_ancho <- predicciones_vs_real %>%
    select(all_of(columna_fecha_pred), .model, error) %>%
    pivot_wider(
      names_from = .model,
      values_from = error
    )

  calcular_dm_aproximado <- function(nombre_modelo) {

    if (is.na(nombre_modelo) || nombre_modelo == modelo_menor_rmsfe) {
      return(tibble(
        .model = nombre_modelo,
        dm_stat_vs_mejor_rmsfe = NA_real_,
        dm_p_value_vs_mejor_rmsfe = NA_real_,
        diagnostico_dm = "modelo_referencia_menor_RMSFE"
      ))
    }

    if (!(modelo_menor_rmsfe %in% names(errores_ancho)) || !(nombre_modelo %in% names(errores_ancho))) {
      return(tibble(
        .model = nombre_modelo,
        dm_stat_vs_mejor_rmsfe = NA_real_,
        dm_p_value_vs_mejor_rmsfe = NA_real_,
        diagnostico_dm = "errores_no_disponibles"
      ))
    }

    e_ref <- errores_ancho[[modelo_menor_rmsfe]]
    e_mod <- errores_ancho[[nombre_modelo]]

    diferencial <- e_mod^2 - e_ref^2
    diferencial <- diferencial[is.finite(diferencial)]
    n_dm <- length(diferencial)

    if (n_dm < min_obs_dm) {
      return(tibble(
        .model = nombre_modelo,
        dm_stat_vs_mejor_rmsfe = NA_real_,
        dm_p_value_vs_mejor_rmsfe = NA_real_,
        diagnostico_dm = paste0("no_aplicable_n_menor_", min_obs_dm)
      ))
    }

    sd_diferencial <- stats::sd(diferencial, na.rm = TRUE)

    if (is.na(sd_diferencial) || sd_diferencial == 0) {
      return(tibble(
        .model = nombre_modelo,
        dm_stat_vs_mejor_rmsfe = NA_real_,
        dm_p_value_vs_mejor_rmsfe = NA_real_,
        diagnostico_dm = "no_aplicable_varianza_nula"
      ))
    }

    dm_stat <- mean(diferencial, na.rm = TRUE) / (sd_diferencial / sqrt(n_dm))
    dm_p_value <- 2 * stats::pnorm(-abs(dm_stat))

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

  tabla_dm <- tabla_final %>%
    pull(.model) %>%
    map_dfr(calcular_dm_aproximado)

  tabla_final_limpia <- tabla_final %>%
    left_join(tabla_dm, by = ".model") %>%
    mutate(
      rmsfe_minimo = rmsfe_minimo,
      margen_rmsfe = margen_rmsfe,
      limite_rmsfe_candidato = limite_rmsfe_candidato,
      diferencia_relativa_RMSFE = (RMSFE / rmsfe_minimo) - 1,
      candidato_modelo_limpio = RMSFE <= limite_rmsfe_candidato,
      residuos_ok_modelo_limpio = diagnostico_ljung == "residuos_ok",
      prioridad_residuos_modelo_limpio = case_when(
        diagnostico_ljung == "residuos_ok" ~ 1L,
        diagnostico_ljung == "no_calculable" ~ 2L,
        diagnostico_ljung == "autocorrelacion_residual" ~ 3L,
        TRUE ~ 4L
      ),
      criterio_modelo_limpio = case_when(
        candidato_modelo_limpio & residuos_ok_modelo_limpio ~ "candidato_RMSFE_similar_residuos_ok",
        candidato_modelo_limpio & !residuos_ok_modelo_limpio ~ "candidato_RMSFE_similar_residuos_no_ok",
        !candidato_modelo_limpio ~ "fuera_margen_RMSFE",
        TRUE ~ "sin_clasificar"
      )
    )

  candidatos <- tabla_final_limpia %>%
    filter(candidato_modelo_limpio)

  if (nrow(candidatos) == 0) {
    candidatos <- tabla_final_limpia
  }

  mejor_modelo_limpio <- candidatos %>%
    arrange(
      MAE,
      prioridad_residuos_modelo_limpio,
      AICc,
      RMSFE,
      BIC
    ) %>%
    slice(1)

  candidatos_residuos_ok <- candidatos %>%
    filter(residuos_ok_modelo_limpio)

  if (
    nrow(candidatos_residuos_ok) > 0 &&
    mejor_modelo_limpio$diagnostico_ljung[[1]] != "residuos_ok"
  ) {
    mejor_modelo_limpio <- candidatos_residuos_ok %>%
      arrange(MAE, AICc, RMSFE, BIC) %>%
      slice(1) %>%
      mutate(
        criterio_modelo_limpio = paste0(
          criterio_modelo_limpio,
          "_seleccionado_por_residuos_ok_con_RMSFE_similar"
        )
      )
  }

  tabla_final_limpia <- tabla_final_limpia %>%
    mutate(
      seleccionado_modelo_limpio = .model == mejor_modelo_limpio$.model[[1]]
    ) %>%
    arrange(
      desc(seleccionado_modelo_limpio),
      candidato_modelo_limpio == FALSE,
      MAE,
      prioridad_residuos_modelo_limpio,
      AICc,
      RMSFE,
      BIC
    )

  list(
    tabla_final = tabla_final_limpia,
    mejor_modelo = mejor_modelo_limpio,
    nombre_mejor_modelo = mejor_modelo_limpio$.model[[1]],
    modelo_referencia_menor_rmsfe = modelo_menor_rmsfe,
    margen_rmsfe = margen_rmsfe
  )
}


# ============================================================
# 2. DIAGNÓSTICO DE ESTACIONARIEDAD
# ============================================================

#Esta función diagnostica la estacionariedad de la vble objetivo, así como del
#resto de vbles (regresores). 


comprobar_estacionariedad_regdin <- function(datos,
                                             fecha = "fecha_mensual",
                                             variables,
                                             alpha = 0.05) {
  
  #Se asegura que las variables existan en la tabla de datos
  variables_validas <- variables[variables %in% names(datos)]

  if (length(variables_validas) == 0) {
    return(tibble())
  }
  
  #Se pasan los datos de formato ancho a formato largo, creando una vble 
  #llamada "variable_modelo" donde cada categoría es una de las vbles (obj o 
  #regresores)

  datos_largos <- datos %>%
    as_tibble() %>%
    select(all_of(c(fecha, variables_validas))) %>%
    pivot_longer(
      cols = all_of(variables_validas),
      names_to = "variable_modelo",
      values_to = "valor"
    ) %>%
    filter(!is.na(valor), is.finite(valor)) %>%
    as_tsibble(
      key = variable_modelo,
      index = !!sym(fecha)
    )

  if (nrow(datos_largos) == 0) {
    return(tibble())
  }
  
  #Se calculan los tests: 
  # KPSS: 
  # Hipótesis nula del KPSS: la serie es estacionaria.
  # Hipótesis alternativa: la serie no es estacionaria.
  
  
  #ndiffs: 
  # ndiffs = 0: no parece requerir diferenciación.
  # ndiffs = 1: podría requerir una diferenciación.
  # ndiffs > 1: requeriría más diferenciaciones, aunque en práctica hay que revisar con cuidado.
  
  #Phillips-Perron: 
  # Hipótesis nula del PP: la serie tiene raíz unitaria, es decir, no es estacionaria.
  # Hipótesis alternativa: la serie es estacionaria.

  tabla_kpss <- tryCatch(
    datos_largos %>% features(valor, unitroot_kpss),
    error = function(e) tibble(variable_modelo = unique(datos_largos$variable_modelo), kpss_stat = NA_real_, kpss_pvalue = NA_real_)
  )

  tabla_ndiffs <- tryCatch(
    datos_largos %>% features(valor, unitroot_ndiffs),
    error = function(e) tibble(variable_modelo = unique(datos_largos$variable_modelo), ndiffs = NA_integer_)
  )

  tabla_pp <- tryCatch(
    datos_largos %>% features(valor, unitroot_pp),
    error = function(e) tibble(variable_modelo = unique(datos_largos$variable_modelo), pp_stat = NA_real_, pp_pvalue = NA_real_)
  )
  
  #Se crea un resumen con las principales medidas estadísticas para cada vble
  tabla_resumen_basica <- datos_largos %>%
    as_tibble() %>%
    group_by(variable_modelo) %>%
    summarise(
      n_obs = sum(!is.na(valor)),
      media = mean(valor, na.rm = TRUE),
      sd = sd(valor, na.rm = TRUE),
      min = min(valor, na.rm = TRUE),
      max = max(valor, na.rm = TRUE),
      .groups = "drop"
    )
  
  #Se crea una tabla con los resultados de los tests anteriores y la tabla de 
  #las medidas estadísticas 
  tabla_kpss %>%
    left_join(tabla_ndiffs, by = "variable_modelo") %>%
    left_join(tabla_pp, by = "variable_modelo") %>%
    left_join(tabla_resumen_basica, by = "variable_modelo") %>%
    mutate( #se crea una columna booleana para test KPSS
      estacionaria_kpss = case_when(
        is.na(kpss_pvalue) ~ NA,
        kpss_pvalue >= alpha ~ TRUE,
        kpss_pvalue < alpha ~ FALSE
      ),
      requiere_diferencia = case_when( #se crea otra columna booleana en caso de
        #concluirse que la serie requiere diferenciación (utilizando el test KPSS)
        !is.na(ndiffs) & ndiffs > 0 ~ TRUE,
        !is.na(kpss_pvalue) & kpss_pvalue < alpha ~ TRUE,
        !is.na(kpss_pvalue) & kpss_pvalue >= alpha ~ FALSE,
        TRUE ~ NA
      ),
      
      #Criterio final, que decide si requiere o no requiere diferenciación cada vble. 
      diagnostico_estacionariedad = case_when(
        requiere_diferencia == TRUE ~ "posible_no_estacionaria_revisar_transformacion",
        requiere_diferencia == FALSE ~ "compatible_con_estacionariedad",
        TRUE ~ "no_concluyente"
      )
    ) %>%
    arrange(desc(requiere_diferencia), variable_modelo)
}


# ============================================================
# 3. CREACIÓN DE RETARDOS DE REGRESORES
# ============================================================

crear_retardos_regresores <- function(datos,
                                      fecha = "fecha_mensual",
                                      regresores,
                                      lags_regresores = 1:3) {

  datos <- datos %>% arrange(.data[[fecha]])

  diccionario <- tibble()

  for (reg in regresores) {
    if (!reg %in% names(datos)) next #si el regresor no está en el conjunto de
    #datos, se pasa a lo siguiente. 

    for (lag_i in lags_regresores) {
      if (lag_i == 0) {
        nombre_lag <- reg #no hace falta indciar nada si no hay lag
      } else {
        nombre_lag <- paste0(reg, "_lag", lag_i) #se crea el nombre del retardo
        datos <- datos %>% mutate(!!nombre_lag := dplyr::lag(.data[[reg]], lag_i))
        #se crea el propio retardo. 
      }
      
      #Finalmente, se crea una tabla en la que 

      diccionario <- bind_rows( #para cada regresor y lag de regresor, se crea 
        #una línea a continuación. 
        diccionario,
        tibble(
          regresor_original = reg,
          lag = lag_i,
          regresor_modelo = nombre_lag
        )
      )
    }
  }
  
  #En la salida se devuelve la base de datos original, pero con la ampliación 
  #de las columnas retardadas, el diccionario (tabla que documenta todos los
  #lags que se han creado y están disponibles para el modelo) y un vector
  #con todas las vbles que se pueden utilizar como regresores. 

  list(
    datos = datos,
    diccionario_regresores = diccionario,
    regresores_modelo = unique(diccionario$regresor_modelo)
  )
}


# ============================================================
# 4. UTILIDADES DE MODELIZACIÓN
# ============================================================

crear_grid_errores_estacionarios <- function(m = 12) {
  tibble(
    id_error = c(
      "ARMA_auto_d0_D0",
      "ARMA_000",
      "ARMA_100",
      "ARMA_001",
      "ARMA_101",
      "SARMA_001_001",
      "SARMA_101_001",
      "SARMA_101_101"
    ),
    especificacion_error = c(
      paste0("pdq(d = 0) + PDQ(D = 0, period = ", m, ")"),
      "pdq(0, 0, 0)",
      "pdq(1, 0, 0)",
      "pdq(0, 0, 1)",
      "pdq(1, 0, 1)",
      paste0("pdq(0, 0, 1) + PDQ(0, 0, 1, period = ", m, ")"),
      paste0("pdq(1, 0, 1) + PDQ(0, 0, 1, period = ", m, ")"),
      paste0("pdq(1, 0, 1) + PDQ(1, 0, 1, period = ", m, ")")
    )
  )
}

construir_formula_regdin <- function(variable,
                                     regresores_combo,
                                     especificacion_error) {

  parte_regresores <- if (length(regresores_combo) == 0) {
    "1"
  } else {
    paste(regresores_combo, collapse = " + ")
  }

  paste(variable, "~", parte_regresores, "+", especificacion_error)
}

calcular_max_correlacion_combo <- function(datos,
                                           regresores_combo) {

  if (length(regresores_combo) <= 1) {
    return(0)
  }

  datos_corr <- datos %>%
    as_tibble() %>%
    select(all_of(regresores_combo)) %>%
    drop_na()

  if (nrow(datos_corr) < 5) {
    return(NA_real_)
  }

  matriz_corr <- suppressWarnings(cor(datos_corr, use = "pairwise.complete.obs"))

  if (all(is.na(matriz_corr))) {
    return(NA_real_)
  }

  matriz_corr[lower.tri(matriz_corr, diag = TRUE)] <- NA_real_
  max(abs(matriz_corr), na.rm = TRUE)
}

crear_grid_desde_combinaciones <- function(variable,
                                           lista_combinaciones,
                                           grid_errores,
                                           datos_correlacion = NULL,
                                           umbral_correlacion = 0.95) {

  grid_combinaciones <- tibble(
    id_combo = sprintf("REG%03d", seq_along(lista_combinaciones)),
    regresores_combo = lista_combinaciones
  ) %>%
    mutate(
      n_regresores = map_int(regresores_combo, length),
      regresores_texto = map_chr(
        regresores_combo,
        ~ ifelse(length(.x) == 0, "sin_regresores", paste(.x, collapse = " + "))
      ),
      max_abs_corr_regresores = map_dbl(
        regresores_combo,
        ~ if (is.null(datos_correlacion)) NA_real_ else calcular_max_correlacion_combo(datos_correlacion, .x)
      ),
      supera_umbral_correlacion = case_when(
        is.na(max_abs_corr_regresores) ~ FALSE,
        max_abs_corr_regresores > umbral_correlacion ~ TRUE,
        TRUE ~ FALSE
      )
    )

  grid_combinaciones_filtrado <- grid_combinaciones %>%
    filter(!supera_umbral_correlacion)

  grid_modelos <- expand_grid(
    grid_combinaciones_filtrado,
    grid_errores
  ) %>%
    mutate(
      nombre_modelo = paste0(id_combo, "__", id_error),
      formula_texto = map2_chr(
        regresores_combo,
        especificacion_error,
        ~ construir_formula_regdin(variable, .x, .y)
      )
    )

  list(
    grid_combinaciones_original = grid_combinaciones,
    grid_combinaciones = grid_combinaciones_filtrado,
    grid_modelos = grid_modelos
  )
}

ajustar_grid_modelos_regdin <- function(datos_ajuste,
                                        grid_modelos,
                                        fecha = "fecha_mensual") {

  if (!tsibble::is_tsibble(datos_ajuste)) {
    datos_ajuste <- datos_ajuste %>% as_tsibble(index = !!sym(fecha))
  }

  ajustes <- vector("list", nrow(grid_modelos))

  for (i in seq_len(nrow(grid_modelos))) {

    nombre_modelo_i <- grid_modelos$nombre_modelo[[i]]
    formula_modelo_i <- as.formula(grid_modelos$formula_texto[[i]])

    ajuste_i <- tryCatch(
      expr = {
        suppressWarnings(
          datos_ajuste %>% model(modelo_tmp = ARIMA(formula_modelo_i))
        )
      },
      error = function(e) NULL
    )

    if (is.null(ajuste_i)) {
      ajustes[[i]] <- NULL
      next
    }

    names(ajuste_i)[names(ajuste_i) == "modelo_tmp"] <- nombre_modelo_i

    tabla_glance_i <- tryCatch(
      suppressWarnings(glance(ajuste_i)),
      error = function(e) NULL
    )

    if (is.null(tabla_glance_i)) {
      ajustes[[i]] <- NULL
      next
    }

    if (!all(c("AICc", "BIC") %in% names(tabla_glance_i))) {
      ajustes[[i]] <- NULL
      next
    }

    if (
      is.na(tabla_glance_i$AICc[[1]]) ||
      is.na(tabla_glance_i$BIC[[1]]) ||
      !is.finite(tabla_glance_i$AICc[[1]]) ||
      !is.finite(tabla_glance_i$BIC[[1]])
    ) {
      ajustes[[i]] <- NULL
      next
    }

    ajustes[[i]] <- ajuste_i
  }

  ajustes_validos <- compact(ajustes)

  if (length(ajustes_validos) == 0) {
    return(NULL)
  }

  reduce(ajustes_validos, bind_cols)
}

extraer_tabla_ajuste_regdin <- function(modelos_ajustados,
                                        grid_modelos) {

  modelos_ajustados %>%
    glance() %>%
    select(.model, AICc, BIC) %>%
    filter(
      !is.na(AICc), !is.na(BIC),
      is.finite(AICc), is.finite(BIC)
    ) %>%
    arrange(AICc) %>%
    mutate(ranking_AICc = row_number()) %>%
    left_join(
      grid_modelos %>%
        select(
          .model = nombre_modelo,
          id_combo,
          id_error,
          regresores_combo,
          n_regresores,
          regresores_texto,
          formula_texto,
          max_abs_corr_regresores
        ),
      by = ".model"
    )
}

calcular_ljung_modelos_regdin <- function(modelos_ajustados,
                                          modelos_validos,
                                          alpha = 0.05,
                                          ljung_lag = 24) {

  tabla_coeficientes <- tryCatch(
    modelos_ajustados %>%
      select(all_of(modelos_validos)) %>%
      tidy(),
    error = function(e) tibble()
  )

  tabla_parametros <- tabla_coeficientes %>%
    as_tibble() %>%
    count(.model, name = "n_parametros")

  residuos_modelos <- map_dfr(
    modelos_validos,
    function(modelo_i) {
      tryCatch(
        modelos_ajustados %>%
          select(all_of(modelo_i)) %>%
          augment() %>%
          as_tibble(),
        error = function(e) tibble()
      )
    }
  )

  if (nrow(residuos_modelos) == 0) {
    tabla_ljung <- tibble(
      .model = modelos_validos,
      n_parametros = NA_integer_,
      n_residuos_validos = NA_integer_,
      ljung_p_value = NA_real_,
      diagnostico_ljung = "no_calculable"
    )
  } else {
    tabla_ljung <- residuos_modelos %>%
      as_tibble() %>%
      left_join(tabla_parametros, by = ".model") %>%
      mutate(n_parametros = replace_na(n_parametros, 0)) %>%
      group_by(.model, n_parametros) %>%
      summarise(
        n_residuos_validos = sum(!is.na(.resid)),
        ljung_p_value = {
          residuos_validos <- na.omit(.resid)
          # Aproximación conservadora: no se permite que fitdf deje sin grados de libertad.
          dof_ljung <- min(first(n_parametros), ljung_lag - 1)

          if (length(residuos_validos) > ljung_lag) {
            tryCatch(
              Box.test(
                residuos_validos,
                lag = ljung_lag,
                type = "Ljung-Box",
                fitdf = dof_ljung
              )$p.value,
              error = function(e) NA_real_
            )
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
  }

  list(
    tabla_ljung = tabla_ljung,
    tabla_coeficientes = tabla_coeficientes
  )
}

calcular_predicciones_y_metricas <- function(modelos_ajustados,
                                             modelos_validos,
                                             new_data,
                                             variable,
                                             fecha = "fecha_mensual",
                                             y_train_para_mase = NULL,
                                             m = 12) {

  if (!tsibble::is_tsibble(new_data)) {
    new_data <- new_data %>% as_tsibble(index = !!sym(fecha))
  }

  forecasts <- map_dfr(
    modelos_validos,
    function(modelo_i) {
      tryCatch(
        modelos_ajustados %>%
          select(all_of(modelo_i)) %>%
          forecast(new_data = new_data) %>%
          as_tibble(),
        error = function(e) tibble()
      )
    }
  )

  if (nrow(forecasts) == 0) {
    return(list(
      forecasts = tibble(),
      predicciones_vs_real = tibble(),
      tabla_metricas = tibble()
    ))
  }

  predicciones_vs_real <- forecasts %>%
    select(all_of(fecha), .model, prediccion = .mean) %>%
    left_join(
      new_data %>%
        as_tibble() %>%
        transmute(!!fecha := .data[[fecha]], real = .data[[variable]]),
      by = fecha
    ) %>%
    filter(
      !is.na(real), !is.na(prediccion),
      is.finite(real), is.finite(prediccion)
    ) %>%
    mutate(
      error = real - prediccion,
      abs_error = abs(error),
      sq_error = error^2
    )

  if (nrow(predicciones_vs_real) == 0) {
    return(list(
      forecasts = forecasts,
      predicciones_vs_real = tibble(),
      tabla_metricas = tibble()
    ))
  }

  denominador_mase <- NA_real_

  if (!is.null(y_train_para_mase)) {
    y_train_num <- as.numeric(y_train_para_mase)
    y_train_num <- y_train_num[is.finite(y_train_num)]

    if (length(y_train_num) > m) {
      denominador_mase <- mean(abs(y_train_num[(m + 1):length(y_train_num)] - y_train_num[1:(length(y_train_num) - m)]), na.rm = TRUE)
    }

    if (is.na(denominador_mase) || denominador_mase == 0) {
      if (length(y_train_num) > 1) {
        denominador_mase <- mean(abs(diff(y_train_num)), na.rm = TRUE)
      }
    }

    if (is.na(denominador_mase) || denominador_mase == 0) {
      denominador_mase <- NA_real_
    }
  }

  tabla_metricas <- predicciones_vs_real %>%
    group_by(.model) %>%
    summarise(
      n_eval = n(),
      RMSFE = sqrt(mean(sq_error, na.rm = TRUE)),
      MAE = mean(abs_error, na.rm = TRUE),
      MASE = MAE / denominador_mase,
      .groups = "drop"
    ) %>%
    filter(
      !is.na(RMSFE), !is.na(MAE),
      is.finite(RMSFE), is.finite(MAE)
    ) %>%
    arrange(RMSFE, MAE, MASE) %>%
    mutate(
      ranking_RMSFE = row_number(),
      ranking_MAE = rank(MAE, ties.method = "first"),
      ranking_MASE = rank(MASE, ties.method = "first")
    )

  list(
    forecasts = forecasts,
    predicciones_vs_real = predicciones_vs_real,
    tabla_metricas = tabla_metricas
  )
}


# ============================================================
# 4B. PREDICCIONES RECURSIVAS ONE-STEP-AHEAD
# ============================================================
# Para cada fecha objetivo t:
#   - se define como origen de predicción t - 1 mes;
#   - se reestima cada modelo con todos los datos disponibles hasta t - 1;
#   - se predice únicamente el mes t;
#   - se incorporan los valores reales observados conforme avanza la evaluación.
#
# IMPORTANTE:
# Para que esto sea ex ante, los regresores candidatos deben estar retardados.
# Por eso, en el ejemplo de uso se utiliza lags_regresores = 1:3.
# ============================================================

calcular_denominador_mase_regdin <- function(y_train_para_mase,
                                             m = 12) {

  denominador_mase <- NA_real_

  if (!is.null(y_train_para_mase)) {
    y_train_num <- as.numeric(y_train_para_mase)
    y_train_num <- y_train_num[is.finite(y_train_num)]

    if (length(y_train_num) > m) {
      denominador_mase <- mean(
        abs(y_train_num[(m + 1):length(y_train_num)] -
              y_train_num[1:(length(y_train_num) - m)]),
        na.rm = TRUE
      )
    }

    if (is.na(denominador_mase) || denominador_mase == 0) {
      if (length(y_train_num) > 1) {
        denominador_mase <- mean(abs(diff(y_train_num)), na.rm = TRUE)
      }
    }

    if (is.na(denominador_mase) || denominador_mase == 0) {
      denominador_mase <- NA_real_
    }
  }

  denominador_mase
}


calcular_predicciones_recursivas_y_metricas <- function(datos_recursivos,
                                                        fechas_objetivo,
                                                        grid_modelos,
                                                        modelos_validos = NULL,
                                                        variable,
                                                        fecha = "fecha_mensual",
                                                        y_train_para_mase = NULL,
                                                        m = 12,
                                                        min_train_recursivo = NULL) {

  datos_recursivos <- datos_recursivos %>%
    arrange(.data[[fecha]])

  if (!tsibble::is_tsibble(datos_recursivos)) {
    datos_recursivos <- datos_recursivos %>% as_tsibble(index = !!sym(fecha))
  }

  fechas_objetivo <- tsibble::yearmonth(fechas_objetivo)
  fechas_objetivo <- sort(unique(fechas_objetivo))

  if (is.null(modelos_validos)) {
    grid_modelos_rec <- grid_modelos
  } else {
    grid_modelos_rec <- grid_modelos %>%
      filter(nombre_modelo %in% modelos_validos)
  }

  if (nrow(grid_modelos_rec) == 0 || length(fechas_objetivo) == 0) {
    return(list(
      forecasts = tibble(),
      predicciones_vs_real = tibble(),
      tabla_metricas = tibble()
    ))
  }

  regresores_grid <- unique(unlist(grid_modelos_rec$regresores_combo, use.names = FALSE))
  regresores_grid <- regresores_grid[
    !is.na(regresores_grid) &
      regresores_grid != "" &
      regresores_grid %in% names(datos_recursivos)
  ]

  columnas_obligatorias_train <- c(variable, regresores_grid)
  columnas_obligatorias_train <- columnas_obligatorias_train[
    columnas_obligatorias_train %in% names(datos_recursivos)
  ]

  if (is.null(min_train_recursivo)) {
    min_train_recursivo <- max(24, m + 6)
  }

  lista_forecasts <- list()
  
  for (i_fecha in seq_along(fechas_objetivo)) {
    
    fecha_objetivo_i <- fechas_objetivo[i_fecha]
    fecha_origen_i <- fecha_objetivo_i - 1
    
    datos_ajuste_i <- datos_recursivos %>%
      filter(.data[[fecha]] <= fecha_origen_i) %>%
      drop_na(all_of(columnas_obligatorias_train))
    
    new_data_i <- datos_recursivos %>%
      filter(.data[[fecha]] == fecha_objetivo_i)

    if (nrow(datos_ajuste_i) < min_train_recursivo || nrow(new_data_i) == 0) {
      next
    }

    if (length(regresores_grid) > 0) {
      new_data_i <- new_data_i %>%
        filter(if_all(all_of(regresores_grid), ~ !is.na(.x) & is.finite(.x)))
    }

    if (nrow(new_data_i) == 0) {
      next
    }

    modelos_i <- ajustar_grid_modelos_regdin(
      datos_ajuste = datos_ajuste_i,
      grid_modelos = grid_modelos_rec,
      fecha = fecha
    )

    if (is.null(modelos_i)) {
      next
    }

    modelos_validos_i <- intersect(grid_modelos_rec$nombre_modelo, names(modelos_i))

    if (length(modelos_validos_i) == 0) {
      next
    }

    forecasts_i <- map_dfr(
      modelos_validos_i,
      function(modelo_i) {
        tryCatch(
          modelos_i %>%
            select(all_of(modelo_i)) %>%
            forecast(new_data = new_data_i) %>%
            as_tibble(),
          error = function(e) tibble()
        )
      }
    )

    if (nrow(forecasts_i) == 0) {
      next
    }

    forecasts_i <- forecasts_i %>%
      mutate(
        fecha_origen_prediccion = fecha_origen_i,
        tipo_prediccion = "ex_ante_recursiva_1_paso"
      )

    lista_forecasts[[as.character(fecha_objetivo_i)]] <- forecasts_i
  }

  forecasts <- bind_rows(lista_forecasts)

  if (nrow(forecasts) == 0) {
    return(list(
      forecasts = tibble(),
      predicciones_vs_real = tibble(),
      tabla_metricas = tibble()
    ))
  }

  reales <- datos_recursivos %>%
    as_tibble() %>%
    transmute(
      !!fecha := .data[[fecha]],
      real = .data[[variable]]
    )

  predicciones_vs_real <- forecasts %>%
    select(all_of(fecha), fecha_origen_prediccion, tipo_prediccion, .model, prediccion = .mean) %>%
    left_join(reales, by = fecha) %>%
    filter(
      !is.na(real), !is.na(prediccion),
      is.finite(real), is.finite(prediccion)
    ) %>%
    mutate(
      error = real - prediccion,
      abs_error = abs(error),
      sq_error = error^2
    )

  if (nrow(predicciones_vs_real) == 0) {
    return(list(
      forecasts = forecasts,
      predicciones_vs_real = tibble(),
      tabla_metricas = tibble()
    ))
  }

  denominador_mase <- calcular_denominador_mase_regdin(
    y_train_para_mase = y_train_para_mase,
    m = m
  )

  tabla_metricas <- predicciones_vs_real %>%
    group_by(.model) %>%
    summarise(
      n_eval = n(),
      inicio_eval = min(.data[[fecha]], na.rm = TRUE),
      fin_eval = max(.data[[fecha]], na.rm = TRUE),
      inicio_origen = min(fecha_origen_prediccion, na.rm = TRUE),
      fin_origen = max(fecha_origen_prediccion, na.rm = TRUE),
      RMSFE = sqrt(mean(sq_error, na.rm = TRUE)),
      MAE = mean(abs_error, na.rm = TRUE),
      MASE = MAE / denominador_mase,
      denominador_MASE = denominador_mase,
      .groups = "drop"
    ) %>%
    filter(
      !is.na(RMSFE), !is.na(MAE),
      is.finite(RMSFE), is.finite(MAE)
    ) %>%
    arrange(RMSFE, MAE, MASE) %>%
    mutate(
      ranking_RMSFE = row_number(),
      ranking_MAE = rank(MAE, ties.method = "first"),
      ranking_MASE = rank(MASE, ties.method = "first")
    )

  list(
    forecasts = forecasts,
    predicciones_vs_real = predicciones_vs_real,
    tabla_metricas = tabla_metricas
  )
}


# ============================================================
# 5. FUNCIÓN PRINCIPAL 9/10: SELECCIÓN CON VALIDACIÓN INTERNA
# ============================================================

seleccionar_regresion_dinamica_automatico_9_10 <- function(train_data,
                                                          test_data,
                                                          variable = "y_ipc_general",
                                                          fecha = "fecha_mensual",
                                                          regresores = NULL,
                                                          lags_regresores = 1:3,
                                                          min_regresores = 1,
                                                          max_regresores = 3,
                                                          incluir_modelo_sin_regresores = FALSE,
                                                          m = 12,
                                                          alpha = 0.05,
                                                          ljung_lag = 24,
                                                          n_top_errores = 3,
                                                          n_top_regresores = 5,
                                                          validation_size = 6,
                                                          margen_rmsfe = 0.10,
                                                          umbral_correlacion = 0.95,
                                                          detener_si_no_estacionaria = FALSE,
                                                          modo_prediccion_regresores = "ex_ante_recursivo_1_paso",
                                                          datos_recursivos_completos = NULL) {

  # ----------------------------------------------------------
  # 5.1. Preparación básica
  # ----------------------------------------------------------

  train_data <- train_data %>% arrange(.data[[fecha]])
  test_data <- test_data %>% arrange(.data[[fecha]])

  if (is.null(regresores)) {
    regresores <- setdiff(names(train_data), c(fecha, variable))
  }

  regresores <- regresores[
    regresores %in% names(train_data) &
      regresores %in% names(test_data)
  ]

  if (length(regresores) == 0 && !incluir_modelo_sin_regresores) {
    stop("No hay regresores válidos en train_data y test_data.")
  }

  train_tmp <- train_data %>% mutate(.origen_regdin = "train")
  test_tmp <- test_data %>% mutate(.origen_regdin = "test")

  datos_combinados <- bind_rows(train_tmp, test_tmp) %>%
    arrange(.data[[fecha]])

  retardos <- crear_retardos_regresores(
    datos = datos_combinados,
    fecha = fecha,
    regresores = regresores,
    lags_regresores = lags_regresores
  )

  datos_con_lags <- retardos$datos
  diccionario_regresores <- retardos$diccionario_regresores
  regresores_modelo <- retardos$regresores_modelo

  train_con_lags <- datos_con_lags %>%
    filter(.origen_regdin == "train") %>%
    select(-.origen_regdin)

  test_con_lags <- datos_con_lags %>%
    filter(.origen_regdin == "test") %>%
    select(-.origen_regdin)

  regresores_modelo <- regresores_modelo[
    regresores_modelo %in% names(train_con_lags) &
      regresores_modelo %in% names(test_con_lags)
  ]

  train_con_lags <- train_con_lags %>%
    drop_na(all_of(c(variable, regresores_modelo)))

  test_con_lags <- test_con_lags %>%
    filter(if_all(all_of(regresores_modelo), ~ !is.na(.x) & is.finite(.x)))

  if (nrow(train_con_lags) <= validation_size + max(24, m + 6)) {
    stop("El entrenamiento queda demasiado corto para separar validación interna y estimar modelos con seguridad.")
  }

  if (nrow(test_con_lags) == 0) {
    stop("test_data queda vacío tras exigir regresores no nulos.")
  }

  if (is.null(datos_recursivos_completos)) {
    datos_recursivos_completos <- bind_rows(train_con_lags, test_con_lags) %>%
      arrange(.data[[fecha]]) %>%
      distinct(!!sym(fecha), .keep_all = TRUE)
  } else {
    datos_recursivos_completos <- datos_recursivos_completos %>%
      arrange(.data[[fecha]])
  }

  regresores_modelo <- regresores_modelo[
    regresores_modelo %in% names(datos_recursivos_completos)
  ]

  # ----------------------------------------------------------
  # 5.2. Diagnóstico de estacionariedad dentro del train del evento
  # ----------------------------------------------------------

  diagnostico_estacionariedad <- comprobar_estacionariedad_regdin(
    datos = train_con_lags,
    fecha = fecha,
    variables = c(variable, regresores),
    alpha = alpha
  )

  variables_no_estacionarias <- diagnostico_estacionariedad %>%
    filter(requiere_diferencia == TRUE) %>%
    pull(variable_modelo)

  if (length(variables_no_estacionarias) > 0) {
    mensaje_estacionariedad <- paste0(
      "Posibles variables no estacionarias según KPSS/ndiffs: ",
      paste(variables_no_estacionarias, collapse = ", "),
      ". Revisa transformaciones antes de interpretar coeficientes o predicciones."
    )

    if (detener_si_no_estacionaria) {
      stop(mensaje_estacionariedad)
    } else {
      warning(mensaje_estacionariedad)
    }
  }

  # ----------------------------------------------------------
  # 5.3. Separación train_seleccion / validacion_interna
  # ----------------------------------------------------------

  n_train_total <- nrow(train_con_lags)
  indice_inicio_validacion <- n_train_total - validation_size + 1

  train_seleccion <- train_con_lags %>% slice(1:(indice_inicio_validacion - 1))
  validacion_interna <- train_con_lags %>% slice(indice_inicio_validacion:n_train_total)

  if (!tsibble::is_tsibble(train_seleccion)) {
    train_seleccion <- train_seleccion %>% as_tsibble(index = !!sym(fecha))
  }

  if (!tsibble::is_tsibble(validacion_interna)) {
    validacion_interna <- validacion_interna %>% as_tsibble(index = !!sym(fecha))
  }

  if (!tsibble::is_tsibble(train_con_lags)) {
    train_con_lags <- train_con_lags %>% as_tsibble(index = !!sym(fecha))
  }

  if (!tsibble::is_tsibble(test_con_lags)) {
    test_con_lags <- test_con_lags %>% as_tsibble(index = !!sym(fecha))
  }

  if (!tsibble::is_tsibble(datos_recursivos_completos)) {
    datos_recursivos_completos <- datos_recursivos_completos %>% as_tsibble(index = !!sym(fecha))
  }

  # ----------------------------------------------------------
  # 5.4. Grid de errores estacionarios
  # ----------------------------------------------------------

  grid_errores_completo <- crear_grid_errores_estacionarios(m = m)

  lista_sin_regresores <- list(character(0))

  grid_base <- crear_grid_desde_combinaciones(
    variable = variable,
    lista_combinaciones = lista_sin_regresores,
    grid_errores = grid_errores_completo,
    datos_correlacion = train_seleccion,
    umbral_correlacion = umbral_correlacion
  )$grid_modelos %>%
    mutate(nombre_modelo = paste0("BASE__", id_error))

  modelos_base <- ajustar_grid_modelos_regdin(
    datos_ajuste = train_seleccion,
    grid_modelos = grid_base,
    fecha = fecha
  )

  if (is.null(modelos_base)) {
    stop("No se pudo estimar ningún modelo base de error ARMA/SARMA.")
  }

  tabla_errores_base <- extraer_tabla_ajuste_regdin(
    modelos_ajustados = modelos_base,
    grid_modelos = grid_base
  ) %>%
    arrange(AICc)

  n_errores_disponibles <- nrow(tabla_errores_base)
  n_errores_seleccionados <- min(n_top_errores, n_errores_disponibles)

  ids_top_errores <- tabla_errores_base %>%
    arrange(AICc) %>%
    head(n_errores_seleccionados) %>%
    pull(id_error)

  grid_errores <- grid_errores_completo %>%
    filter(id_error %in% ids_top_errores)

  # ----------------------------------------------------------
  # 5.5. Preselección de regresores individuales por AICc
  # ----------------------------------------------------------

  if (length(regresores_modelo) > 0) {

    lista_regresores_individuales <- map(regresores_modelo, ~ .x)

    grid_individuales <- crear_grid_desde_combinaciones(
      variable = variable,
      lista_combinaciones = lista_regresores_individuales,
      grid_errores = grid_errores,
      datos_correlacion = train_seleccion,
      umbral_correlacion = umbral_correlacion
    )

    modelos_individuales <- ajustar_grid_modelos_regdin(
      datos_ajuste = train_seleccion,
      grid_modelos = grid_individuales$grid_modelos,
      fecha = fecha
    )

    if (is.null(modelos_individuales)) {
      stop("No se pudo estimar ningún modelo con regresores individuales.")
    }

    tabla_regresores_individuales <- extraer_tabla_ajuste_regdin(
      modelos_ajustados = modelos_individuales,
      grid_modelos = grid_individuales$grid_modelos
    ) %>%
      mutate(regresor = map_chr(regresores_combo, ~ .x[[1]])) %>%
      arrange(AICc)

    tabla_regresores_ordenada <- tabla_regresores_individuales %>%
      arrange(AICc) %>%
      distinct(regresor, .keep_all = TRUE)

    n_regresores_disponibles <- nrow(tabla_regresores_ordenada)
    n_regresores_seleccionados <- min(n_top_regresores, n_regresores_disponibles)

    top_regresores <- tabla_regresores_ordenada %>%
      arrange(AICc) %>%
      head(n_regresores_seleccionados) %>%
      pull(regresor)

  } else {
    tabla_regresores_individuales <- tibble()
    tabla_regresores_ordenada <- tibble()
    top_regresores <- character(0)
  }

  if (length(top_regresores) == 0 && !incluir_modelo_sin_regresores) {
    stop("No se ha seleccionado ningún regresor en la etapa individual.")
  }

  # ----------------------------------------------------------
  # 5.6. Combinaciones finales
  # ----------------------------------------------------------

  max_regresores_final <- min(max_regresores, 3, length(top_regresores))
  min_regresores_final <- min(min_regresores, max_regresores_final)

  lista_combinaciones <- list()

  if (incluir_modelo_sin_regresores) {
    lista_combinaciones <- append(lista_combinaciones, list(character(0)))
  }

  if (length(top_regresores) > 0 && max_regresores_final >= min_regresores_final) {
    tamanos <- min_regresores_final:max_regresores_final
    combinaciones_con_regresores <- tamanos %>%
      map(~ combn(top_regresores, .x, simplify = FALSE)) %>%
      flatten()
    lista_combinaciones <- append(lista_combinaciones, combinaciones_con_regresores)
  }

  if (length(lista_combinaciones) == 0) {
    stop("No se ha generado ninguna combinación final de regresores.")
  }

  grids_finales <- crear_grid_desde_combinaciones(
    variable = variable,
    lista_combinaciones = lista_combinaciones,
    grid_errores = grid_errores,
    datos_correlacion = train_seleccion,
    umbral_correlacion = umbral_correlacion
  )

  grid_combinaciones_original <- grids_finales$grid_combinaciones_original
  grid_combinaciones <- grids_finales$grid_combinaciones
  grid_modelos <- grids_finales$grid_modelos

  if (nrow(grid_modelos) == 0) {
    stop("Todas las combinaciones fueron descartadas por colinealidad u otros filtros.")
  }

  # ----------------------------------------------------------
  # 5.7. Ajuste en train_seleccion y selección en validación interna
  #      con predicción recursiva 1 paso
  # ----------------------------------------------------------

  modelos_validacion <- ajustar_grid_modelos_regdin(
    datos_ajuste = train_seleccion,
    grid_modelos = grid_modelos,
    fecha = fecha
  )

  if (is.null(modelos_validacion)) {
    stop("Ningún modelo final pudo estimarse en train_seleccion.")
  }

  tabla_ajuste_validacion <- extraer_tabla_ajuste_regdin(
    modelos_ajustados = modelos_validacion,
    grid_modelos = grid_modelos
  )

  modelos_validos_validacion <- tabla_ajuste_validacion %>% pull(.model)

  diag_ljung_validacion <- calcular_ljung_modelos_regdin(
    modelos_ajustados = modelos_validacion,
    modelos_validos = modelos_validos_validacion,
    alpha = alpha,
    ljung_lag = ljung_lag
  )

  pred_val <- calcular_predicciones_recursivas_y_metricas(
    datos_recursivos = train_con_lags,
    fechas_objetivo = validacion_interna[[fecha]],
    grid_modelos = grid_modelos,
    modelos_validos = modelos_validos_validacion,
    variable = variable,
    fecha = fecha,
    y_train_para_mase = train_seleccion[[variable]],
    m = m
  )

  if (nrow(pred_val$tabla_metricas) == 0) {
    stop("No se pudieron calcular métricas recursivas en la validación interna.")
  }

  tabla_final_validacion_pre <- tabla_ajuste_validacion %>%
    left_join(diag_ljung_validacion$tabla_ljung, by = ".model") %>%
    inner_join(pred_val$tabla_metricas, by = ".model") %>%
    mutate(
      tipo_evaluacion = "validacion_ex_ante_recursiva_1_paso",
      puntuacion_total = ranking_RMSFE + ranking_MAE
    ) %>%
    arrange(ranking_RMSFE, ranking_MAE, ranking_AICc, BIC)

  seleccion_validacion <- aplicar_criterio_modelo_limpio(
    tabla_final = tabla_final_validacion_pre,
    predicciones_vs_real = pred_val$predicciones_vs_real,
    alpha = alpha,
    margen_rmsfe = margen_rmsfe,
    min_obs_dm = 10
  )

  nombre_modelo_seleccionado <- seleccion_validacion$nombre_mejor_modelo
  modelo_limpio_validacion <- seleccion_validacion$mejor_modelo
  
  nombres_top3_modelos_ex_ante <- seleccion_validacion$tabla_final %>%
    slice_head(n = 3) %>%
    pull(.model)

  # ----------------------------------------------------------
  # 5.8. Evaluación ex post del evento con predicción recursiva 1 paso
  # ----------------------------------------------------------
  # La estructura de modelos candidatos es la elegida antes del evento.
  # Para cada mes del test se reestima cada candidato con toda la información
  # disponible hasta el mes anterior y se predice solo el mes siguiente.
  # Después se aplica también el criterio modelo_limpio sobre el test para
  # analizar cuál habría sido el mejor modelo ex post, sin sustituir la selección
  # ex ante realizada en validación.
  # ----------------------------------------------------------

  fecha_origen_inicial_test <- min(test_con_lags[[fecha]], na.rm = TRUE) - 1

  train_inicial_test_recursivo <- datos_recursivos_completos %>%
    filter(.data[[fecha]] <= fecha_origen_inicial_test) %>%
    drop_na(all_of(c(variable, regresores_modelo)))

  if (nrow(train_inicial_test_recursivo) <= max(24, m + 6)) {
    stop("El train inicial del test recursivo es demasiado corto.")
  }

  modelos_test <- ajustar_grid_modelos_regdin(
    datos_ajuste = train_inicial_test_recursivo,
    grid_modelos = grid_modelos,
    fecha = fecha
  )

  if (is.null(modelos_test)) {
    stop("No se pudieron estimar los modelos con el train inicial del test recursivo.")
  }

  tabla_ajuste_test <- extraer_tabla_ajuste_regdin(
    modelos_ajustados = modelos_test,
    grid_modelos = grid_modelos
  )

  modelos_validos_test <- tabla_ajuste_test %>% pull(.model)

  if (!nombre_modelo_seleccionado %in% modelos_validos_test) {
    stop("El modelo seleccionado en validación no pudo estimarse en el train inicial del test recursivo.")
  }

  diag_ljung_test <- calcular_ljung_modelos_regdin(
    modelos_ajustados = modelos_test,
    modelos_validos = modelos_validos_test,
    alpha = alpha,
    ljung_lag = ljung_lag
  )

  pred_test <- calcular_predicciones_recursivas_y_metricas(
    datos_recursivos = datos_recursivos_completos,
    fechas_objetivo = test_con_lags[[fecha]],
    grid_modelos = grid_modelos,
    modelos_validos = modelos_validos_test,
    variable = variable,
    fecha = fecha,
    y_train_para_mase = train_inicial_test_recursivo[[variable]],
    m = m
  )

  if (nrow(pred_test$tabla_metricas) == 0) {
    stop("No se pudieron calcular métricas recursivas en el test del evento.")
  }
  
  comparacion_top3_ex_ante <- pred_test$predicciones_vs_real %>%
    filter(.model %in% nombres_top3_modelos_ex_ante)

  tabla_final_test_pre <- tabla_ajuste_test %>%
    left_join(diag_ljung_test$tabla_ljung, by = ".model") %>%
    inner_join(pred_test$tabla_metricas, by = ".model") %>%
    mutate(
      seleccionado_por_validacion = .model == nombre_modelo_seleccionado,
      tipo_evaluacion = "test_ex_post_recursivo_1_paso",
      puntuacion_total = ranking_RMSFE + ranking_MAE
    ) %>%
    arrange(desc(seleccionado_por_validacion), ranking_RMSFE, ranking_MAE, ranking_AICc)

  seleccion_test_expost <- aplicar_criterio_modelo_limpio(
    tabla_final = tabla_final_test_pre,
    predicciones_vs_real = pred_test$predicciones_vs_real,
    alpha = alpha,
    margen_rmsfe = margen_rmsfe,
    min_obs_dm = 10
  )

  tabla_final_test <- seleccion_test_expost$tabla_final %>%
    mutate(
      seleccionado_por_validacion = .model == nombre_modelo_seleccionado,
      seleccionado_expost_modelo_limpio = seleccionado_modelo_limpio
    ) %>%
    arrange(
      desc(seleccionado_por_validacion),
      desc(seleccionado_expost_modelo_limpio),
      candidato_modelo_limpio == FALSE,
      MAE,
      prioridad_residuos_modelo_limpio,
      AICc,
      RMSFE,
      BIC
    )
  
  # ----------------------------------------------------------
  # Tabla completa de métricas de test, pero ordenada según
  # el criterio de selección ex ante obtenido en validación.
  # ----------------------------------------------------------
  
  tabla_orden_ex_ante <- seleccion_validacion$tabla_final %>%
    mutate(
      orden_ex_ante = row_number()
    ) %>%
    transmute(
      .model,
      orden_ex_ante,
      
      seleccionado_modelo_limpio_ex_ante = seleccionado_modelo_limpio,
      candidato_modelo_limpio_ex_ante = candidato_modelo_limpio,
      residuos_ok_modelo_limpio_ex_ante = residuos_ok_modelo_limpio,
      prioridad_residuos_modelo_limpio_ex_ante = prioridad_residuos_modelo_limpio,
      criterio_modelo_limpio_ex_ante = criterio_modelo_limpio,
      
      RMSFE_validacion = RMSFE,
      MAE_validacion = MAE,
      MASE_validacion = MASE,
      AICc_validacion = AICc,
      BIC_validacion = BIC,
      
      ranking_RMSFE_validacion = ranking_RMSFE,
      ranking_MAE_validacion = ranking_MAE,
      ranking_MASE_validacion = ranking_MASE,
      ranking_AICc_validacion = ranking_AICc,
      
      ljung_p_value_validacion = ljung_p_value,
      diagnostico_ljung_validacion = diagnostico_ljung,
      
      diferencia_relativa_RMSFE_validacion = diferencia_relativa_RMSFE,
      dm_stat_vs_mejor_rmsfe_validacion = dm_stat_vs_mejor_rmsfe,
      dm_p_value_vs_mejor_rmsfe_validacion = dm_p_value_vs_mejor_rmsfe,
      diagnostico_dm_validacion = diagnostico_dm
    )
  
  tabla_final_test_ex_ante <- tabla_final_test_pre %>%
    left_join(
      tabla_orden_ex_ante,
      by = ".model"
    ) %>%
    mutate(
      tipo_evaluacion = "test_ex_ante_recursivo_1_paso",
      seleccionado_por_validacion = .model == nombre_modelo_seleccionado,
      top3_ex_ante_test = .model %in% nombres_top3_modelos_ex_ante
    ) %>%
    arrange(
      is.na(orden_ex_ante),
      orden_ex_ante
    )
  
  metricas_top3_ex_ante_test <- tabla_final_test_ex_ante %>%
    filter(top3_ex_ante_test) %>%
    arrange(orden_ex_ante)
  
  # Top 3 ex post: modelos ordenados por comportamiento en el test
  tabla_final_test_ex_post <- tabla_final_test %>%
    arrange(ranking_RMSFE, ranking_MAE, ranking_AICc, BIC)
  
  nombres_top3_modelos_ex_post <- tabla_final_test_ex_post %>%
    slice_head(n = 3) %>%
    pull(.model)
  
  comparacion_top3_ex_post <- pred_test$predicciones_vs_real %>%
    filter(.model %in% nombres_top3_modelos_ex_post)
  
  metricas_top3_ex_post <- tabla_final_test_ex_post %>%
    filter(.model %in% nombres_top3_modelos_ex_post) %>%
    mutate(
      orden_ex_post = match(.model, nombres_top3_modelos_ex_post)
    ) %>%
    arrange(orden_ex_post)

  mejor_modelo_test_limpio <- tabla_final_test %>%
    filter(.model == nombre_modelo_seleccionado) %>%
    slice(1)

  modelo_limpio_expost <- seleccion_test_expost$mejor_modelo

  comparacion_pred_real <- pred_test$predicciones_vs_real %>%
    filter(.model == nombre_modelo_seleccionado)

  if (nrow(comparacion_pred_real) == 0) {
    stop("No hay predicciones válidas en test para el modelo seleccionado en validación.")
  }

  # ----------------------------------------------------------
  # 5.9. Gráficos finales
  # ----------------------------------------------------------

  grafico_pred_real <- comparacion_pred_real %>%
    ggplot(aes(x = .data[[fecha]])) +
    geom_line(aes(y = real, colour = "Valor real"), linewidth = 1) +
    geom_line(aes(y = prediccion, colour = "Predicción"), linewidth = 1, linetype = "dashed") +
    labs(
      title = paste("Test recursivo: valores reales vs predicciones -", nombre_modelo_seleccionado),
      subtitle = "Modelo elegido en validación interna; predicción ex ante recursiva un paso",
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
      title = paste("Test recursivo: error de predicción -", nombre_modelo_seleccionado),
      x = "Fecha",
      y = "Error = real - predicción"
    )

  # ----------------------------------------------------------
  # 5.10. Mensaje resumen
  # ----------------------------------------------------------

  cat("\n")
  cat("============================================================\n")
  cat("REGRESIÓN DINÁMICA 9/10: RESUMEN EX ANTE RECURSIVO\n")
  cat("============================================================\n")
  cat("Variable objetivo:", variable, "\n")
  cat("Modo de predicción:", modo_prediccion_regresores, "\n")
  cat("Train selección:", as.character(min(train_seleccion[[fecha]])), "a", as.character(max(train_seleccion[[fecha]])), "\n")
  cat("Validación interna:", as.character(min(validacion_interna[[fecha]])), "a", as.character(max(validacion_interna[[fecha]])), "\n")
  cat("Test evento:", as.character(min(test_con_lags[[fecha]])), "a", as.character(max(test_con_lags[[fecha]])), "\n")
  cat("Origen inicial test:", as.character(fecha_origen_inicial_test), "\n")
  cat("Regresores originales:", paste(regresores, collapse = ", "), "\n")
  cat("Regresores candidatos con retardos:", paste(regresores_modelo, collapse = ", "), "\n")
  cat("Top errores:", paste(ids_top_errores, collapse = ", "), "\n")
  cat("Top regresores:", paste(top_regresores, collapse = ", "), "\n")
  cat("Modelo seleccionado en validación:", nombre_modelo_seleccionado, "\n")
  cat("Regresores del modelo seleccionado:", mejor_modelo_test_limpio$regresores_texto, "\n")
  cat("RMSFE test recursivo:", mejor_modelo_test_limpio$RMSFE, "\n")
  cat("MAE test recursivo:", mejor_modelo_test_limpio$MAE, "\n")
  cat("MASE test recursivo:", mejor_modelo_test_limpio$MASE, "\n")
  cat("Modelo limpio ex post en test:", modelo_limpio_expost$.model, "\n")
  cat("Diagnóstico Ljung-Box:", mejor_modelo_test_limpio$diagnostico_ljung, "\n")
  cat("============================================================\n\n")

  # ----------------------------------------------------------
  # 5.11. Salida
  # ----------------------------------------------------------

  list(
    modo_prediccion_regresores = modo_prediccion_regresores,
    diccionario_regresores = diccionario_regresores,
    diagnostico_estacionariedad = diagnostico_estacionariedad,
    variables_no_estacionarias = variables_no_estacionarias,
    train_seleccion = train_seleccion,
    validacion_interna = validacion_interna,
    train_evento_completo = train_con_lags,
    train_inicial_test_recursivo = train_inicial_test_recursivo,
    test_evento = test_con_lags,
    datos_recursivos_completos = datos_recursivos_completos,
    grid_errores_completo = grid_errores_completo,
    tabla_errores_base = tabla_errores_base,
    top_errores = ids_top_errores,
    tabla_regresores_individuales = tabla_regresores_individuales,
    tabla_regresores_ordenada = tabla_regresores_ordenada,
    top_regresores = top_regresores,
    grid_combinaciones_original = grid_combinaciones_original,
    grid_combinaciones = grid_combinaciones,
    grid_modelos = grid_modelos,
    modelos_validacion = modelos_validacion,
    tabla_ajuste_validacion = tabla_ajuste_validacion,
    tabla_ljung_validacion = diag_ljung_validacion$tabla_ljung,
    tabla_coeficientes_validacion = diag_ljung_validacion$tabla_coeficientes,
    predicciones_validacion = pred_val$predicciones_vs_real,
    tabla_metricas_validacion = pred_val$tabla_metricas,
    tabla_final_validacion = seleccion_validacion$tabla_final,
    seleccion_validacion = seleccion_validacion,
    modelo_limpio_validacion = modelo_limpio_validacion,
    modelos_test = modelos_test,
    tabla_ajuste_test = tabla_ajuste_test,
    tabla_ljung_test = diag_ljung_test$tabla_ljung,
    tabla_coeficientes_test = diag_ljung_test$tabla_coeficientes,
    predicciones_vs_real = pred_test$predicciones_vs_real,
    nombres_top3_modelos_ex_ante = nombres_top3_modelos_ex_ante,
    tabla_final_test_ex_ante = tabla_final_test_ex_ante,
    metricas_top3_ex_ante_test = metricas_top3_ex_ante_test,
    comparacion_top3_ex_ante = comparacion_top3_ex_ante,
    tabla_final_test_ex_post = tabla_final_test_ex_post,
    nombres_top3_modelos_ex_post = nombres_top3_modelos_ex_post,
    metricas_top3_ex_post = metricas_top3_ex_post,
    comparacion_top3_ex_post = comparacion_top3_ex_post,
    tabla_metricas_test = pred_test$tabla_metricas,
    tabla_final = tabla_final_test,
    seleccion_test_expost = seleccion_test_expost,
    modelo_limpio_expost = modelo_limpio_expost,
    mejor_modelo = mejor_modelo_test_limpio,
    modelo_limpio = mejor_modelo_test_limpio,
    nombre_mejor_modelo = nombre_modelo_seleccionado,
    comparacion_pred_real = comparacion_pred_real,
    grafico_pred_real = grafico_pred_real,
    grafico_error = grafico_error
  )
}

# ============================================================
# 6. PARTICIÓN DE EVENTOS
# ============================================================

crear_particion_evento_regdin_6m <- function(datos_completos,
                                             fecha_corte,
                                             fecha = "fecha_mensual",
                                             horizonte = 6,
                                             incluir_mes_corte = FALSE) {

  fecha_corte <- yearmonth(fecha_corte)

  if (incluir_mes_corte) {
    fecha_inicio_test <- fecha_corte
    fecha_fin_test <- fecha_corte + horizonte - 1
  } else {
    fecha_inicio_test <- fecha_corte + 1
    fecha_fin_test <- fecha_corte + horizonte
  }

  train_data <- datos_completos %>%
    arrange(.data[[fecha]]) %>%
    filter(.data[[fecha]] < fecha_corte)

  test_data <- datos_completos %>%
    arrange(.data[[fecha]]) %>%
    filter(
      .data[[fecha]] >= fecha_inicio_test,
      .data[[fecha]] <= fecha_fin_test
    )

  if (nrow(train_data) == 0) {
    stop("La partición generada no contiene observaciones de entrenamiento.")
  }

  if (nrow(test_data) == 0) {
    stop("La partición generada no contiene observaciones de test.")
  }

  if (nrow(test_data) < horizonte) {
    warning(
      paste0(
        "La ventana de test contiene ", nrow(test_data),
        " observaciones, aunque se esperaban ", horizonte, "."
      )
    )
  }

  list(
    fecha_corte = fecha_corte,
    fecha_inicio_test = fecha_inicio_test,
    fecha_fin_test = fecha_fin_test,
    train_data = train_data,
    test_data = test_data
  )
}


# ============================================================
# 7. EJECUCIÓN DE UN EVENTO
# ============================================================

seleccionar_regdin_evento_6m_9_10 <- function(datos_completos,
                                              fecha_corte,
                                              nombre_evento = NULL,
                                              variable = "y_ipc_general",
                                              fecha = "fecha_mensual",
                                              regresores = NULL,
                                              lags_regresores = 1:3,
                                              min_regresores = 1,
                                              max_regresores = 3,
                                              incluir_modelo_sin_regresores = FALSE,
                                              m = 12,
                                              alpha = 0.05,
                                              ljung_lag = 24,
                                              n_top_errores = 3,
                                              n_top_regresores = 5,
                                              horizonte = 6,
                                              incluir_mes_corte = FALSE,
                                              validation_size = 6,
                                              margen_rmsfe = 0.10,
                                              umbral_correlacion = 0.95,
                                              detener_si_no_estacionaria = FALSE,
                                              modo_prediccion_regresores = "ex_ante_recursivo_1_paso") {

  # ----------------------------------------------------------
  # Restricción ex ante: no usar regresores contemporáneos del mes objetivo
  # ----------------------------------------------------------
  # Para predecir abril desde marzo, el modelo no puede usar X_abril.
  # Por eso se eliminan los retardos 0 si se hubieran pasado por error.
  # Se conservan lag1, lag2, lag3, ... porque representan información conocida
  # hasta el mes anterior o meses anteriores.

  if (0 %in% lags_regresores) {
    warning("Se elimina lag 0 de lags_regresores para mantener una evaluación ex ante recursiva estricta.")
    lags_regresores <- setdiff(lags_regresores, 0)
  }

  if (length(lags_regresores) == 0) {
    stop("Para una evaluación ex ante recursiva se necesita al menos un retardo positivo de los regresores.")
  }

  # ----------------------------------------------------------
  # Construcción correcta de retardos antes de separar train/test
  # ----------------------------------------------------------
  # Los lags se crean sobre la base completa, no sobre train + test ya recortados.
  # Así, para predecir mayo, por ejemplo, X_lag1 corresponde a abril.
  # Después la evaluación recursiva garantiza que mayo se predice desde abril.
  # ----------------------------------------------------------

  datos_completos <- datos_completos %>%
    arrange(.data[[fecha]])

  if (is.null(regresores)) {
    regresores_originales <- setdiff(names(datos_completos), c(fecha, variable))
  } else {
    regresores_originales <- regresores
  }

  regresores_originales <- regresores_originales[
    regresores_originales %in% names(datos_completos)
  ]

  if (length(regresores_originales) == 0 && !incluir_modelo_sin_regresores) {
    stop("No hay regresores originales válidos en datos_completos.")
  }

  retardos_globales <- crear_retardos_regresores(
    datos = datos_completos,
    fecha = fecha,
    regresores = regresores_originales,
    lags_regresores = lags_regresores
  )

  datos_completos_con_lags <- retardos_globales$datos
  diccionario_regresores_global <- retardos_globales$diccionario_regresores
  regresores_modelo_global <- retardos_globales$regresores_modelo

  regresores_modelo_global <- regresores_modelo_global[
    regresores_modelo_global %in% names(datos_completos_con_lags)
  ]

  particion <- crear_particion_evento_regdin_6m(
    datos_completos = datos_completos_con_lags,
    fecha_corte = fecha_corte,
    fecha = fecha,
    horizonte = horizonte,
    incluir_mes_corte = incluir_mes_corte
  )

  cat("\n")
  cat("############################################################\n")
  cat("EVENTO DE RUPTURA:", ifelse(is.null(nombre_evento), as.character(particion$fecha_corte), nombre_evento), "\n")
  cat("Fecha de corte:", as.character(particion$fecha_corte), "\n")
  cat("Train de selección: hasta", as.character(max(particion$train_data[[fecha]], na.rm = TRUE)), "\n")
  cat("Test recursivo:", as.character(particion$fecha_inicio_test), "a", as.character(particion$fecha_fin_test), "\n")
  cat("Observaciones train selección:", nrow(particion$train_data), "\n")
  cat("Observaciones test:", nrow(particion$test_data), "\n")
  cat("Retardos de regresores usados:", paste(lags_regresores, collapse = ", "), "\n")
  cat("############################################################\n\n")

  resultado <- seleccionar_regresion_dinamica_automatico_9_10(
    train_data = particion$train_data,
    test_data = particion$test_data,
    variable = variable,
    fecha = fecha,
    regresores = regresores_modelo_global,
    lags_regresores = 0,
    min_regresores = min_regresores,
    max_regresores = max_regresores,
    incluir_modelo_sin_regresores = incluir_modelo_sin_regresores,
    m = m,
    alpha = alpha,
    ljung_lag = ljung_lag,
    n_top_errores = n_top_errores,
    n_top_regresores = n_top_regresores,
    validation_size = validation_size,
    margen_rmsfe = margen_rmsfe,
    umbral_correlacion = umbral_correlacion,
    detener_si_no_estacionaria = detener_si_no_estacionaria,
    modo_prediccion_regresores = modo_prediccion_regresores,
    datos_recursivos_completos = datos_completos_con_lags
  )

  resultado$nombre_evento <- nombre_evento
  resultado$fecha_corte <- particion$fecha_corte
  resultado$fecha_inicio_test <- particion$fecha_inicio_test
  resultado$fecha_fin_test <- particion$fecha_fin_test
  resultado$horizonte <- horizonte
  resultado$incluir_mes_corte <- incluir_mes_corte
  resultado$n_train_evento <- nrow(particion$train_data)
  resultado$n_test_evento <- nrow(particion$test_data)

  resultado$train_evento_original <- particion$train_data
  resultado$test_evento_original <- particion$test_data

  resultado$datos_completos_con_lags <- datos_completos_con_lags
  resultado$diccionario_regresores <- diccionario_regresores_global
  resultado$regresores_originales <- regresores_originales
  resultado$regresores_modelo_global <- regresores_modelo_global

  resultado$grafico_pred_real_evento <- resultado$comparacion_pred_real %>%
    ggplot(aes(x = .data[[fecha]])) +
    geom_line(aes(y = real, colour = "Valor real"), linewidth = 1) +
    geom_line(aes(y = prediccion, colour = "Predicción"), linewidth = 1, linetype = "dashed") +
    labs(
      title = paste(
        "Valores reales vs predicciones - Regresión dinámica ex ante recursiva -",
        ifelse(is.null(nombre_evento), as.character(particion$fecha_corte), nombre_evento)
      ),
      subtitle = paste0(
        "Modelo elegido en validación interna | Test recursivo: ",
        as.character(particion$fecha_inicio_test), " a ", as.character(particion$fecha_fin_test),
        " | Modelo: ", resultado$nombre_mejor_modelo
      ),
      x = "Fecha",
      y = variable,
      colour = ""
    )

  resultado$grafico_error_evento <- resultado$comparacion_pred_real %>%
    ggplot(aes(x = .data[[fecha]], y = error)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line() +
    geom_point() +
    labs(
      title = paste(
        "Error de predicción - Regresión dinámica ex ante recursiva -",
        ifelse(is.null(nombre_evento), as.character(particion$fecha_corte), nombre_evento)
      ),
      subtitle = paste0(
        "Test recursivo: ", as.character(particion$fecha_inicio_test),
        " a ", as.character(particion$fecha_fin_test),
        " | Modelo: ", resultado$nombre_mejor_modelo
      ),
      x = "Fecha",
      y = "Error = real - predicción"
    )

  resultado
}

# ============================================================
# 8. EJECUCIÓN DE LOS TRES EVENTOS
# ============================================================

ejecutar_regdin_tres_eventos_6m_9_10 <- function(datos_completos,
                                                 variable = "y_ipc_general",
                                                 fecha = "fecha_mensual",
                                                 regresores = NULL,
                                                 lags_regresores = 1:3,
                                                 min_regresores = 1,
                                                 max_regresores = 3,
                                                 incluir_modelo_sin_regresores = FALSE,
                                                 m = 12,
                                                 alpha = 0.05,
                                                 ljung_lag = 24,
                                                 n_top_errores = 3,
                                                 n_top_regresores = 5,
                                                 horizonte = 6,
                                                 incluir_mes_corte = FALSE,
                                                 validation_size = 6,
                                                 margen_rmsfe = 0.10,
                                                 umbral_correlacion = 0.95,
                                                 detener_si_no_estacionaria = FALSE,
                                                 puntos_ruptura = NULL,
                                                 continuar_si_error = TRUE,
                                                 modo_prediccion_regresores = "ex_ante_recursivo_1_paso") {

  if (is.null(puntos_ruptura)) {
    puntos_ruptura <- tibble(
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
  }

  if (!all(c("evento", "fecha_corte") %in% names(puntos_ruptura))) {
    stop("puntos_ruptura debe contener las columnas 'evento' y 'fecha_corte'.")
  }

  resultados_eventos <- pmap(
    list(puntos_ruptura$evento, puntos_ruptura$fecha_corte),
    function(evento, fecha_corte) {

      if (continuar_si_error) {
        tryCatch(
          seleccionar_regdin_evento_6m_9_10(
            datos_completos = datos_completos,
            fecha_corte = fecha_corte,
            nombre_evento = evento,
            variable = variable,
            fecha = fecha,
            regresores = regresores,
            lags_regresores = lags_regresores,
            min_regresores = min_regresores,
            max_regresores = max_regresores,
            incluir_modelo_sin_regresores = incluir_modelo_sin_regresores,
            m = m,
            alpha = alpha,
            ljung_lag = ljung_lag,
            n_top_errores = n_top_errores,
            n_top_regresores = n_top_regresores,
            horizonte = horizonte,
            incluir_mes_corte = incluir_mes_corte,
            validation_size = validation_size,
            margen_rmsfe = margen_rmsfe,
            umbral_correlacion = umbral_correlacion,
            detener_si_no_estacionaria = detener_si_no_estacionaria,
            modo_prediccion_regresores = modo_prediccion_regresores
          ),
          error = function(e) {
            list(
              nombre_evento = evento,
              fecha_corte = yearmonth(fecha_corte),
              error = TRUE,
              mensaje_error = conditionMessage(e)
            )
          }
        )
      } else {
        seleccionar_regdin_evento_6m_9_10(
          datos_completos = datos_completos,
          fecha_corte = fecha_corte,
          nombre_evento = evento,
          variable = variable,
          fecha = fecha,
          regresores = regresores,
          lags_regresores = lags_regresores,
          min_regresores = min_regresores,
          max_regresores = max_regresores,
          incluir_modelo_sin_regresores = incluir_modelo_sin_regresores,
          m = m,
          alpha = alpha,
          ljung_lag = ljung_lag,
          n_top_errores = n_top_errores,
          n_top_regresores = n_top_regresores,
          horizonte = horizonte,
          incluir_mes_corte = incluir_mes_corte,
          validation_size = validation_size,
          margen_rmsfe = margen_rmsfe,
          umbral_correlacion = umbral_correlacion,
          detener_si_no_estacionaria = detener_si_no_estacionaria,
          modo_prediccion_regresores = modo_prediccion_regresores
        )
      }
    }
  )

  names(resultados_eventos) <- puntos_ruptura$evento

  resultados_ok <- resultados_eventos %>%
    keep(~ is.null(.x$error) || isFALSE(.x$error))

  resultados_error <- resultados_eventos %>%
    keep(~ isTRUE(.x$error))

  tabla_resumen_eventos <- tibble()

  if (length(resultados_ok) > 0) {
    tabla_resumen_eventos <- imap_dfr(
      resultados_ok,
      function(res, evento) {
        res$mejor_modelo %>%
          mutate(
            evento = evento,
            fecha_corte = as.character(res$fecha_corte),
            fecha_inicio_test = as.character(res$fecha_inicio_test),
            fecha_fin_test = as.character(res$fecha_fin_test),
            n_train = res$n_train_evento,
            n_test = res$n_test_evento,
            nombre_mejor_modelo = res$nombre_mejor_modelo,
            modo_prediccion_regresores = res$modo_prediccion_regresores,
            n_variables_no_estacionarias = length(res$variables_no_estacionarias),
            variables_no_estacionarias = paste(res$variables_no_estacionarias, collapse = ", "),
            .before = 1
          )
      }
    )
  }

  tabla_errores_eventos <- tibble()

  if (length(resultados_error) > 0) {
    tabla_errores_eventos <- imap_dfr(
      resultados_error,
      function(res, evento) {
        tibble(
          evento = evento,
          fecha_corte = as.character(res$fecha_corte),
          mensaje_error = res$mensaje_error
        )
      }
    )
  }

  cat("\n")
  cat("============================================================\n")
  cat("RESUMEN FINAL: REGRESIÓN DINÁMICA 9/10 EN EVENTOS\n")
  cat("============================================================\n\n")

  if (nrow(tabla_resumen_eventos) > 0) {
    print(
      tabla_resumen_eventos %>%
        select(
          evento,
          fecha_corte,
          fecha_inicio_test,
          fecha_fin_test,
          n_train,
          n_test,
          nombre_mejor_modelo,
          id_error,
          regresores_texto,
          AICc,
          BIC,
          RMSFE,
          MAE,
          MASE,
          ljung_p_value,
          diagnostico_ljung,
          modo_prediccion_regresores,
          n_variables_no_estacionarias,
          variables_no_estacionarias
        ),
      n = Inf,
      width = Inf
    )
  }

  if (nrow(tabla_errores_eventos) > 0) {
    cat("\nEventos con error:\n")
    print(tabla_errores_eventos, n = Inf, width = Inf)
  }

  cat("\n")
  cat("Nota metodológica: los RMSFE/MAE mostrados son de test limpio, porque el modelo fue elegido\n")
  cat("con validación interna previa dentro del train, no con los 6 meses del evento.\n")
  cat("============================================================\n\n")

  list(
    puntos_ruptura = puntos_ruptura,
    resultados_eventos = resultados_eventos,
    resultados_ok = resultados_ok,
    resultados_error = resultados_error,
    tabla_resumen_eventos = tabla_resumen_eventos,
    tabla_errores_eventos = tabla_errores_eventos
  )
}


# ============================================================
# 9. EJEMPLO DE USO CON TU BASE df_excepto_alim
# ============================================================
# Este bloque queda sin comentar porque replica tu forma de trabajo.
# Si prefieres usar source() sin ejecución automática, comenta desde setwd().
# ============================================================

setwd("C:/Users/rober/Desktop/UNIR/TFM/ubicacion_datos/tablasR")

df_excepto_alim <- read_excel("df_excepto_alim.xlsx")

df_excepto_alim <- df_excepto_alim %>%
  mutate(fecha_mensual = yearmonth(fecha_mensual)) %>%
  arrange(fecha_mensual) %>%
  as_tsibble(index = fecha_mensual)

regresores_excepto_alim <- c(
  "dlog_brent_dollars_per_barrel",
  "dlog_omie_precio",
  "dlog_gasolina_95",
  "dlog_diesel",
  "dlog_usd_eur",
  "dlog_neer_aprox"
)

resultados_regdin_eventos <- ejecutar_regdin_tres_eventos_6m_9_10(
  datos_completos = df_excepto_alim,
  variable = "y_ipc_general",
  fecha = "fecha_mensual",
  regresores = regresores_excepto_alim,
  lags_regresores = 1:3,
  min_regresores = 1,
  max_regresores = 3,
  incluir_modelo_sin_regresores = FALSE,
  m = 12,
  alpha = 0.05,
  ljung_lag = 24,
  n_top_errores = 3,
  n_top_regresores = 5,
  horizonte = 6,
  incluir_mes_corte = FALSE,
  validation_size = 6,
  margen_rmsfe = 0.10,
  umbral_correlacion = 0.95,
  detener_si_no_estacionaria = FALSE,
  continuar_si_error = FALSE,
  modo_prediccion_regresores = "ex_ante_recursivo_1_paso"
)

modelo_limpio_regdin <- resultados_regdin_eventos

# ============================================================
# 10. VISUALIZACIONES Y CONSULTA DE RESULTADOS
# ============================================================

#Tablas necesarias para la combinación de predicciones 
View(resultados_regdin_eventos$resultados_eventos$`Post-confinamiento`$comparacion_top3_ex_ante)
View(resultados_regdin_eventos$resultados_eventos$`Post-confinamiento`$comparacion_top3_ex_post)


View(resultados_regdin_eventos$tabla_resumen_eventos)

resultado_post_confinamiento <- resultados_regdin_eventos$resultados_eventos$`Post-confinamiento`
resultado_repunte_2021 <- resultados_regdin_eventos$resultados_eventos$`Repunte inflacionario de 2021`
resultado_ucrania_2022 <- resultados_regdin_eventos$resultados_eventos$`Inicio invasión rusa en Ucrania`

# Diagnóstico de estacionariedad por evento
View(resultado_post_confinamiento$diagnostico_estacionariedad)
View(resultado_repunte_2021$diagnostico_estacionariedad)
View(resultado_ucrania_2022$diagnostico_estacionariedad)

# Gráficos de predicción vs real
resultado_post_confinamiento$grafico_pred_real_evento
resultado_repunte_2021$grafico_pred_real_evento
resultado_ucrania_2022$grafico_pred_real_evento

# Gráficos de error
resultado_post_confinamiento$grafico_error_evento
resultado_repunte_2021$grafico_error_evento
resultado_ucrania_2022$grafico_error_evento

# Tablas predicción vs real de cada episodio
resultado_post_confinamiento$comparacion_pred_real
resultado_repunte_2021$comparacion_pred_real
resultado_ucrania_2022$comparacion_pred_real

# Tablas finales de test limpio
View(resultado_post_confinamiento$tabla_final)
View(resultado_repunte_2021$tabla_final)
View(resultado_ucrania_2022$tabla_final)

# Tablas de selección en validación interna
View(resultado_post_confinamiento$tabla_final_validacion)
View(resultado_repunte_2021$tabla_final_validacion)
View(resultado_ucrania_2022$tabla_final_validacion)

# Modelo elegido por validación y evaluado en test limpio
resultado_post_confinamiento$mejor_modelo
resultado_repunte_2021$mejor_modelo
resultado_ucrania_2022$mejor_modelo

# Errores de ejecución, si los hubiera
resultados_regdin_eventos$tabla_errores_eventos

resultado_post_confinamiento$grafico_pred_real_event
resultado_repunte_2021$grafico_pred_real_event
resultado_ucrania_2022$grafico_pred_real_event

library(writexl)
ruta_salida <- "C:/Users/rober/Desktop/UNIR/TFM/proyecto/tablas_resultados"
write_xlsx(resultado_ucrania_2022$tabla_final, file.path(ruta_salida, "tab_16.xlsx"))

pred_2020_regdin_exante<-resultados_regdin_eventos$resultados_eventos$`Post-confinamiento`$comparacion_top3_ex_ante
pred_2020_regdin_expost<-resultados_regdin_eventos$resultados_eventos$`Post-confinamiento`$comparacion_top3_ex_post

pred_2021_regdin_exante<-resultados_regdin_eventos$resultados_eventos$`Repunte inflacionario de 2021`$comparacion_top3_ex_ante
pred_2021_regdin_expost<-resultados_regdin_eventos$resultados_eventos$`Repunte inflacionario de 2021`$comparacion_top3_ex_post

pred_2022_regdin_exante<-resultados_regdin_eventos$resultados_eventos$`Inicio invasión rusa en Ucrania`$comparacion_top3_ex_ante
pred_2022_regdin_expost<-resultados_regdin_eventos$resultados_eventos$`Inicio invasión rusa en Ucrania`$comparacion_top3_ex_post


library(writexl)
ruta_salida <- "C:/Users/rober/Desktop/UNIR/TFM/ubicacion_datos/comb_preds"
write_xlsx(pred_2020_regdin_expost, file.path(ruta_salida, "pred_2020_regdin_expost.xlsx"))


#Tablas de métricas ex ante y ex post
View(resultados_regdin_eventos$resultados_eventos$`Post-confinamiento`$metricas_top3_ex_ante_test)
View(resultados_regdin_eventos$resultados_eventos$`Post-confinamiento`$metricas_top3_ex_post)

met_regdin_2020_exante<-resultados_regdin_eventos$resultados_eventos$`Post-confinamiento`$metricas_top3_ex_ante_test 
met_regdin_2020_expost<-resultados_regdin_eventos$resultados_eventos$`Post-confinamiento`$metricas_top3_ex_post

met_regdin_2021_exante<-resultados_regdin_eventos$resultados_eventos$`Repunte inflacionario de 2021`$metricas_top3_ex_ante_test 
met_regdin_2021_expost<-resultados_regdin_eventos$resultados_eventos$`Repunte inflacionario de 2021`$metricas_top3_ex_post

met_regdin_2022_exante<-resultados_regdin_eventos$resultados_eventos$`Inicio invasión rusa en Ucrania`$metricas_top3_ex_ante_test 
met_regdin_2022_expost<-resultados_regdin_eventos$resultados_eventos$`Inicio invasión rusa en Ucrania`$metricas_top3_ex_post


library(writexl)
ruta_salida <- "C:/Users/rober/Desktop/UNIR/TFM/proyecto/tablas_resultados_seg_version"
write_xlsx(met_regdin_2020_expost, file.path(ruta_salida, "met_regdin_2020_post.xlsx"))

#Tabla expost con todos los modelos 
regdin_2020_expost_completa<-resultados_regdin_eventos$resultados_eventos$`Post-confinamiento`$tabla_final
regdin_2021_expost_completa<-resultados_regdin_eventos$resultados_eventos$`Repunte inflacionario de 2021`$tabla_final
regdin_2022_expost_completa<-resultados_regdin_eventos$resultados_eventos$`Inicio invasión rusa en Ucrania`$tabla_final

library(writexl)
ruta_salida <- "C:/Users/rober/Desktop/UNIR/TFM/ubicacion_datos/tablas_para_visualizaciones"
write_xlsx(regdin_2022_expost_completa, file.path(ruta_salida, "regdin_2022_expost_completa.xlsx"))


#Tabla exante durante el periodo de test, completa 
regdin_2020_exante_completa <- resultados_regdin_eventos$resultados_eventos$`Post-confinamiento`$tabla_final_test_ex_ante
regdin_2021_exante_completa <- resultados_regdin_eventos$resultados_eventos$`Repunte inflacionario de 2021`$tabla_final_test_ex_ante
regdin_2022_exante_completa <- resultados_regdin_eventos$resultados_eventos$`Inicio invasión rusa en Ucrania`$tabla_final_test_ex_ante

library(writexl)

ruta_salida <- "C:/Users/rober/Desktop/UNIR/TFM/ubicacion_datos/tablas_para_visualizaciones"

write_xlsx(
  regdin_2020_exante_completa,
  file.path(ruta_salida, "regdin_2020_exante_completa.xlsx")
)

write_xlsx(
  regdin_2021_exante_completa,
  file.path(ruta_salida, "regdin_2021_exante_completa.xlsx")
)

write_xlsx(
  regdin_2022_exante_completa,
  file.path(ruta_salida, "regdin_2022_exante_completa.xlsx")
)