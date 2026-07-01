# ============================================================
# PIPELINE AUTOMÁTICO PARA SELECCIÓN ETS
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

  columna_fecha_pred <- setdiff(
    names(predicciones_vs_real),
    c(".model", "prediccion", "real", "error", "abs_error", "sq_error")
  )[[1]]

  errores_ancho <- predicciones_vs_real %>%
    select(all_of(columna_fecha_pred), .model, error) %>%
    tidyr::pivot_wider(
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

    perdidas_ref <- e_ref^2
    perdidas_mod <- e_mod^2
    diferencial <- perdidas_mod - perdidas_ref
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
    purrr::map_dfr(calcular_dm_aproximado)

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

  # Selección jerárquica:
  # 1) Solo candidatos con RMSFE dentro del margen del mejor.
  # 2) Dentro de ellos, se prefiere MAE menor.
  # 3) Luego residuos mejores.
  # 4) Luego AICc menor.
  # 5) Luego RMSFE menor como cierre.
  mejor_modelo_limpio <- candidatos %>%
    arrange(
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
# FUNCIÓN PRINCIPAL: SELECCIÓN AUTOMÁTICA ETS
# ============================================================

seleccionar_ets_automatico <- function(train_data,
                                       test_data,
                                       variable = "y_ipc_general",
                                       fecha = "fecha_mensual",
                                       m = 12,
                                       alpha = 0.05,
                                       ljung_lag = 24) { #nº rezagos autocorrelación resid
  
  # ----------------------------------------------------------
  # 1. Preparación básica
  # ----------------------------------------------------------
  
  #se asegura el orden de datos de train y test según la fecha
  
  train_data <- train_data %>%
    arrange(.data[[fecha]]) 
  
  test_data <- test_data %>%
    arrange(.data[[fecha]])
  
  y_train <- train_data[[variable]]
  
  serie_positiva <- all(y_train > 0, na.rm = TRUE) #para modelos multiplicativos
  #se necesita que los valores no sean 0 o negativos
  
  # ----------------------------------------------------------
  # 2. EDA automático básico
  # ----------------------------------------------------------
  
  grafico_serie <- train_data %>% #se guarda la grafica de la serie
    autoplot(.data[[variable]]) +
    labs(
      title = "Serie de entrenamiento",
      x = "Fecha",
      y = variable
    )
  
  grafico_tendencia <- train_data %>% #grafico donde se resalta la tendencia 
    ggplot(aes(x = .data[[fecha]], y = .data[[variable]])) +
    geom_line() +
    geom_smooth(method = "loess", se = FALSE) +
    labs(
      title = "Evaluación visual de tendencia",
      x = "Fecha",
      y = variable
    )
  
  grafico_acf <- train_data %>% #gráfico de la función de autocorrelación. 
    ACF(.data[[variable]]) %>%
    autoplot() +
    labs(
      title = "ACF de la serie de entrenamiento",
      x = "Rezago",
      y = "Autocorrelación"
    )
  
  grafico_pacf <- train_data %>% #gráfico de la función de autocorrelación parcial
    PACF(.data[[variable]]) %>%
    autoplot() +
    labs(
      title = "PACF de la serie de entrenamiento",
      x = "Rezago",
      y = "Autocorrelación parcial"
    )
  
  # ----------------------------------------------------------
  # 3. Crear modelos ETS
  # ----------------------------------------------------------
  # error:
  #   A = error aditivo
  #   M = error multiplicativo
  #
  # trend:
  #   N  = sin tendencia
  #   A  = tendencia aditiva
  #   Ad = tendencia aditiva amortiguada
  #
  # season:
  #   N = sin estacionalidad
  #   A = estacionalidad aditiva
  #   M = estacionalidad multiplicativa
  #
  # Los modelos con componentes multiplicativos solo son válidos
  # si la serie toma valores estrictamente positivos.
  # ----------------------------------------------------------
  
  grid_modelos <- crossing(
    error = c("A", "M"),
    trend = c("N", "A", "Ad"),
    season = c("N", "A", "M")
  ) %>%
    filter(
      serie_positiva | (error != "M" & season != "M")
    ) %>% #se excluyen modelos con error y estacionalidad multiplicativos, puesto que
    #la serie no es estrictamente positiva, y es un requisito para poder ajustar este 
    #tipo de modelos
    mutate(
      nombre_modelo = paste0("ETS_", error, "_", trend, "_", season), #creación de los
      #nombres en la columna nombre_modelo
      
      n_parametros = case_when( #Se guarda nº param's para test de Ljung Box 
        trend == "N"  & season == "N" ~ 1,
        trend != "N"  & season == "N" ~ 2,
        trend == "Ad" & season == "N" ~ 3,
        trend == "N"  & season != "N" ~ 2,
        trend != "N"  & season != "N" ~ 3,
        trend == "Ad" & season != "N" ~ 4,
        TRUE ~ 1
      )
    )
  
  # ----------------------------------------------------------
  # 4. Ajuste automático de todos los modelos ETS
  # ----------------------------------------------------------
  
  modelos_ets_lista <- grid_modelos %>%
    mutate( #se añade la columna ajuste
      ajuste = pmap( #para todos los elementos de la lista
        list(nombre_modelo, error, trend, season),
        function(nombre_modelo, error, trend, season) {
          
          formula_modelo <- as.formula( #sintaxis de cada modelo
            paste0(
              variable,
              " ~ error(\"", error, "\") + ",
              "trend(\"", trend, "\") + ",
              "season(\"", season, "\", period = ", m, ")"
            )
          )
          
          tryCatch(
            {
              mod <- train_data %>%
                model(modelo_tmp = ETS(formula_modelo)) #vble con modelos entrenados
              
              names(mod)[names(mod) == "modelo_tmp"] <- nombre_modelo #se asigna el nombre
              
              mod #la función devuelve la vble con todos los modelos
            },
            error = function(e) { #En caso de error devuelve un valor nulo 
              NULL
            }
          )
        }
      )
    )
  
  ajustes_validos <- modelos_ets_lista %>%
    pull(ajuste) %>%
    compact() #se toma una lista de todos los modelos aue se han ajustado correctamente
  
  if (length(ajustes_validos) == 0) {
    stop("Ningún modelo ETS pudo estimarse correctamente para esta ventana.")
  } #En caso de que ningún modelo se haya ajustado correctamente la función hace una parada
  
  modelos_ets <- ajustes_validos %>%
    reduce(bind_cols)
  
  # ----------------------------------------------------------
  # 5. Tabla de bondad de ajuste: AICc y BIC
  # ----------------------------------------------------------
  
  tabla_ajuste <- modelos_ets %>%
    glance() %>%
    select(.model, AICc, BIC) %>%
    filter(
      !is.na(AICc),
      !is.na(BIC),
      is.finite(AICc),
      is.finite(BIC)
    ) %>%
    arrange(AICc) %>%
    mutate(
      ranking_AICc = row_number()
    )
  
  modelos_validos <- tabla_ajuste %>%
    pull(.model)
  
  # ----------------------------------------------------------
  # 6. Test de Ljung-Box para todos los modelos
  # ----------------------------------------------------------
  
  residuos_modelos <- modelos_ets %>%
    select(all_of(modelos_validos)) %>%
    augment()
  
  tabla_ljung <- residuos_modelos %>%
    as_tibble() %>%
    left_join(
      grid_modelos %>%
        select(nombre_modelo, n_parametros),
      by = c(".model" = "nombre_modelo")
    ) %>%
    group_by(.model, n_parametros) %>%
    summarise(
      n_residuos_validos = sum(!is.na(.resid)),
      
      ljung_p_value = {
        residuos_validos <- na.omit(.resid)
        
        if (length(residuos_validos) > ljung_lag) {
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
  # 7. Forecasts para todos los modelos
  # ----------------------------------------------------------
  
  modelos_ordenados_aicc <- tabla_ajuste %>%
    arrange(AICc) %>%
    pull(.model)
  
  forecasts_ets <- modelos_ets %>%
    select(all_of(modelos_ordenados_aicc)) %>%
    forecast(new_data = test_data)
  
  # ----------------------------------------------------------
  # 8. Métricas predictivas
  # ----------------------------------------------------------
  
  predicciones_vs_real <- forecasts_ets %>%
    as_tibble() %>%
    select(
      all_of(fecha),
      .model,
      prediccion = .mean
    ) %>%
    left_join(
      test_data %>%
        as_tibble() %>%
        select(
          all_of(fecha),
          real = all_of(variable)
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
  
  y_train_num <- as.numeric(train_data[[variable]])
  
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
    arrange(RMSFE, MAE, MASE) %>%
    mutate(
      ranking_RMSFE = row_number(),
      ranking_MAE = rank(MAE, ties.method = "first"),
      ranking_MASE = rank(MASE, ties.method = "first")
    )
  
  # ----------------------------------------------------------
  # 9. Tabla conjunta: ajuste + residuos + predicción
  # ----------------------------------------------------------
  
  tabla_final <- tabla_ajuste %>%
    left_join(tabla_ljung, by = ".model") %>%
    inner_join(tabla_metricas, by = ".model") %>%
    mutate(
      # Se conserva el nombre puntuacion_total por compatibilidad con
      # scripts anteriores, pero ya NO es el criterio principal.
      # Ahora representa una puntuación descriptiva basada en errores.
      puntuacion_total = ranking_RMSFE + ranking_MAE
    ) %>%
    arrange(ranking_RMSFE, ranking_MAE, ranking_AICc, BIC)
  
  if (nrow(tabla_final) == 0) {
    stop("No se ha podido construir tabla_final: ningún modelo tiene métricas predictivas válidas.")
  }
  
  # ----------------------------------------------------------
  # 10. Selección automática del mejor modelo: modelo_limpio
  # ----------------------------------------------------------
  
  seleccion_modelo_limpio <- aplicar_criterio_modelo_limpio(
    tabla_final = tabla_final,
    predicciones_vs_real = predicciones_vs_real,
    alpha = alpha,
    margen_rmsfe = 0.10,
    min_obs_dm = 10
  )
  
  tabla_final <- seleccion_modelo_limpio$tabla_final
  mejor_modelo <- seleccion_modelo_limpio$mejor_modelo
  nombre_mejor_modelo <- seleccion_modelo_limpio$nombre_mejor_modelo
  modelo_limpio <- mejor_modelo
  
  if (nrow(mejor_modelo) == 0) {
    stop("No se ha seleccionado ningún modelo final. Revisa tabla_final y tabla_metricas.")
  }
  
  # ----------------------------------------------------------
  # 11. Tabla de predicción vs valor real del mejor modelo
  # ----------------------------------------------------------
  
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
  # 12. Resumen en pantalla
  # ----------------------------------------------------------
  
  cat("\n")
  cat("============================================================\n")
  cat("RESUMEN DEL PIPELINE ETS AUTOMÁTICO\n")
  cat("============================================================\n\n")
  
  cat("Variable analizada:", variable, "\n")
  cat("Frecuencia estacional m:", m, "\n")
  cat("Serie estrictamente positiva:", serie_positiva, "\n\n")
  
  cat("------------------------------------------------------------\n")
  cat("1. Número de modelos ETS considerados\n")
  cat("------------------------------------------------------------\n")
  cat("Modelos teóricos en la parrilla:", nrow(grid_modelos), "\n")
  cat("Modelos estimados correctamente:", length(modelos_validos), "\n\n")
  
  cat("------------------------------------------------------------\n")
  cat("2. Mejores modelos por bondad de ajuste, menor AICc\n")
  cat("------------------------------------------------------------\n")
  print(
    tabla_ajuste %>%
      select(.model, AICc, BIC, ranking_AICc) %>%
      slice_head(n = 10)
  )
  
  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("3. Diagnóstico Ljung-Box de residuos\n")
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
  cat("4. Mejores modelos por precisión predictiva\n")
  cat("------------------------------------------------------------\n")
  print(
    tabla_metricas %>%
      select(.model, RMSFE, MAE, MASE, ranking_RMSFE, ranking_MAE, ranking_MASE) %>%
      slice_head(n = 10)
  )
  
  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("5. Ranking final con criterio modelo_limpio\n")
  cat("------------------------------------------------------------\n")
  cat("Criterio aplicado:\n")
  cat("  1) RMSFE define candidatos dentro del 10% del menor RMSFE.\n")
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
  cat("6. Modelo seleccionado automáticamente\n")
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
  cat("7. Top 3 modelos según criterio modelo_limpio\n")
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
  
  # ----------------------------------------------------------
  # 13. Salida final
  # ----------------------------------------------------------
  
  list(
    grid_modelos = grid_modelos,
    modelos_ets = modelos_ets,
    tabla_ajuste = tabla_ajuste,
    tabla_ljung = tabla_ljung,
    forecasts_ets = forecasts_ets,
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
# FUNCIÓN AUXILIAR: CREAR TRAIN/TEST PARA UN EVENTO
# ============================================================
# Lógica por defecto:
#   - Si fecha_corte = "2021 Mar":
#       train = todos los datos reales anteriores a 2021 Mar
#               es decir, hasta 2021 Feb.
#       test  = seis meses posteriores a 2021 Mar
#               es decir, desde 2021 Apr hasta 2021 Sep.
#
# Si se desea incluir el propio mes de corte dentro del test,
# puede usarse incluir_mes_corte = TRUE.
# En ese caso:
#   - fecha_corte = "2021 Mar"
#   - test = 2021 Mar, 2021 Apr, ..., 2021 Aug.
# ============================================================

crear_train_test_evento_ets <- function(datos_completos,
                                        fecha_corte,
                                        fecha = "fecha_mensual",
                                        horizonte = 6,
                                        incluir_mes_corte = FALSE) {
  
  datos_completos <- datos_completos %>%
    arrange(.data[[fecha]])
  
  fecha_corte <- yearmonth(fecha_corte)
  
  if (incluir_mes_corte) {
    fecha_inicio_test <- fecha_corte
    fecha_fin_test <- fecha_corte + horizonte - 1
  } else {
    fecha_inicio_test <- fecha_corte + 1
    fecha_fin_test <- fecha_corte + horizonte
  }
  
  train_data <- datos_completos %>%
    filter(.data[[fecha]] < fecha_corte)
  
  test_data <- datos_completos %>%
    filter(
      .data[[fecha]] >= fecha_inicio_test,
      .data[[fecha]] <= fecha_fin_test
    )
  
  if (nrow(train_data) == 0) {
    stop("El conjunto de entrenamiento está vacío. Revisa fecha_corte.")
  }
  
  if (nrow(test_data) == 0) {
    stop("El conjunto de test está vacío. Revisa fecha_corte y la cobertura temporal de los datos.")
  }
  
  if (nrow(test_data) < horizonte) {
    warning(
      paste0(
        "La ventana de test tiene solo ", nrow(test_data),
        " observaciones, aunque se esperaban ", horizonte, "."
      )
    )
  }
  
  list(
    fecha_corte = fecha_corte,
    fecha_inicio_test = fecha_inicio_test,
    fecha_fin_test = fecha_fin_test,
    horizonte = horizonte,
    incluir_mes_corte = incluir_mes_corte,
    train_data = train_data,
    test_data = test_data
  )
}


# ============================================================
# FUNCIÓN PRINCIPAL POR EVENTO: SELECCIÓN ETS EN VENTANA 6M
# ============================================================
# Esta función NO mantiene fijo el entrenamiento.
# Para cada fecha de ruptura:
#   1. Construye un nuevo train con todos los datos reales anteriores.
#   2. Construye un nuevo test con los 6 meses posteriores.
#   3. Ejecuta seleccionar_ets_automatico().
#   4. Añade metadatos del evento y de la ventana temporal.
# ============================================================

seleccionar_ets_evento_6m <- function(datos_completos,
                                      fecha_corte,
                                      nombre_evento = NULL,
                                      variable = "y_ipc_general",
                                      fecha = "fecha_mensual",
                                      m = 12,
                                      alpha = 0.05,
                                      ljung_lag = 24,
                                      horizonte = 6,
                                      incluir_mes_corte = FALSE) {
  
  ventana <- crear_train_test_evento_ets(
    datos_completos = datos_completos,
    fecha_corte = fecha_corte,
    fecha = fecha,
    horizonte = horizonte,
    incluir_mes_corte = incluir_mes_corte
  )
  
  cat("\n")
  cat("============================================================\n")
  cat("SELECCIÓN ETS PARA EVENTO\n")
  cat("============================================================\n")
  cat("Evento:", ifelse(is.null(nombre_evento), "sin_nombre", nombre_evento), "\n")
  cat("Fecha de corte:", as.character(ventana$fecha_corte), "\n")
  cat("Train: hasta", as.character(max(ventana$train_data[[fecha]], na.rm = TRUE)), "\n")
  cat("Test:", as.character(ventana$fecha_inicio_test), "a", as.character(ventana$fecha_fin_test), "\n")
  cat("Número de observaciones train:", nrow(ventana$train_data), "\n")
  cat("Número de observaciones test:", nrow(ventana$test_data), "\n")
  cat("============================================================\n\n")
  
  resultado_ets <- seleccionar_ets_automatico(
    train_data = ventana$train_data,
    test_data = ventana$test_data,
    variable = variable,
    fecha = fecha,
    m = m,
    alpha = alpha,
    ljung_lag = ljung_lag
  )
  
  # Gráficos específicos con título del evento
  grafico_pred_real_evento <- resultado_ets$comparacion_pred_real %>%
    ggplot(aes(x = .data[[fecha]])) +
    geom_line(aes(y = real, colour = "Valor real"), linewidth = 1) +
    geom_line(aes(y = prediccion, colour = "Predicción"), linewidth = 1, linetype = "dashed") +
    labs(
      title = paste0(
        "Valores reales vs predicciones - ",
        ifelse(is.null(nombre_evento), "Evento", nombre_evento),
        " - ", resultado_ets$nombre_mejor_modelo
      ),
      subtitle = paste0(
        "Train hasta ", as.character(max(ventana$train_data[[fecha]], na.rm = TRUE)),
        " | Test: ", as.character(ventana$fecha_inicio_test),
        " a ", as.character(ventana$fecha_fin_test)
      ),
      x = "Fecha",
      y = variable,
      colour = ""
    )
  
  grafico_error_evento <- resultado_ets$comparacion_pred_real %>%
    ggplot(aes(x = .data[[fecha]], y = error)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line() +
    geom_point() +
    labs(
      title = paste0(
        "Error de predicción - ",
        ifelse(is.null(nombre_evento), "Evento", nombre_evento),
        " - ", resultado_ets$nombre_mejor_modelo
      ),
      subtitle = paste0(
        "Test: ", as.character(ventana$fecha_inicio_test),
        " a ", as.character(ventana$fecha_fin_test)
      ),
      x = "Fecha",
      y = "Error = real - predicción"
    )
  
  list(
    evento = nombre_evento,
    fecha_corte = ventana$fecha_corte,
    fecha_inicio_test = ventana$fecha_inicio_test,
    fecha_fin_test = ventana$fecha_fin_test,
    horizonte = horizonte,
    incluir_mes_corte = incluir_mes_corte,
    train_data = ventana$train_data,
    test_data = ventana$test_data,
    resultado_ets = resultado_ets,
    grid_modelos = resultado_ets$grid_modelos,
    modelos_ets = resultado_ets$modelos_ets,
    tabla_ajuste = resultado_ets$tabla_ajuste,
    tabla_ljung = resultado_ets$tabla_ljung,
    forecasts_ets = resultado_ets$forecasts_ets,
    tabla_metricas = resultado_ets$tabla_metricas,
    tabla_final = resultado_ets$tabla_final,
    mejor_modelo = resultado_ets$mejor_modelo,
    nombre_mejor_modelo = resultado_ets$nombre_mejor_modelo,
    comparacion_pred_real = resultado_ets$comparacion_pred_real,
    graficos_eda = resultado_ets$graficos_eda,
    grafico_pred_real = grafico_pred_real_evento,
    grafico_error = grafico_error_evento
  )
}


# ============================================================
# FUNCIÓN PARA EJECUTAR LOS TRES PUNTOS DE RUPTURA
# ============================================================
# Fechas por defecto:
#   - Post-confinamiento: marzo de 2020.
#   - Repunte inflacionario 2021: marzo de 2021.
#   - Inicio invasión rusa en Ucrania: febrero de 2022.
#
# Como incluir_mes_corte = FALSE por defecto, la evaluación empieza
# en el mes posterior al corte.
# ============================================================

ejecutar_ets_tres_eventos_6m <- function(datos_completos,
                                        variable = "y_ipc_general",
                                        fecha = "fecha_mensual",
                                        m = 12,
                                        alpha = 0.05,
                                        ljung_lag = 24,
                                        horizonte = 6,
                                        incluir_mes_corte = FALSE,
                                        puntos_ruptura = NULL) {
  
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
  
  resultados_eventos <- puntos_ruptura %>%
    mutate(
      resultado = map2(
        evento,
        fecha_corte,
        ~ seleccionar_ets_evento_6m(
          datos_completos = datos_completos,
          fecha_corte = .y,
          nombre_evento = .x,
          variable = variable,
          fecha = fecha,
          m = m,
          alpha = alpha,
          ljung_lag = ljung_lag,
          horizonte = horizonte,
          incluir_mes_corte = incluir_mes_corte
        )
      )
    )
  
  tabla_resumen_eventos <- resultados_eventos %>%
    transmute(
      evento,
      fecha_corte,
      fecha_inicio_test = map_chr(resultado, ~ as.character(.x$fecha_inicio_test)),
      fecha_fin_test = map_chr(resultado, ~ as.character(.x$fecha_fin_test)),
      n_train = map_int(resultado, ~ nrow(.x$train_data)),
      n_test = map_int(resultado, ~ nrow(.x$test_data)),
      nombre_mejor_modelo = map_chr(resultado, ~ .x$nombre_mejor_modelo),
      mejor_modelo = map(resultado, ~ .x$mejor_modelo)
    ) %>%
    unnest(mejor_modelo)
  
  predicciones_eventos <- resultados_eventos %>%
    transmute(
      evento,
      predicciones = map(resultado, ~ .x$comparacion_pred_real)
    ) %>%
    unnest(predicciones)
  
  list(
    puntos_ruptura = puntos_ruptura,
    resultados_eventos = resultados_eventos,
    tabla_resumen_eventos = tabla_resumen_eventos,
    predicciones_eventos = predicciones_eventos
  )
}


# ============================================================


# ============================================================
# EXTENSIÓN 9/10: VALIDACIÓN ROLLING EX ANTE + TEST LIMPIO
# ============================================================
# Esta sección mantiene intacto el motor original:
#   - seleccionar_ets_automatico()
#   - crear_train_test_evento_ets()
#
# La mejora metodológica consiste en cambiar el orden del procedimiento:
#   1. Se crea el train/test del evento.
#   2. Dentro del train del evento se crean varias ventanas rolling de validación.
#   3. En cada ventana se evalúa la parrilla ETS original.
#   4. Se selecciona un modelo ex ante por rendimiento medio en validación rolling.
#   5. Se reestima/evalúa ese modelo con todo el train del evento y el test limpio.
#   6. Se calcula además el mejor modelo ex post del evento, solo como referencia retrospectiva.
# ============================================================

crear_ventanas_rolling_validacion_ets <- function(train_evento,
                                                  fecha = "fecha_mensual",
                                                  horizonte_validacion = 6,
                                                  min_train_validacion = 72,
                                                  paso_validacion = 6,
                                                  max_ventanas_validacion = 8) {
  
  train_evento <- train_evento %>%
    arrange(.data[[fecha]])
  
  n_total <- nrow(train_evento)
  ultimo_fin_train_posible <- n_total - horizonte_validacion
  
  if (ultimo_fin_train_posible < min_train_validacion) {
    stop(
      paste0(
        "No hay observaciones suficientes para validación rolling. ",
        "n_total = ", n_total,
        ", min_train_validacion = ", min_train_validacion,
        ", horizonte_validacion = ", horizonte_validacion, "."
      )
    )
  }
  
  fines_train <- seq(
    from = min_train_validacion,
    to = ultimo_fin_train_posible,
    by = paso_validacion
  )
  
  if (length(fines_train) > max_ventanas_validacion) {
    fines_train <- tail(fines_train, max_ventanas_validacion)
  }
  
  ventanas <- map_dfr(
    seq_along(fines_train),
    function(i) {
      fin_train <- fines_train[[i]]
      inicio_validacion <- fin_train + 1
      fin_validacion <- fin_train + horizonte_validacion
      
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
  
  ventanas
}


agregar_validacion_rolling_ets <- function(tablas_validacion,
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
      error = first(error),
      trend = first(trend),
      season = first(season),
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
    arrange(
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
  
  list(
    tabla_validacion_modelos = tabla_validacion_modelos,
    mejor_modelo_ex_ante_validacion = mejor_modelo_ex_ante,
    nombre_mejor_modelo_ex_ante = nombre_mejor_modelo_ex_ante
  )
}


validar_rolling_ets_9_10 <- function(train_evento,
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
  
  ventanas <- crear_ventanas_rolling_validacion_ets(
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
        seleccionar_ets_automatico(
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
              error, trend, season
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
  agregacion <- agregar_validacion_rolling_ets(
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


crear_graficos_modelo_ets_9_10 <- function(comparacion_pred_real,
                                           nombre_modelo,
                                           variable = "y_ipc_general",
                                           fecha = "fecha_mensual",
                                           titulo_base = "ETS") {
  
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


seleccionar_ets_evento_6m_9_10 <- function(datos_completos,
                                           fecha_corte,
                                           nombre_evento = NULL,
                                           variable = "y_ipc_general",
                                           fecha = "fecha_mensual",
                                           m = 12,
                                           alpha = 0.05,
                                           ljung_lag = 24,
                                           horizonte = 6,
                                           incluir_mes_corte = FALSE,
                                           horizonte_validacion = 6,
                                           min_train_validacion = 72,
                                           paso_validacion = 6,
                                           max_ventanas_validacion = 8,
                                           margen_rmsfe_validacion = 0.10) {
  
  ventana <- crear_train_test_evento_ets(
    datos_completos = datos_completos,
    fecha_corte = fecha_corte,
    fecha = fecha,
    horizonte = horizonte,
    incluir_mes_corte = incluir_mes_corte
  )
  
  cat("\n")
  cat("############################################################\n")
  cat("ETS 9/10 - EVENTO:", ifelse(is.null(nombre_evento), as.character(ventana$fecha_corte), nombre_evento), "\n")
  cat("Fecha de corte:", as.character(ventana$fecha_corte), "\n")
  cat("Train del evento:", as.character(min(ventana$train_data[[fecha]])), "a", as.character(max(ventana$train_data[[fecha]])), "\n")
  cat("Test limpio:", as.character(min(ventana$test_data[[fecha]])), "a", as.character(max(ventana$test_data[[fecha]])), "\n")
  cat("############################################################\n")
  
  # ----------------------------------------------------------
  # A. Selección ex ante: solo usa datos anteriores al evento.
  # ----------------------------------------------------------
  validacion_rolling <- validar_rolling_ets_9_10(
    train_evento = ventana$train_data,
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
  # ----------------------------------------------------------
  resultado_test <- seleccionar_ets_automatico(
    train_data = ventana$train_data,
    test_data = ventana$test_data,
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
      prop_top3_RMSFE_validacion,
      rmsfe_minimo_validacion,
      margen_rmsfe_validacion,
      diferencia_relativa_RMSFE_validacion,
      error,
      trend,
      season
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
  
  graficos_ex_ante <- crear_graficos_modelo_ets_9_10(
    comparacion_pred_real = comparacion_ex_ante,
    nombre_modelo = nombre_mejor_modelo_ex_ante,
    variable = variable,
    fecha = fecha,
    titulo_base = "ETS ex ante"
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
  
  graficos_ex_post <- crear_graficos_modelo_ets_9_10(
    comparacion_pred_real = comparacion_ex_post,
    nombre_modelo = nombre_mejor_modelo_ex_post,
    variable = variable,
    fecha = fecha,
    titulo_base = "ETS ex post"
  )
  
  list(
    evento = nombre_evento,
    fecha_corte = ventana$fecha_corte,
    fecha_inicio_test = ventana$fecha_inicio_test,
    fecha_fin_test = ventana$fecha_fin_test,
    horizonte = horizonte,
    train_evento = ventana$train_data,
    test_evento = ventana$test_data,
    validacion_rolling = validacion_rolling,
    tabla_validacion_rolling = validacion_rolling$tabla_validacion_modelos,
    tabla_validacion_completa = validacion_rolling$tabla_validacion_completa,
    ventanas_validacion_rolling = validacion_rolling$ventanas_validacion_rolling,
    nombre_mejor_modelo_ex_ante = nombre_mejor_modelo_ex_ante,
    nombres_top3_modelos_ex_ante = nombres_top3_modelos_ex_ante,
    tabla_final_ex_ante_test = tabla_final_ex_ante_test,
    metricas_top3_ex_ante_test = metricas_top3_ex_ante_test,
    comparacion_top3_ex_ante = comparacion_top3_ex_ante,
    mejor_modelo_ex_ante_validacion = validacion_rolling$mejor_modelo_ex_ante_validacion,
    mejor_modelo_ex_ante_test = fila_ex_ante_test,
    comparacion_pred_real_ex_ante = comparacion_ex_ante,
    grafico_pred_real_ex_ante = graficos_ex_ante$grafico_pred_real,
    grafico_error_ex_ante = graficos_ex_ante$grafico_error,
    nombre_mejor_modelo_ex_post = nombre_mejor_modelo_ex_post,
    mejor_modelo_ex_post = mejor_modelo_ex_post,
    tabla_final_ex_post = resultado_test$tabla_final,
    comparacion_pred_real_ex_post = comparacion_ex_post,
    nombres_top3_modelos_ex_post = nombres_top3_modelos_ex_post,
    metricas_top3_ex_post = metricas_top3_ex_post,
    comparacion_top3_ex_post = comparacion_top3_ex_post,
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


ejecutar_ets_tres_eventos_6m_9_10 <- function(datos_completos,
                                             variable = "y_ipc_general",
                                             fecha = "fecha_mensual",
                                             m = 12,
                                             alpha = 0.05,
                                             ljung_lag = 24,
                                             horizonte = 6,
                                             incluir_mes_corte = FALSE,
                                             puntos_ruptura = NULL,
                                             horizonte_validacion = 6,
                                             min_train_validacion = 72,
                                             paso_validacion = 6,
                                             max_ventanas_validacion = 8,
                                             margen_rmsfe_validacion = 0.10,
                                             continuar_si_error = TRUE) {
  
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
  
  resultados <- vector("list", nrow(puntos_ruptura))
  errores <- list()
  
  for (i in seq_len(nrow(puntos_ruptura))) {
    evento_i <- puntos_ruptura$evento[[i]]
    fecha_corte_i <- puntos_ruptura$fecha_corte[[i]]
    
    resultado_i <- tryCatch(
      {
        seleccionar_ets_evento_6m_9_10(
          datos_completos = datos_completos,
          fecha_corte = fecha_corte_i,
          nombre_evento = evento_i,
          variable = variable,
          fecha = fecha,
          m = m,
          alpha = alpha,
          ljung_lag = ljung_lag,
          horizonte = horizonte,
          incluir_mes_corte = incluir_mes_corte,
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
  
  nombres_eventos <- puntos_ruptura$evento
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
# EJEMPLO DE USO ETS 9/10
# ============================================================
 setwd("C:/Users/rober/Desktop/UNIR/TFM/ubicacion_datos/tablasR")

 df_ipc_m <- read_excel("df_ipc_m.xlsx") %>%
   mutate(fecha_mensual = yearmonth(fecha_mensual)) %>%
   arrange(fecha_mensual) %>%
   as_tsibble(index = fecha_mensual)

 resultados_ets_9_10 <- ejecutar_ets_tres_eventos_6m_9_10(
   datos_completos = df_ipc_m,
   variable = "y_ipc_general",
   fecha = "fecha_mensual",
   m = 12,
   alpha = 0.05,
   ljung_lag = 24,
   horizonte = 6,
   incluir_mes_corte = FALSE,
   horizonte_validacion = 6,
   min_train_validacion = 72,
   paso_validacion = 6,
   max_ventanas_validacion = 8
 )

 #Tablas necesarias para la combinación de predicciones 
 View(resultados_ets_9_10$resultados_eventos$`Post-confinamiento`$comparacion_top3_ex_ante)
 View(resultados_ets_9_10$resultados_eventos$`Post-confinamiento`$comparacion_top3_ex_post)
 
 
 View(resultados_ets_9_10$tabla_resumen_eventos)
 resultado_post <- resultados_ets_9_10$resultados_eventos$`Post-confinamiento`
 View(resultado_post$mejor_modelo_ex_ante_test)
 View(resultado_post$tabla_validacion_rolling)
 resultado_post$grafico_pred_real_ex_ante
 resultado_post$grafico_pred_real_ex_post

 resultado_rep <- resultados_ets_9_10$resultados_eventos$`Repunte inflacionario de 2021`
 resultado_ucr <- resultados_ets_9_10$resultados_eventos$`Inicio invasión rusa en Ucrania`
 
 tab_val_post <- resultado_post$tabla_validacion_rolling
 tab_val_rep <- resultado_rep$tabla_validacion_rolling
 tab_val_ucr <- resultado_ucr$tabla_validacion_rolling
 
 tab_expost_post <- resultado_post$tabla_final_ex_post
 tab_expost_rep <- resultado_rep$tabla_final_ex_post
 tab_expost_ucr <- resultado_ucr$tabla_final_ex_post
 
 View(resultado_post$tabla_validacion_completa)
 View(resultado_rep$tabla_validacion_completa)
 View(resultado_ucr$tabla_validacion_completa)
 View(tab_expost_post)
 View(tab_expost_rep)
 View(tab_expost_ucr)
 
library(writexl)
ruta_salida <- "C:/Users/rober/Desktop/UNIR/TFM/proyecto/tablas_resultados"
write_xlsx(tab_expost_ucr, file.path(ruta_salida, "tab_10.xlsx"))
 
pred_2020_ets_exante<-resultados_ets_9_10$resultados_eventos$`Post-confinamiento`$comparacion_top3_ex_ante
pred_2020_ets_expost<-resultados_ets_9_10$resultados_eventos$`Post-confinamiento`$comparacion_top3_ex_post
 
pred_2021_ets_exante<-resultados_ets_9_10$resultados_eventos$`Repunte inflacionario de 2021`$comparacion_top3_ex_ante
pred_2021_ets_expost<-resultados_ets_9_10$resultados_eventos$`Repunte inflacionario de 2021`$comparacion_top3_ex_post
 
pred_2022_ets_exante<-resultados_ets_9_10$resultados_eventos$`Inicio invasión rusa en Ucrania`$comparacion_top3_ex_ante
pred_2022_ets_expost<-resultados_ets_9_10$resultados_eventos$`Inicio invasión rusa en Ucrania`$comparacion_top3_ex_post
 
 
library(writexl)
ruta_salida <- "C:/Users/rober/Desktop/UNIR/TFM/ubicacion_datos/comb_preds"
write_xlsx(pred_2020_ets_exante, file.path(ruta_salida, "pred_2020_ets_exante.xlsx"))

#Tablas de métricas para la sección de resultados empíricos

#Tablas de métricas ex ante y ex post
View(resultados_ets_9_10$resultados_eventos$`Post-confinamiento`$metricas_top3_ex_ante_test)
View(resultados_ets_9_10$resultados_eventos$`Post-confinamiento`$metricas_top3_ex_post)

met_ets_2020_exante<-resultados_ets_9_10$resultados_eventos$`Post-confinamiento`$metricas_top3_ex_ante_test 
met_ets_2020_expost<-resultados_ets_9_10$resultados_eventos$`Post-confinamiento`$metricas_top3_ex_post

met_ets_2021_exante<-resultados_ets_9_10$resultados_eventos$`Repunte inflacionario de 2021`$metricas_top3_ex_ante_test 
met_ets_2021_expost<-resultados_ets_9_10$resultados_eventos$`Repunte inflacionario de 2021`$metricas_top3_ex_post

met_ets_2022_exante<-resultados_ets_9_10$resultados_eventos$`Inicio invasión rusa en Ucrania`$metricas_top3_ex_ante_test 
met_ets_2022_expost<-resultados_ets_9_10$resultados_eventos$`Inicio invasión rusa en Ucrania`$metricas_top3_ex_post


library(writexl)
ruta_salida <- "C:/Users/rober/Desktop/UNIR/TFM/proyecto/tablas_resultados_seg_version"
write_xlsx(met_ets_2020_exante, file.path(ruta_salida, "met_ets_2020_exante.xlsx"))


#Tabla expost con todos los modelos 
ets_2020_expost_completa<-resultados_ets_9_10$resultados_eventos$`Post-confinamiento`$tabla_final_ex_post
ets_2021_expost_completa<-resultados_ets_9_10$resultados_eventos$`Repunte inflacionario de 2021`$tabla_final_ex_post
ets_2022_expost_completa<-resultados_ets_9_10$resultados_eventos$`Inicio invasión rusa en Ucrania`$tabla_final_ex_post

library(writexl)
ruta_salida <- "C:/Users/rober/Desktop/UNIR/TFM/ubicacion_datos/tablas_para_visualizaciones"
write_xlsx(ets_2020_expost_completa, file.path(ruta_salida, "ets_2020_expost_completa.xlsx"))


#Tabla exante_test con todos los modelos
ets_2020_exante_completa <- resultados_ets_9_10$resultados_eventos$`Post-confinamiento`$tabla_final_ex_ante_test
ets_2021_exante_completa <- resultados_ets_9_10$resultados_eventos$`Repunte inflacionario de 2021`$tabla_final_ex_ante_test
ets_2022_exante_completa <- resultados_ets_9_10$resultados_eventos$`Inicio invasión rusa en Ucrania`$tabla_final_ex_ante_test

library(writexl)
ruta_salida <- "C:/Users/rober/Desktop/UNIR/TFM/ubicacion_datos/tablas_para_visualizaciones"

write_xlsx(
  ets_2020_exante_completa,
  file.path(ruta_salida, "ets_2020_exante_test_completa.xlsx")
)

write_xlsx(
  ets_2021_exante_completa,
  file.path(ruta_salida, "ets_2021_exante_test_completa.xlsx")
)

write_xlsx(
  ets_2022_exante_completa,
  file.path(ruta_salida, "ets_2022_exante_test_completa.xlsx")
)