# ============================================================
# MODELOS MIDAS RESTRINGIDOS: EX POST Y EX ANTE 2 MESES
# ============================================================
# Objetivo:
#   Implementar modelos MIDAS restringidos con pesos nealmon
#   para predecir la tasa logarítmica mensual del IPC general.
#
# Idea del script:
#   1. Se parte de una variable objetivo mensual y varios regresores
#      de frecuencia superior: semanal, diaria o mensual.
#   2. Se prueban combinaciones de hasta 4 regresores entre 6 candidatos.
#      Con 6 regresores y tamaño máximo 4 se obtienen 56 modelos.
#   3. Para cada modelo se generan predicciones de 2 meses.
#   4. Cada mes se actualiza por semanas: semana 1, 2, 3 y 4.
#      Por tanto, el horizonte completo tiene 8 actualizaciones.
#      Las métricas se calculan sobre esas 8 actualizaciones.
#   5. Se ofrecen dos procedimientos:
#      - Ex post: selección mirando el error total de las 8 predicciones.
#      - Ex ante: selección mediante 4 ventanas de validación anteriores
#        a una fecha de ruptura, promediando las métricas.
#
# Nota:
#   Este script está pensado como base sencilla y orgánica. No intenta
#   resolver todos los detalles posibles de un pipeline final, sino dejar
#   clara la lógica MIDAS esencial.
# ============================================================

# install.packages(c("tidyverse", "lubridate", "midasr"))
library(tidyverse)
library(lubridate)
library(midasr)
library(readxl)
library(dplyr)
library(tsibble)
library(feasts)
library(rlang)
library(fable)
library(fabletools)


# ============================================================
# 2. DIAGNÓSTICO DE ESTACIONARIEDAD MIDAS
# ============================================================
# Unión metodológica con el script de regresión dinámica.
#
# Qué hace:
#   Comprueba estacionariedad de la variable objetivo mensual y de los
#   regresores de alta frecuencia que entran en el objeto MIDAS.
#
#   No transforma automáticamente las series, porque en este proyecto las
#   variables ya deberían llegar como tasas logarítmicas. La función sirve
#   para documentar, auditar y, si se desea, detener la ejecución cuando
#   alguna variable parezca no estacionaria.
#
# Qué devuelve:
#   Una tabla con KPSS, ndiffs, Phillips-Perron, estadísticos básicos y
#   diagnóstico final por variable.
# ============================================================

comprobar_estacionariedad_midas <- function(objeto_midas,
                                            regresores = NULL,
                                            n_meses_train = NULL,
                                            alpha = 0.05,
                                            detener_si_no_estacionaria = FALSE) {

  data_midas <- objeto_midas$data_midas
  frecuencias <- objeto_midas$frecuencias

  if (is.null(regresores)) {
    regresores <- setdiff(names(data_midas), "y")
  }

  regresores <- regresores[regresores %in% names(data_midas)]

  if (is.null(n_meses_train)) {
    n_meses_train <- length(data_midas$y)
  }

  n_meses_train <- min(n_meses_train, length(data_midas$y))

  construir_serie_larga <- function(nombre_variable) {

    if (nombre_variable == "y") {
      valores <- as.numeric(data_midas$y[seq_len(n_meses_train)])
    } else {
      frecuencia_variable <- frecuencias[[nombre_variable]]
      n_obs <- n_meses_train * frecuencia_variable
      valores <- as.numeric(data_midas[[nombre_variable]][seq_len(min(n_obs, length(data_midas[[nombre_variable]])))])
    }

    tibble(
      variable_modelo = nombre_variable,
      indice = seq_along(valores),
      valor = valores
    )
  }

  datos_largos <- bind_rows(
    c("y", regresores) %>%
      purrr::map(construir_serie_larga)
  ) %>%
    filter(!is.na(valor), is.finite(valor))

  if (nrow(datos_largos) == 0) {
    return(tibble())
  }

  datos_ts <- datos_largos %>%
    as_tsibble(
      key = variable_modelo,
      index = indice
    )

  tabla_kpss <- tryCatch(
    datos_ts %>% features(valor, feasts::unitroot_kpss),
    error = function(e) {
      tibble(
        variable_modelo = unique(datos_largos$variable_modelo),
        kpss_stat = NA_real_,
        kpss_pvalue = NA_real_
      )
    }
  )

  tabla_ndiffs <- tryCatch(
    datos_ts %>% features(valor, feasts::unitroot_ndiffs),
    error = function(e) {
      tibble(
        variable_modelo = unique(datos_largos$variable_modelo),
        ndiffs = NA_integer_
      )
    }
  )

  tabla_pp <- tryCatch(
    datos_ts %>% features(valor, feasts::unitroot_pp),
    error = function(e) {
      tibble(
        variable_modelo = unique(datos_largos$variable_modelo),
        pp_stat = NA_real_,
        pp_pvalue = NA_real_
      )
    }
  )

  tabla_resumen_basica <- datos_largos %>%
    group_by(variable_modelo) %>%
    summarise(
      n_obs = sum(!is.na(valor)),
      media = mean(valor, na.rm = TRUE),
      sd = sd(valor, na.rm = TRUE),
      min = min(valor, na.rm = TRUE),
      max = max(valor, na.rm = TRUE),
      .groups = "drop"
    )

  diagnostico <- tabla_kpss %>%
    left_join(tabla_ndiffs, by = "variable_modelo") %>%
    left_join(tabla_pp, by = "variable_modelo") %>%
    left_join(tabla_resumen_basica, by = "variable_modelo") %>%
    mutate(
      estacionaria_kpss = case_when(
        is.na(kpss_pvalue) ~ NA,
        kpss_pvalue >= alpha ~ TRUE,
        kpss_pvalue < alpha ~ FALSE
      ),
      requiere_diferencia = case_when(
        !is.na(ndiffs) & ndiffs > 0 ~ TRUE,
        !is.na(kpss_pvalue) & kpss_pvalue < alpha ~ TRUE,
        !is.na(kpss_pvalue) & kpss_pvalue >= alpha ~ FALSE,
        TRUE ~ NA
      ),
      diagnostico_estacionariedad = case_when(
        requiere_diferencia == TRUE ~ "posible_no_estacionaria_revisar_transformacion",
        requiere_diferencia == FALSE ~ "compatible_con_estacionariedad",
        TRUE ~ "no_concluyente"
      )
    ) %>%
    arrange(desc(requiere_diferencia), variable_modelo)

  variables_no_estacionarias <- diagnostico %>%
    filter(requiere_diferencia == TRUE) %>%
    pull(variable_modelo)

  if (detener_si_no_estacionaria && length(variables_no_estacionarias) > 0) {
    stop(
      "El diagnóstico de estacionariedad detecta variables posiblemente no estacionarias: ",
      paste(variables_no_estacionarias, collapse = ", "),
      ". Revisa las transformaciones antes de estimar MIDAS."
    )
  }

  diagnostico
}


extraer_variables_no_estacionarias_midas <- function(diagnostico_estacionariedad) {

  if (is.null(diagnostico_estacionariedad) || nrow(diagnostico_estacionariedad) == 0) {
    return(character(0))
  }

  diagnostico_estacionariedad %>%
    filter(requiere_diferencia == TRUE) %>%
    pull(variable_modelo)
}


# ============================================================
# 1. FUNCIÓN: construir_objeto_midas_basico()
# ============================================================
# Qué hace:
#   Recibe la serie mensual objetivo y una lista nombrada de regresores.
#   Cada regresor debe estar ya transformado en tasa logarítmica y ordenado
#   cronológicamente. La función extrae los vectores y construye la lista
#   de datos que necesita midasr.
#
# Qué devuelve:
#   Una lista con:
#     - data_midas: lista nombrada con y y los regresores.
#     - fechas_y: fechas mensuales de la variable objetivo.
#     - variable_y: nombre de la variable objetivo.
#     - regresores: nombres de los regresores disponibles.
#     - frecuencias: número de observaciones de cada regresor por mes.
#
construir_objeto_midas_basico <- function(df_y,
                                          lista_regresores,
                                          frecuencias_regresores,
                                          variable_y = "y_ipc_general",
                                          fecha_y = "fecha_mensual",
                                          columnas_regresores = NULL) {

  if (is.null(names(lista_regresores)) || any(names(lista_regresores) == "")) {
    stop("lista_regresores debe ser una lista nombrada.")
  }

  if (is.null(names(frecuencias_regresores))) {
    stop("frecuencias_regresores debe ser un vector nombrado. Ejemplo: c(brent = 4, omie = 28).")
  }

  regresores <- names(lista_regresores)

  if (!all(regresores %in% names(frecuencias_regresores))) {
    stop("Todos los regresores deben aparecer en frecuencias_regresores.")
  }

  y_vec <- df_y %>%
    arrange(.data[[fecha_y]]) %>%
    pull(.data[[variable_y]])

  fechas_y <- df_y %>%
    arrange(.data[[fecha_y]]) %>%
    pull(.data[[fecha_y]])

  data_midas <- list(y = as.numeric(y_vec))

  for (reg in regresores) {

    df_reg <- lista_regresores[[reg]]

    if (is.null(columnas_regresores)) {
      posibles <- setdiff(names(df_reg), c("fecha", "fecha_mensual", "mes", "yearmonth"))
      columna_reg <- posibles[1]
    } else {
      columna_reg <- columnas_regresores[[reg]]
    }

    data_midas[[reg]] <- df_reg %>%
      arrange(1) %>%
      pull(.data[[columna_reg]]) %>%
      as.numeric()
  }

  n_meses <- length(data_midas$y)

  for (reg in regresores) {
    m <- frecuencias_regresores[[reg]]
    n_esperado <- n_meses * m

    if (length(data_midas[[reg]]) != n_esperado) {
      warning(paste0(
        "El regresor ", reg, " tiene ", length(data_midas[[reg]]),
        " observaciones, pero se esperaban ", n_esperado,
        " (= meses de y x frecuencia). Revisa la alineación."
      ))
    }
  }

  list(
    data_midas = data_midas,
    fechas_y = fechas_y,
    variable_y = variable_y,
    regresores = regresores,
    frecuencias = frecuencias_regresores
  )
}


# ============================================================
# 2. FUNCIÓN: crear_combinaciones_regresores()
# ============================================================
# Qué hace:
#   Crea todas las combinaciones posibles de regresores entre tamaño 1 y
#   tamaño máximo. Además, cruza esas combinaciones con las especificaciones
#   MIDAS indicadas: número de meses de retardos, inclusión o no del componente
#   autorregresivo, desplazamiento adicional de retardos y, si se indica, una
#   estructura ARMA/SARMA para corregir los residuos del modelo MIDAS.
#
# Qué devuelve:
#   Una tibble con id_modelo, regresores y especificación de cada modelo.
#
crear_combinaciones_regresores <- function(regresores,
                                           max_regresores = 4,
                                           meses_lag_midas = 3,
                                           incluir_ar = TRUE,
                                           desplazamiento_retardos = 0,
                                           errores_sarima = NULL) {

  lista_combos <- list()
  contador <- 1

  max_regresores <- min(max_regresores, length(regresores))

  if (is.null(errores_sarima)) {
    errores_sarima <- tibble(
      id_error = "SIN_SARIMA",
      especificacion_error = "sin_correccion_sarima"
    )
  }

  for (k in 1:max_regresores) {
    combos_k <- combn(regresores, k, simplify = FALSE)

    for (combo in combos_k) {
      for (lag_meses in meses_lag_midas) {
        for (ar_modelo in incluir_ar) {
          for (desfase in desplazamiento_retardos) {
            for (j in seq_len(nrow(errores_sarima))) {

              id_error_j <- errores_sarima$id_error[j]
              especificacion_error_j <- errores_sarima$especificacion_error[j]

              lista_combos[[contador]] <- tibble(
                id_modelo = paste0(
                  "MIDAS_",
                  stringr::str_pad(contador, 3, pad = "0"),
                  "_SARIMA_",
                  id_error_j
                ),
                id_error = id_error_j,
                especificacion_error = especificacion_error_j,
                n_regresores = length(combo),
                regresores_texto = paste(combo, collapse = " + "),
                regresores = list(combo),
                meses_lag_midas = lag_meses,
                incluir_ar = ar_modelo,
                desplazamiento_retardos = desfase
              )

              contador <- contador + 1
            }
          }
        }
      }
    }
  }

  bind_rows(lista_combos)
}


# ============================================================
# 3. FUNCIÓN: crear_lags_disponibles_semana()
# ============================================================
# Qué hace:
#   Define qué retardos de un regresor de alta frecuencia están disponibles
#   en una actualización semanal concreta.
#
#   Ejemplo semanal, m = 4:
#     - semana 1: se conoce la primera semana del mes.
#     - semana 2: se conocen las dos primeras semanas.
#     - semana 3: se conocen las tres primeras semanas.
#     - semana 4: se conocen las cuatro semanas.
#
#   Como en mls() el lag 0 representa la observación más reciente del bloque
#   mensual, una predicción en semana 1 no puede usar lag 0, porque equivaldría
#   a usar información de final de mes. Por eso el bloque de lags empieza más
#   atrás. El argumento desplazamiento_retardos permite probar si el regresor
#   funciona mejor con un retardo adicional, por ejemplo una o varias semanas.
#
# Qué devuelve:
#   Un vector de retardos para usar dentro de mls().
#
crear_lags_disponibles_semana <- function(m,
                                          semana,
                                          meses_lag_midas = 3,
                                          semanas_por_mes = 4,
                                          desplazamiento_retardos = 0) {

  n_lags_total <- m * meses_lag_midas

  proporcion_disponible <- semana / semanas_por_mes
  n_obs_disponibles_mes <- max(1, floor(m * proporcion_disponible))

  lag_inicio <- m - n_obs_disponibles_mes
  lag_fin <- n_lags_total - 1

  (lag_inicio:lag_fin) + desplazamiento_retardos
}


# ============================================================
# 4. FUNCIÓN: construir_formula_midas_nealmon()
# ============================================================
# Qué hace:
#   Construye la fórmula de un modelo MIDAS restringido usando nealmon como
#   función de ponderación. La fórmula incluye los regresores de la combinación
#   y, opcionalmente, un componente autorregresivo de la variable objetivo.
#
#   La fórmula cambia con la semana de actualización: en semana 1 no usa
#   posiciones intra-mensuales que todavía no existen; en semana 4 puede usar
#   el bloque mensual completo. Esta es la base del ex post honesto.
#
# Qué devuelve:
#   Una fórmula de R lista para midas_r().
#
construir_formula_midas_nealmon <- function(regresores_combo,
                                            frecuencias_regresores,
                                            semana,
                                            meses_lag_midas = 3,
                                            semanas_por_mes = 4,
                                            incluir_ar = TRUE,
                                            desplazamiento_retardos = 0) {

  terminos <- c()

  if (incluir_ar) {
    terminos <- c(terminos, "mls(y, 1, 1)")
  }

  for (reg in regresores_combo) {
    m <- frecuencias_regresores[[reg]]
    lags <- crear_lags_disponibles_semana(
      m = m,
      semana = semana,
      meses_lag_midas = meses_lag_midas,
      semanas_por_mes = semanas_por_mes,
      desplazamiento_retardos = desplazamiento_retardos
    )

    texto_lags <- paste(lags, collapse = ",")
    termino <- paste0("mls(", reg, ", c(", texto_lags, "), ", m, ", nealmon)")
    terminos <- c(terminos, termino)
  }

  as.formula(paste("y ~", paste(terminos, collapse = " + ")))
}


# ============================================================
# 5. FUNCIÓN: crear_start_nealmon()
# ============================================================
# Qué hace:
#   Crea los valores iniciales para las restricciones nealmon de cada regresor.
#   En este script se usan dos parámetros iniciales por regresor, que es la
#   opción sencilla para empezar.
#
# Qué devuelve:
#   Una lista nombrada para el argumento start de midas_r().
#
crear_start_nealmon <- function(regresores_combo,
                                valores_iniciales = c(0, 0)) {

  start <- list()

  for (reg in regresores_combo) {
    start[[reg]] <- valores_iniciales
  }

  start
}


# ============================================================
# 6. FUNCIÓN: cortar_data_midas_por_meses()
# ============================================================
# Qué hace:
#   Recorta la lista de datos MIDAS para quedarse con los primeros n_meses de
#   la variable mensual y con n_meses x frecuencia observaciones de cada regresor.
#
# Qué devuelve:
#   Una lista data_midas recortada.
#
cortar_data_midas_por_meses <- function(data_midas,
                                         frecuencias_regresores,
                                         n_meses) {

  data_cortada <- list(y = data_midas$y[1:n_meses])

  for (reg in names(frecuencias_regresores)) {
    m <- frecuencias_regresores[[reg]]
    data_cortada[[reg]] <- data_midas[[reg]][1:(n_meses * m)]
  }

  data_cortada
}


# ============================================================
# 7. NOTA SOBRE LA PREDICCIÓN HONESTA SIN RELLENO
# ============================================================
# En versiones anteriores se construía un objeto de predicción rellenando las
# semanas no observadas del mes. Esa estrategia se elimina en esta versión.
#
# La solución adoptada es distinta:
#   1. La fórmula MIDAS se adapta a la semana de actualización y solo incluye
#      retardos de alta frecuencia realmente disponibles.
#   2. El objeto futuro conserva como NA las posiciones intra-mensuales aún no
#      observadas.
#   3. Como la fórmula no utiliza esos retardos futuros, esos NA no entran en
#      la predicción.
#
# De esta forma no se introduce información inventada ni valores futuros reales
# para semanas que no estarían disponibles en el momento de la predicción.


# ============================================================
# 7B. FUNCIÓN: crear_data_futuro_forecast_midas()
# ============================================================
# Qué hace:
#   Construye el objeto newdata que necesita forecast.midas_r(). Devuelve los
#   valores futuros de los regresores desde el final del train hasta el mes
#   objetivo.
#
#   Para el mes objetivo, solo se dejan como observadas las posiciones de alta
#   frecuencia disponibles hasta la semana indicada. Las posiciones posteriores
#   se dejan como NA. Esto no implica imputación: esas posiciones no deben ser
#   usadas por la fórmula MIDAS de esa semana.
#
# Qué devuelve:
#   Una lista nombrada con los regresores futuros. Esta lista se pasa a:
#     forecast(ajuste, newdata = data_futuro, insample = data_train, ...)
#
crear_data_futuro_forecast_midas <- function(data_midas,
                                             frecuencias_regresores,
                                             n_meses_train,
                                             mes_objetivo_pos,
                                             semana,
                                             semanas_por_mes = 4) {

  horizonte <- mes_objetivo_pos - n_meses_train

  if (horizonte < 1) {
    stop("mes_objetivo_pos debe ser posterior a n_meses_train.")
  }

  data_futuro <- list()

  for (reg in names(frecuencias_regresores)) {

    m <- frecuencias_regresores[[reg]]
    valores_futuros_reg <- c()

    for (h in 1:horizonte) {

      mes_pos <- n_meses_train + h

      inicio_mes <- (mes_pos - 1) * m + 1
      fin_mes <- mes_pos * m

      bloque_mes <- data_midas[[reg]][inicio_mes:fin_mes]

      if (mes_pos == mes_objetivo_pos) {

        n_disponibles <- max(1, floor(m * semana / semanas_por_mes))

        if (n_disponibles < m) {
          posiciones_no_disponibles <- (n_disponibles + 1):m
          bloque_mes[posiciones_no_disponibles] <- NA_real_
        }
      }

      valores_futuros_reg <- c(valores_futuros_reg, bloque_mes)
    }

    data_futuro[[reg]] <- valores_futuros_reg
  }

  data_futuro
}


# ============================================================
# 8AA. FUNCIÓN: crear_grid_errores_estacionarios()
# ============================================================
# Qué hace:
#   Define las estructuras ARMA/SARMA candidatas que se van a probar sobre la
#   serie objetivo mensual. Estas estructuras no sustituyen al modelo MIDAS:
#   sirven para seleccionar dos dinámicas temporales que después se usarán como
#   corrección sobre los residuos de cada ajuste MIDAS.
#
# Qué devuelve:
#   Una tibble con id_error y especificacion_error.
#
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


# ============================================================
# 8AB. FUNCIÓN AUXILIAR: crear_tsibble_mensual_generico()
# ============================================================
# Qué hace:
#   Convierte un vector mensual en un tsibble auxiliar. Se usa una fecha mensual
#   genérica porque para seleccionar estructuras ARMA/SARMA solo importa el
#   orden temporal de la serie, no la fecha de calendario exacta.
#
# Qué devuelve:
#   Un tsibble con columnas fecha_mensual e y.
#
crear_tsibble_mensual_generico <- function(y) {

  tibble(
    fecha_mensual = tsibble::yearmonth("2000 Jan") + seq_along(y) - 1,
    y = as.numeric(y)
  ) %>%
    as_tsibble(index = fecha_mensual)
}


# ============================================================
# 8AC. FUNCIÓN AUXILIAR: ajustar_arima_fable()
# ============================================================
# Qué hace:
#   Ajusta una estructura ARMA/SARMA escrita con la sintaxis de fable::ARIMA().
#   Se usa eval(parse()) para poder reutilizar las especificaciones guardadas
#   como texto, por ejemplo "pdq(1, 0, 1) + PDQ(0, 0, 1, period = 12)".
#
# Qué devuelve:
#   Una lista con ajuste y tabla de ajuste.
#
ajustar_arima_fable <- function(y,
                                id_error,
                                especificacion_error) {

  datos_ts <- crear_tsibble_mensual_generico(y)
  mensaje_error <- NA_character_

  ajuste <- tryCatch(
    {
      eval(parse(text = paste0(
        "datos_ts %>% fabletools::model(modelo = fable::ARIMA(y ~ ",
        especificacion_error,
        "))"
      )))
    },
    error = function(e) {
      mensaje_error <<- conditionMessage(e)
      return(NULL)
    }
  )

  if (is.null(ajuste)) {
    return(list(
      ajuste = NULL,
      tabla_ajuste = tibble(
        id_error = id_error,
        especificacion_error = especificacion_error,
        AIC = NA_real_,
        AICc = NA_real_,
        BIC = NA_real_,
        diagnostico_ajuste_error = "fallo_ajuste_arima",
        error_ajuste_error = mensaje_error
      )
    ))
  }

  tabla_ajuste <- tryCatch(
    {
      fabletools::glance(ajuste) %>%
        as_tibble() %>%
        transmute(
          id_error = id_error,
          especificacion_error = especificacion_error,
          AIC = AIC,
          AICc = AICc,
          BIC = BIC,
          diagnostico_ajuste_error = "ok",
          error_ajuste_error = NA_character_
        )
    },
    error = function(e) {
      tibble(
        id_error = id_error,
        especificacion_error = especificacion_error,
        AIC = NA_real_,
        AICc = NA_real_,
        BIC = NA_real_,
        diagnostico_ajuste_error = "fallo_glance_arima",
        error_ajuste_error = conditionMessage(e)
      )
    }
  )

  list(
    ajuste = ajuste,
    tabla_ajuste = tabla_ajuste
  )
}


# ============================================================
# 8AD. FUNCIÓN: seleccionar_errores_sarima_objetivo()
# ============================================================
# Qué hace:
#   Ajusta las estructuras ARMA/SARMA candidatas sobre la serie objetivo del
#   conjunto de entrenamiento y escoge dos estructuras. Por defecto se fuerza
#   que una de ellas sea la búsqueda automática con d = 0 y D = 0, y la otra la
#   mejor estructura manual según AICc.
#
# Qué devuelve:
#   Una lista con:
#     - tabla_errores: todas las estructuras ajustadas.
#     - errores_seleccionados: estructuras que se cruzarán con MIDAS.
#
seleccionar_errores_sarima_objetivo <- function(y_train,
                                                m = 12,
                                                n_errores = 2,
                                                forzar_incluir_auto = TRUE) {

  grid_errores <- crear_grid_errores_estacionarios(m = m)

  ajustes <- purrr::pmap(
    list(grid_errores$id_error, grid_errores$especificacion_error),
    function(id_error, especificacion_error) {
      ajustar_arima_fable(
        y = y_train,
        id_error = id_error,
        especificacion_error = especificacion_error
      )
    }
  )

  tabla_errores <- purrr::map_dfr(ajustes, "tabla_ajuste") %>%
    arrange(AICc, BIC, AIC)

  tabla_validos <- tabla_errores %>%
    filter(
      diagnostico_ajuste_error == "ok",
      !is.na(AICc),
      is.finite(AICc)
    )

  if (nrow(tabla_validos) == 0) {
    warning("No se ha podido ajustar ninguna estructura ARMA/SARMA. Se ejecutará MIDAS sin corrección SARIMA.")
    return(list(
      tabla_errores = tabla_errores,
      errores_seleccionados = tibble(
        id_error = "SIN_SARIMA",
        especificacion_error = "sin_correccion_sarima"
      )
    ))
  }

  if (forzar_incluir_auto) {

    mejor_auto <- tabla_validos %>%
      filter(id_error == "ARMA_auto_d0_D0") %>%
      slice_head(n = 1)

    mejor_manual <- tabla_validos %>%
      filter(id_error != "ARMA_auto_d0_D0") %>%
      arrange(AICc, BIC, AIC) %>%
      slice_head(n = 1)

    errores_seleccionados <- bind_rows(mejor_auto, mejor_manual) %>%
      distinct(id_error, .keep_all = TRUE)

    if (nrow(errores_seleccionados) < n_errores) {
      errores_seleccionados <- bind_rows(
        errores_seleccionados,
        tabla_validos %>%
          filter(!id_error %in% errores_seleccionados$id_error) %>%
          arrange(AICc, BIC, AIC) %>%
          slice_head(n = n_errores - nrow(errores_seleccionados))
      )
    }

  } else {

    errores_seleccionados <- tabla_validos %>%
      arrange(AICc, BIC, AIC) %>%
      slice_head(n = n_errores)
  }

  errores_seleccionados <- errores_seleccionados %>%
    select(id_error, especificacion_error, AIC, AICc, BIC)

  list(
    tabla_errores = tabla_errores,
    errores_seleccionados = errores_seleccionados
  )
}


# ============================================================
# 8AE. FUNCIÓN AUXILIAR: forecast_arima_fable()
# ============================================================
# Qué hace:
#   Ajusta una estructura ARMA/SARMA sobre una serie y devuelve su predicción
#   para el horizonte indicado. En el script se usa sobre los residuos MIDAS,
#   no directamente para sustituir la predicción MIDAS.
#
# Qué devuelve:
#   Una lista con predicción, AIC, AICc, BIC y diagnóstico.
#
forecast_arima_fable <- function(y,
                                 id_error,
                                 especificacion_error,
                                 h) {

  if (id_error == "SIN_SARIMA" || especificacion_error == "sin_correccion_sarima") {
    return(list(
      prediccion = 0,
      AIC = NA_real_,
      AICc = NA_real_,
      BIC = NA_real_,
      diagnostico = "sin_correccion_sarima",
      error = NA_character_
    ))
  }

  y <- as.numeric(y)
  y <- y[is.finite(y)]

  if (length(y) < 36) {
    return(list(
      prediccion = 0,
      AIC = NA_real_,
      AICc = NA_real_,
      BIC = NA_real_,
      diagnostico = "sin_correccion_por_pocos_residuos",
      error = NA_character_
    ))
  }

  ajuste_info <- ajustar_arima_fable(
    y = y,
    id_error = id_error,
    especificacion_error = especificacion_error
  )

  if (is.null(ajuste_info$ajuste)) {
    return(list(
      prediccion = 0,
      AIC = NA_real_,
      AICc = NA_real_,
      BIC = NA_real_,
      diagnostico = "fallo_ajuste_correccion_sarima",
      error = ajuste_info$tabla_ajuste$error_ajuste_error[1]
    ))
  }

  mensaje_error_fc <- NA_character_

  fc <- tryCatch(
    {
      fabletools::forecast(ajuste_info$ajuste, h = h)
    },
    error = function(e) {
      mensaje_error_fc <<- conditionMessage(e)
      return(NULL)
    }
  )

  if (is.null(fc)) {
    return(list(
      prediccion = 0,
      AIC = ajuste_info$tabla_ajuste$AIC[1],
      AICc = ajuste_info$tabla_ajuste$AICc[1],
      BIC = ajuste_info$tabla_ajuste$BIC[1],
      diagnostico = "fallo_forecast_correccion_sarima",
      error = mensaje_error_fc
    ))
  }

  pred_vector <- tryCatch(as.numeric(as_tibble(fc)$.mean), error = function(e) numeric(0))

  if (length(pred_vector) == 0) {
    pred <- 0
    diagnostico <- "forecast_correccion_sarima_vacio"
  } else if (length(pred_vector) >= h) {
    pred <- pred_vector[h]
    diagnostico <- "ok"
  } else {
    pred <- tail(pred_vector, 1)
    diagnostico <- "ok_forecast_corto"
  }

  list(
    prediccion = pred,
    AIC = ajuste_info$tabla_ajuste$AIC[1],
    AICc = ajuste_info$tabla_ajuste$AICc[1],
    BIC = ajuste_info$tabla_ajuste$BIC[1],
    diagnostico = diagnostico,
    error = NA_character_
  )
}


# ============================================================
# 8A. FUNCIÓN AUXILIAR: media_na()
# ============================================================
# Qué hace:
#   Calcula la media ignorando NA. Si todos los valores son NA, devuelve NA
#   en lugar de NaN.
#
# Qué devuelve:
#   Un número.
#
media_na <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    mean(x, na.rm = TRUE)
  }
}


# ============================================================
# 8B. FUNCIÓN AUXILIAR: resumir_diagnostico_ljung()
# ============================================================
# Qué hace:
#   Resume varios diagnósticos Ljung-Box de un mismo modelo. En MIDAS se ajusta
#   el modelo varias veces, una por actualización semanal, por lo que puede
#   haber varios diagnósticos para el mismo id_modelo.
#
# Qué devuelve:
#   Una etiqueta: residuos_ok, autocorrelacion_residual o no_calculable.
#
resumir_diagnostico_ljung <- function(diagnosticos) {

  diagnosticos <- diagnosticos[!is.na(diagnosticos)]

  if (length(diagnosticos) == 0) {
    return("no_calculable")
  }

  if (any(diagnosticos == "autocorrelacion_residual")) {
    return("autocorrelacion_residual")
  }

  if (any(diagnosticos == "residuos_ok")) {
    return("residuos_ok")
  }

  "no_calculable"
}


# ============================================================
# 8C. FUNCIÓN AUXILIAR: calcular_aicc_midas()
# ============================================================
# Qué hace:
#   Calcula una versión sencilla del AIC corregido para muestras pequeñas a
#   partir del AIC del modelo, el número de parámetros y el número de residuos.
#
# Qué devuelve:
#   El AICc, o NA si no es calculable.
#
calcular_aicc_midas <- function(ajuste) {

  aic <- tryCatch(AIC(ajuste), error = function(e) NA_real_)
  k <- tryCatch(length(coef(ajuste)), error = function(e) NA_integer_)
  n <- tryCatch(sum(!is.na(residuals(ajuste))), error = function(e) NA_integer_)

  if (is.na(aic) || is.na(k) || is.na(n) || n <= k + 1) {
    return(NA_real_)
  }

  aic + (2 * k * (k + 1)) / (n - k - 1)
}


# ============================================================
# 8D. FUNCIÓN AUXILIAR: calcular_ljung_midas()
# ============================================================
# Qué hace:
#   Aplica el test de Ljung-Box sobre los residuos de un modelo MIDAS ajustado.
#   El número de grados de libertad se aproxima con el número de coeficientes
#   estimados del modelo.
#
# Qué devuelve:
#   Una tibble con p-value y diagnóstico textual.
#
calcular_ljung_midas <- function(ajuste,
                                 alpha = 0.05,
                                 ljung_lag = 24) {

  residuos <- tryCatch(as.numeric(residuals(ajuste)), error = function(e) numeric(0))
  residuos <- residuos[is.finite(residuos)]
  n_residuos <- length(residuos)
  n_parametros <- tryCatch(length(coef(ajuste)), error = function(e) NA_integer_)

  if (n_residuos <= ljung_lag || is.na(n_parametros)) {
    return(tibble(
      n_residuos_validos = n_residuos,
      n_parametros = n_parametros,
      ljung_p_value = NA_real_,
      diagnostico_ljung = "no_calculable"
    ))
  }

  p_value <- tryCatch(
    Box.test(
      residuos,
      lag = ljung_lag,
      type = "Ljung-Box",
      fitdf = n_parametros
    )$p.value,
    error = function(e) NA_real_
  )

  tibble(
    n_residuos_validos = n_residuos,
    n_parametros = n_parametros,
    ljung_p_value = p_value,
    diagnostico_ljung = case_when(
      is.na(p_value) ~ "no_calculable",
      p_value < alpha ~ "autocorrelacion_residual",
      p_value >= alpha ~ "residuos_ok"
    )
  )
}


# ============================================================
# 8E. FUNCIÓN AUXILIAR: calcular_denominador_mase()
# ============================================================
# Qué hace:
#   Calcula el denominador del MASE usando un naive estacional mensual cuando
#   hay suficientes observaciones. Si no hay suficientes datos para usar m = 12,
#   usa un naive no estacional de lag 1.
#
# Qué devuelve:
#   El denominador del MASE.
#
calcular_denominador_mase <- function(y_train,
                                      m = 12) {

  y_train <- as.numeric(y_train)
  y_train <- y_train[is.finite(y_train)]

  if (length(y_train) > m) {
    denominador <- mean(
      abs(y_train[(m + 1):length(y_train)] - y_train[1:(length(y_train) - m)]),
      na.rm = TRUE
    )
  } else if (length(y_train) > 1) {
    denominador <- mean(abs(diff(y_train)), na.rm = TRUE)
  } else {
    denominador <- NA_real_
  }

  if (is.nan(denominador) || denominador == 0) {
    NA_real_
  } else {
    denominador
  }
}


# ============================================================
# 8F. FUNCIÓN AUXILIAR: aplicar_criterio_modelo_limpio_midas()
# ============================================================
# Qué hace:
#   Ordena los modelos siguiendo el criterio de modelo limpio:
#     1. Se identifica el menor RMSFE.
#     2. Se consideran candidatos los modelos con RMSFE dentro del margen.
#     3. Si hay suficientes errores, se informa Diebold-Mariano frente al
#        modelo de menor RMSFE.
#     4. Los candidatos se ordenan por MAE, diagnóstico Ljung-Box, AICc,
#        RMSFE y BIC.
#     5. Si el primer candidato por MAE tiene residuos no adecuados, se
#        sustituye por el siguiente candidato con residuos OK, si existe.
#
# Qué devuelve:
#   Una lista con tabla_final, mejor_modelo y metadatos del criterio.
#
aplicar_criterio_modelo_limpio_midas <- function(tabla_resultados,
                                                 tabla_predicciones,
                                                 alpha = 0.05,
                                                 margen_rmsfe = 0.10,
                                                 min_obs_dm = 10) {

  if (nrow(tabla_resultados) == 0) {
    stop("tabla_resultados está vacía.")
  }

  columnas_necesarias <- c("id_modelo", "RMSFE", "MAE", "AICc", "BIC", "diagnostico_ljung")

  if (!all(columnas_necesarias %in% names(tabla_resultados))) {
    stop("tabla_resultados no contiene las columnas mínimas para aplicar el criterio de modelo limpio.")
  }

  tabla_resultados <- tabla_resultados %>%
    filter(
      !is.na(RMSFE),
      !is.na(MAE),
      is.finite(RMSFE),
      is.finite(MAE)
    )

  if (nrow(tabla_resultados) == 0) {
    stop("No hay modelos con RMSFE y MAE válidos.")
  }

  rmsfe_minimo <- min(tabla_resultados$RMSFE, na.rm = TRUE)
  limite_rmsfe_candidato <- rmsfe_minimo * (1 + margen_rmsfe)

  modelo_menor_rmsfe <- tabla_resultados %>%
    arrange(RMSFE, MAE, AICc, BIC) %>%
    slice(1) %>%
    pull(id_modelo)

  # ----------------------------------------------------------
  # Diebold-Mariano aproximado.
  # En MIDAS se comparan errores de las actualizaciones semanales.
  # Si hay varias ventanas de validación, se añade la ventana al id.
  # ----------------------------------------------------------

  tabla_errores_dm <- tabla_predicciones %>%
    filter(!is.na(error), is.finite(error)) %>%
    mutate(
      id_prediccion_dm = paste(
        if ("ventana_validacion" %in% names(.)) ventana_validacion else "sin_ventana",
        fecha_objetivo,
        actualizacion,
        semana,
        sep = "_"
      )
    ) %>%
    select(id_prediccion_dm, id_modelo, error) %>%
    distinct()

  errores_ancho <- tabla_errores_dm %>%
    tidyr::pivot_wider(
      names_from = id_modelo,
      values_from = error
    )

  calcular_dm_aproximado <- function(modelo) {

    if (is.na(modelo) || modelo == modelo_menor_rmsfe) {
      return(tibble(
        id_modelo = modelo,
        dm_stat_vs_mejor_rmsfe = NA_real_,
        dm_p_value_vs_mejor_rmsfe = NA_real_,
        diagnostico_dm = "modelo_referencia_menor_RMSFE"
      ))
    }

    if (!(modelo_menor_rmsfe %in% names(errores_ancho)) || !(modelo %in% names(errores_ancho))) {
      return(tibble(
        id_modelo = modelo,
        dm_stat_vs_mejor_rmsfe = NA_real_,
        dm_p_value_vs_mejor_rmsfe = NA_real_,
        diagnostico_dm = "errores_no_disponibles"
      ))
    }

    e_ref <- errores_ancho[[modelo_menor_rmsfe]]
    e_mod <- errores_ancho[[modelo]]

    diferencial <- e_mod^2 - e_ref^2
    diferencial <- diferencial[is.finite(diferencial)]
    n_dm <- length(diferencial)

    if (n_dm < min_obs_dm) {
      return(tibble(
        id_modelo = modelo,
        dm_stat_vs_mejor_rmsfe = NA_real_,
        dm_p_value_vs_mejor_rmsfe = NA_real_,
        diagnostico_dm = paste0("no_aplicable_n_menor_", min_obs_dm)
      ))
    }

    sd_diferencial <- sd(diferencial, na.rm = TRUE)

    if (is.na(sd_diferencial) || sd_diferencial == 0) {
      return(tibble(
        id_modelo = modelo,
        dm_stat_vs_mejor_rmsfe = NA_real_,
        dm_p_value_vs_mejor_rmsfe = NA_real_,
        diagnostico_dm = "no_aplicable_varianza_nula"
      ))
    }

    dm_stat <- mean(diferencial, na.rm = TRUE) / (sd_diferencial / sqrt(n_dm))
    dm_p_value <- 2 * pnorm(-abs(dm_stat))

    tibble(
      id_modelo = modelo,
      dm_stat_vs_mejor_rmsfe = dm_stat,
      dm_p_value_vs_mejor_rmsfe = dm_p_value,
      diagnostico_dm = case_when(
        dm_p_value < alpha ~ "diferencia_predictiva_significativa",
        dm_p_value >= alpha ~ "sin_diferencia_predictiva_significativa",
        TRUE ~ "no_calculable"
      )
    )
  }

  tabla_dm <- tabla_resultados %>%
    pull(id_modelo) %>%
    purrr::map_dfr(calcular_dm_aproximado)

  tabla_final <- tabla_resultados %>%
    left_join(tabla_dm, by = "id_modelo") %>%
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
        candidato_modelo_limpio & residuos_ok_modelo_limpio ~ "candidato_RMSFE_DM_residuos_ok",
        candidato_modelo_limpio & !residuos_ok_modelo_limpio ~ "candidato_RMSFE_DM_residuos_no_ok",
        !candidato_modelo_limpio ~ "fuera_grupo_prioritario",
        TRUE ~ "sin_clasificar"
      )
    )

  candidatos <- tabla_final %>%
    filter(candidato_modelo_limpio)

  if (nrow(candidatos) == 0) {
    candidatos <- tabla_final
  }

  mejor_modelo <- candidatos %>%
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
    mejor_modelo$diagnostico_ljung[[1]] != "residuos_ok"
  ) {
    mejor_modelo <- candidatos_residuos_ok %>%
      arrange(MAE, AICc, RMSFE, BIC) %>%
      slice(1) %>%
      mutate(
        criterio_modelo_limpio = paste0(
          criterio_modelo_limpio,
          "_seleccionado_por_residuos_ok_con_RMSFE_similar"
        )
      )
  }

  tabla_final <- tabla_final %>%
    mutate(
      seleccionado_modelo_limpio = id_modelo == mejor_modelo$id_modelo[[1]]
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
    tabla_final = tabla_final,
    mejor_modelo = mejor_modelo,
    id_mejor_modelo = mejor_modelo$id_modelo[[1]],
    modelo_referencia_menor_rmsfe = modelo_menor_rmsfe,
    margen_rmsfe = margen_rmsfe
  )
}


# ============================================================
# 8. FUNCIÓN: ajustar_y_predecir_midas_una_actualizacion()
# ============================================================
# Qué hace:
#   Ajusta un modelo MIDAS para una combinación de regresores y una semana de
#   actualización. Después usa forecast() para generar una predicción del mes
#   objetivo.
#
#   La predicción es ex post honesta: el mes objetivo contiene NA en las
#   posiciones intra-mensuales que aún no estarían disponibles, y la fórmula
#   MIDAS se construye para no usar esos retardos.
#
# Qué devuelve:
#   Una fila con ajuste, residuos, predicción, error y diagnóstico.
#
ajustar_y_predecir_midas_una_actualizacion <- function(data_midas,
                                                       fechas_y,
                                                       frecuencias_regresores,
                                                       regresores_combo,
                                                       id_modelo,
                                                       n_meses_train,
                                                       mes_objetivo_pos,
                                                       semana,
                                                       meses_lag_midas = 3,
                                                       semanas_por_mes = 4,
                                                       incluir_ar = TRUE,
                                                       desplazamiento_retardos = 0,
                                                       id_error = "SIN_SARIMA",
                                                       especificacion_error = "sin_correccion_sarima",
                                                       alpha = 0.05,
                                                       ljung_lag = 24) {

  formula_midas <- construir_formula_midas_nealmon(
    regresores_combo = regresores_combo,
    frecuencias_regresores = frecuencias_regresores,
    semana = semana,
    meses_lag_midas = meses_lag_midas,
    semanas_por_mes = semanas_por_mes,
    incluir_ar = incluir_ar,
    desplazamiento_retardos = desplazamiento_retardos
  )

  data_train <- cortar_data_midas_por_meses(
    data_midas = data_midas,
    frecuencias_regresores = frecuencias_regresores,
    n_meses = n_meses_train
  )

  start <- crear_start_nealmon(regresores_combo)
  mensaje_error_ajuste <- NA_character_

  ajuste <- tryCatch(
    midas_r(
      formula = formula_midas,
      data = data_train,
      start = start
    ),
    error = function(e) {
      mensaje_error_ajuste <<- conditionMessage(e)
      return(NULL)
    }
  )

  if (is.null(ajuste)) {
    return(tibble(
      id_modelo = id_modelo,
      regresores = paste(regresores_combo, collapse = " + "),
      meses_lag_midas = meses_lag_midas,
      incluir_ar = incluir_ar,
      desplazamiento_retardos = desplazamiento_retardos,
      id_error = id_error,
      especificacion_error = especificacion_error,
      fecha_objetivo = fechas_y[mes_objetivo_pos],
      mes_objetivo_pos = mes_objetivo_pos,
      semana = semana,
      prediccion = NA_real_,
      real = data_midas$y[mes_objetivo_pos],
      error = NA_real_,
      abs_error = NA_real_,
      sq_error = NA_real_,
      AIC = NA_real_,
      AICc = NA_real_,
      BIC = NA_real_,
      n_residuos_validos = NA_integer_,
      n_parametros = NA_integer_,
      ljung_p_value = NA_real_,
      diagnostico_ljung = "no_calculable",
      formula = paste(deparse(formula_midas), collapse = ""),
      diagnostico = "fallo_ajuste",
      error_ajuste = mensaje_error_ajuste,
      error_prediccion = NA_character_,
      warning_prediccion = NA_character_,
      horizonte_forecast = mes_objetivo_pos - n_meses_train,
      longitud_pred_vector = NA_integer_
    ))
  }

  diagnostico_ljung <- calcular_ljung_midas(
    ajuste = ajuste,
    alpha = alpha,
    ljung_lag = ljung_lag
  )

  data_futuro <- crear_data_futuro_forecast_midas(
    data_midas = data_midas,
    frecuencias_regresores = frecuencias_regresores,
    n_meses_train = n_meses_train,
    mes_objetivo_pos = mes_objetivo_pos,
    semana = semana,
    semanas_por_mes = semanas_por_mes
  )

  horizonte_forecast <- mes_objetivo_pos - n_meses_train
  mensaje_error_prediccion <- NA_character_
  mensaje_warning_prediccion <- NA_character_

  fc <- tryCatch(
    withCallingHandlers(
      {
        midasr::forecast(
          object = ajuste,
          newdata = data_futuro,
          insample = data_train,
          method = ifelse(incluir_ar, "dynamic", "static"),
          se = FALSE,
          show_progress = FALSE,
          add_ts_info = FALSE
        )
      },
      warning = function(w) {
        mensaje_warning_prediccion <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      mensaje_error_prediccion <<- conditionMessage(e)
      return(NULL)
    }
  )

  if (is.null(fc)) {
    pred_vector <- numeric(0)
  } else {
    pred_vector <- as.numeric(fc$mean)
  }

  if (length(pred_vector) >= horizonte_forecast) {
    pred <- pred_vector[horizonte_forecast]
  } else if (length(pred_vector) > 0) {
    pred <- tail(pred_vector, 1)
  } else {
    pred <- NA_real_
  }

  pred_midas <- pred

  correccion_sarima <- forecast_arima_fable(
    y = residuals(ajuste),
    id_error = id_error,
    especificacion_error = especificacion_error,
    h = horizonte_forecast
  )

  if (!is.na(pred_midas)) {
    pred <- pred_midas + correccion_sarima$prediccion
  } else {
    pred <- NA_real_
  }

  real <- data_midas$y[mes_objetivo_pos]
  err <- real - pred

  diagnostico_pred <- case_when(
    is.null(fc) ~ "fallo_forecast_error",
    length(pred_vector) == 0 ~ "fallo_forecast_vacio",
    is.na(pred) ~ "fallo_forecast_na",
    correccion_sarima$diagnostico %in% c("fallo_ajuste_correccion_sarima", "fallo_forecast_correccion_sarima") ~ "ok_midas_sin_correccion_sarima",
    TRUE ~ "ok"
  )

  tibble(
    id_modelo = id_modelo,
    regresores = paste(regresores_combo, collapse = " + "),
    meses_lag_midas = meses_lag_midas,
    incluir_ar = incluir_ar,
    desplazamiento_retardos = desplazamiento_retardos,
    id_error = id_error,
    especificacion_error = especificacion_error,
    fecha_objetivo = fechas_y[mes_objetivo_pos],
    mes_objetivo_pos = mes_objetivo_pos,
    semana = semana,
    prediccion = pred,
    prediccion_midas = pred_midas,
    correccion_sarima_residuo = correccion_sarima$prediccion,
    real = real,
    error = err,
    abs_error = abs(err),
    sq_error = err^2,
    AIC = tryCatch(AIC(ajuste), error = function(e) NA_real_),
    AICc = calcular_aicc_midas(ajuste),
    BIC = tryCatch(BIC(ajuste), error = function(e) NA_real_),
    AIC_sarima_residuos = correccion_sarima$AIC,
    AICc_sarima_residuos = correccion_sarima$AICc,
    BIC_sarima_residuos = correccion_sarima$BIC,
    diagnostico_sarima_residuos = correccion_sarima$diagnostico,
    error_sarima_residuos = correccion_sarima$error,
    n_residuos_validos = diagnostico_ljung$n_residuos_validos,
    n_parametros = diagnostico_ljung$n_parametros,
    ljung_p_value = diagnostico_ljung$ljung_p_value,
    diagnostico_ljung = diagnostico_ljung$diagnostico_ljung,
    formula = paste(deparse(formula_midas), collapse = ""),
    diagnostico = diagnostico_pred,
    error_ajuste = mensaje_error_ajuste,
    error_prediccion = mensaje_error_prediccion,
    warning_prediccion = mensaje_warning_prediccion,
    horizonte_forecast = horizonte_forecast,
    longitud_pred_vector = length(pred_vector)
  )
}


# ============================================================
# 9. FUNCIÓN: obtener_predicciones_2m_8_actualizaciones()
# ============================================================
# Qué hace:
#   Para una combinación de regresores y una especificación MIDAS, genera las
#   8 predicciones buscadas: cuatro actualizaciones para el primer mes y cuatro
#   para el segundo mes.
#
# Qué devuelve:
#   Una tabla con las 8 predicciones de esa combinación/especificación.
#
obtener_predicciones_2m_8_actualizaciones <- function(data_midas,
                                                      fechas_y,
                                                      frecuencias_regresores,
                                                      regresores_combo,
                                                      id_modelo,
                                                      n_meses_train,
                                                      meses_lag_midas = 3,
                                                      semanas_por_mes = 4,
                                                      incluir_ar = TRUE,
                                                      desplazamiento_retardos = 0,
                                                      id_error = "SIN_SARIMA",
                                                      especificacion_error = "sin_correccion_sarima",
                                                      alpha = 0.05,
                                                      ljung_lag = 24) {

  filas <- list()
  contador <- 1

  for (h_mes in 1:2) {
    mes_objetivo_pos <- n_meses_train + h_mes

    for (semana in 1:semanas_por_mes) {

      filas[[contador]] <- ajustar_y_predecir_midas_una_actualizacion(
        data_midas = data_midas,
        fechas_y = fechas_y,
        frecuencias_regresores = frecuencias_regresores,
        regresores_combo = regresores_combo,
        id_modelo = id_modelo,
        n_meses_train = n_meses_train,
        mes_objetivo_pos = mes_objetivo_pos,
        semana = semana,
        meses_lag_midas = meses_lag_midas,
        semanas_por_mes = semanas_por_mes,
        incluir_ar = incluir_ar,
        desplazamiento_retardos = desplazamiento_retardos,
        id_error = id_error,
        especificacion_error = especificacion_error,
        alpha = alpha,
        ljung_lag = ljung_lag
      ) %>%
        mutate(
          h_mes = h_mes,
          actualizacion = contador
        )

      contador <- contador + 1
    }
  }

  bind_rows(filas)
}


# ============================================================
# 10. FUNCIÓN: resumir_metricas_modelo_limpio()
# ============================================================
# Qué hace:
#   Resume los errores de cada modelo en las 8 actualizaciones y calcula RMSFE,
#   MAE y MASE. Después aplica el criterio de modelo limpio descrito en la
#   metodología: RMSFE similar al mejor, Diebold-Mariano si procede, MAE,
#   Ljung-Box, AICc, RMSFE y BIC.
#
# Qué devuelve:
#   Una lista con tabla_resultados, mejor_modelo y selección completa.
#
resumir_metricas_modelo_limpio <- function(tabla_predicciones,
                                           y_train,
                                           m_mase = 12,
                                           alpha = 0.05,
                                           margen_rmsfe = 0.10,
                                           min_obs_dm = 10) {

  denominador_mase <- calcular_denominador_mase(
    y_train = y_train,
    m = m_mase
  )

  tabla_base <- tabla_predicciones %>%
    group_by(id_modelo, regresores, meses_lag_midas, incluir_ar, desplazamiento_retardos, id_error, especificacion_error) %>%
    summarise(
      n_predicciones = sum(!is.na(prediccion) & is.finite(prediccion)),
      RMSFE = sqrt(media_na(sq_error)),
      MAE = media_na(abs_error),
      MASE = MAE / denominador_mase,
      error_medio = media_na(error),
      AIC = media_na(AIC),
      AICc = media_na(AICc),
      BIC = media_na(BIC),
      ljung_p_value = media_na(ljung_p_value),
      diagnostico_ljung = resumir_diagnostico_ljung(diagnostico_ljung),
      n_fallos = sum(diagnostico != "ok"),
      .groups = "drop"
    ) %>%
    filter(
      n_predicciones > 0,
      !is.na(RMSFE),
      !is.na(MAE),
      is.finite(RMSFE),
      is.finite(MAE)
    ) %>%
    mutate(
      ranking_RMSFE = rank(RMSFE, ties.method = "min", na.last = "keep"),
      ranking_MAE = rank(MAE, ties.method = "min", na.last = "keep"),
      ranking_MASE = rank(MASE, ties.method = "min", na.last = "keep"),
      ranking_AICc = rank(AICc, ties.method = "min", na.last = "keep"),
      ranking_BIC = rank(BIC, ties.method = "min", na.last = "keep"),
      denominador_MASE = denominador_mase
    )

  seleccion <- aplicar_criterio_modelo_limpio_midas(
    tabla_resultados = tabla_base,
    tabla_predicciones = tabla_predicciones,
    alpha = alpha,
    margen_rmsfe = margen_rmsfe,
    min_obs_dm = min_obs_dm
  )

  list(
    tabla_resultados = seleccion$tabla_final,
    mejor_modelo = seleccion$mejor_modelo,
    seleccion_modelo_limpio = seleccion,
    denominador_MASE = denominador_mase
  )
}


# ============================================================
# 11. FUNCIÓN PRINCIPAL: seleccionar_midas_automatico_2_m_ex_post()
# ============================================================
# Qué hace:
#   Ejecuta el procedimiento ex post para una ventana: ajusta las combinaciones
#   MIDAS, obtiene 8 actualizaciones semanales para dos meses y ordena los
#   modelos con el criterio de modelo limpio.
#
# Qué devuelve:
#   Una lista con tabla_resultados, tabla_predicciones, mejor_modelo,
#   combinaciones y selección_modelo_limpio.
#
seleccionar_midas_automatico_2_m_ex_post <- function(objeto_midas,
                                                     regresores_candidatos,
                                                     fecha_inicio_test,
                                                     max_regresores = 4,
                                                     meses_lag_midas = 3,
                                                     semanas_por_mes = 4,
                                                     incluir_ar = TRUE,
                                                     desplazamiento_retardos = 0,
                                                     alpha = 0.05,
                                                     ljung_lag = 24,
                                                     margen_rmsfe = 0.10,
                                                     min_obs_dm = 10,
                                                     m_mase = 12,
                                                     usar_correccion_sarima = TRUE,
                                                     n_errores_sarima = 2,
                                                     forzar_incluir_auto_sarima = TRUE,
                                                     m_sarima = 12) {

  data_midas <- objeto_midas$data_midas
  fechas_y <- objeto_midas$fechas_y
  frecuencias <- objeto_midas$frecuencias[regresores_candidatos]

  if (inherits(fechas_y, "yearmonth")) {
    fecha_inicio_test <- tsibble::yearmonth(fecha_inicio_test)
  }

  pos_inicio_test <- which(fechas_y == fecha_inicio_test)

  if (length(pos_inicio_test) != 1) {
    stop("fecha_inicio_test debe coincidir exactamente con una fecha de fechas_y.")
  }

  n_meses_train <- pos_inicio_test - 1
  y_train <- data_midas$y[1:n_meses_train]

  seleccion_errores_sarima <- NULL

  if (usar_correccion_sarima) {
    seleccion_errores_sarima <- seleccionar_errores_sarima_objetivo(
      y_train = y_train,
      m = m_sarima,
      n_errores = n_errores_sarima,
      forzar_incluir_auto = forzar_incluir_auto_sarima
    )

    errores_sarima <- seleccion_errores_sarima$errores_seleccionados
  } else {
    errores_sarima <- tibble(
      id_error = "SIN_SARIMA",
      especificacion_error = "sin_correccion_sarima"
    )
  }

  combinaciones <- crear_combinaciones_regresores(
    regresores = regresores_candidatos,
    max_regresores = max_regresores,
    meses_lag_midas = meses_lag_midas,
    incluir_ar = incluir_ar,
    desplazamiento_retardos = desplazamiento_retardos,
    errores_sarima = errores_sarima
  )

  predicciones <- list()

  for (i in seq_len(nrow(combinaciones))) {

    combo <- combinaciones$regresores[[i]]
    id_modelo <- combinaciones$id_modelo[i]
    lag_modelo <- combinaciones$meses_lag_midas[i]
    ar_modelo <- combinaciones$incluir_ar[i]
    desfase_modelo <- combinaciones$desplazamiento_retardos[i]
    id_error_modelo <- combinaciones$id_error[i]
    especificacion_error_modelo <- combinaciones$especificacion_error[i]

    predicciones[[i]] <- obtener_predicciones_2m_8_actualizaciones(
      data_midas = data_midas,
      fechas_y = fechas_y,
      frecuencias_regresores = frecuencias,
      regresores_combo = combo,
      id_modelo = id_modelo,
      n_meses_train = n_meses_train,
      meses_lag_midas = lag_modelo,
      semanas_por_mes = semanas_por_mes,
      incluir_ar = ar_modelo,
      desplazamiento_retardos = desfase_modelo,
      id_error = id_error_modelo,
      especificacion_error = especificacion_error_modelo,
      alpha = alpha,
      ljung_lag = ljung_lag
    )
  }

  tabla_predicciones <- bind_rows(predicciones)

  resumen <- resumir_metricas_modelo_limpio(
    tabla_predicciones = tabla_predicciones,
    y_train = y_train,
    m_mase = m_mase,
    alpha = alpha,
    margen_rmsfe = margen_rmsfe,
    min_obs_dm = min_obs_dm
  )

  list(
    tipo = "ex_post",
    fecha_inicio_test = fecha_inicio_test,
    n_meses_train = n_meses_train,
    combinaciones = combinaciones,
    tabla_errores_sarima = if (is.null(seleccion_errores_sarima)) NULL else seleccion_errores_sarima$tabla_errores,
    errores_sarima_seleccionados = errores_sarima,
    tabla_predicciones = tabla_predicciones,
    tabla_resultados = resumen$tabla_resultados,
    mejor_modelo = resumen$mejor_modelo,
    seleccion_modelo_limpio = resumen$seleccion_modelo_limpio,
    denominador_MASE = resumen$denominador_MASE
  )
}


# ============================================================
# 12. FUNCIÓN PRINCIPAL: seleccionar_midas_automatico_2_m_ex_ante()
# ============================================================
# Qué hace:
#   Ejecuta el procedimiento ex ante. Genera cuatro ventanas de validación
#   previas a la fecha de ruptura, calcula las métricas medias de cada modelo y
#   aplica el mismo criterio de modelo limpio sobre la tabla media de validación.
#   Después genera las predicciones finales del modelo elegido para los dos
#   meses posteriores a la ruptura.
#
# Qué devuelve:
#   Una lista con tabla_validacion_media, mejor_modelo, tablas_validacion,
#   tabla_predicciones_finales y tabla_resultados_finales.
#
seleccionar_midas_automatico_2_m_ex_ante <- function(objeto_midas,
                                                     regresores_candidatos,
                                                     fecha_ruptura = as.Date("2020-03-01"),
                                                     max_regresores = 4,
                                                     meses_lag_midas = 3,
                                                     semanas_por_mes = 4,
                                                     incluir_ar = TRUE,
                                                     desplazamiento_retardos = 0,
                                                     usar_correccion_sarima = TRUE,
                                                     n_errores_sarima = 2,
                                                     forzar_incluir_auto_sarima = TRUE,
                                                     m_sarima = 12,
                                                     alpha = 0.05,
                                                     ljung_lag = 24,
                                                     margen_rmsfe = 0.10,
                                                     min_obs_dm = 10,
                                                     m_mase = 12) {

  data_midas <- objeto_midas$data_midas
  fechas_y <- objeto_midas$fechas_y
  frecuencias <- objeto_midas$frecuencias[regresores_candidatos]

  if (inherits(fechas_y, "yearmonth")) {
    fecha_ruptura <- tsibble::yearmonth(fecha_ruptura)
  }

  pos_ruptura <- which(fechas_y == fecha_ruptura)

  if (length(pos_ruptura) != 1) {
    stop("fecha_ruptura debe coincidir exactamente con una fecha de fechas_y.")
  }

  # ----------------------------------------------------------
  # 1. Selección previa de estructuras ARMA/SARMA
  # ----------------------------------------------------------
  # Esta parte debe usar los mismos nombres que la función
  # seleccionar_errores_sarima_objetivo() devuelve realmente en este script:
  #   - tabla_errores
  #   - errores_seleccionados
  #
  # En una versión anterior de esta función ex ante se intentaba extraer:
  #   - tabla_errores_sarima
  #   - errores_sarima_seleccionados
  #
  # Esos nombres no existen en la función auxiliar actual. Por eso, aunque se
  # pidiera usar_correccion_sarima = TRUE, las combinaciones podían terminar
  # creándose como si no hubiera estructuras SARIMA.
  # ----------------------------------------------------------

  y_train_sarima <- data_midas$y[1:pos_ruptura]
  seleccion_errores_sarima <- NULL

  if (usar_correccion_sarima) {

    seleccion_errores_sarima <- seleccionar_errores_sarima_objetivo(
      y_train = y_train_sarima,
      m = m_sarima,
      n_errores = n_errores_sarima,
      forzar_incluir_auto = forzar_incluir_auto_sarima
    )

    tabla_errores_sarima <- seleccion_errores_sarima$tabla_errores
    errores_sarima <- seleccion_errores_sarima$errores_seleccionados

  } else {

    tabla_errores_sarima <- tibble(
      id_error = "SIN_SARIMA",
      especificacion_error = "sin_correccion_sarima",
      AIC = NA_real_,
      AICc = NA_real_,
      BIC = NA_real_
    )

    errores_sarima <- tibble(
      id_error = "SIN_SARIMA",
      especificacion_error = "sin_correccion_sarima"
    )
  }

  if (is.null(errores_sarima) || nrow(errores_sarima) == 0) {
    warning("No se han obtenido estructuras SARIMA válidas. Se ejecutará MIDAS sin corrección SARIMA.")

    errores_sarima <- tibble(
      id_error = "SIN_SARIMA",
      especificacion_error = "sin_correccion_sarima"
    )
  }

  if (usar_correccion_sarima && all(errores_sarima$id_error == "SIN_SARIMA")) {
    warning(
      "Se ha solicitado usar_correccion_sarima = TRUE, pero no se ha seleccionado ninguna estructura ARMA/SARMA válida. ",
      "Revisa resultado$tabla_errores_sarima para ver los errores de ajuste."
    )
  }

  # ----------------------------------------------------------
  # 2. Creación de combinaciones MIDAS x SARIMA
  # ----------------------------------------------------------
  # Si usar_correccion_sarima = TRUE, errores_sarima debe contener los modelos
  # ARMA/SARMA seleccionados. Por tanto, id_modelo tendrá forma:
  #   MIDAS_001_SARIMA_ARMA_auto_d0_D0
  #   MIDAS_002_SARIMA_ARMA_101
  #   MIDAS_003_SARIMA_SARMA_001_001
  #
  # Si usar_correccion_sarima = FALSE, solo se crea la versión:
  #   MIDAS_001_SARIMA_SIN_SARIMA
  # ----------------------------------------------------------

  combinaciones <- crear_combinaciones_regresores(
    regresores = regresores_candidatos,
    max_regresores = max_regresores,
    meses_lag_midas = meses_lag_midas,
    incluir_ar = incluir_ar,
    desplazamiento_retardos = desplazamiento_retardos,
    errores_sarima = errores_sarima
  )

  # ----------------------------------------------------------
  # 3. Definición de las ventanas de validación ex ante
  # ----------------------------------------------------------
  # Se crean cuatro ventanas anteriores a la ruptura.
  # Para cada ventana, se simula una predicción de dos meses con ocho
  # actualizaciones semanales.
  # ----------------------------------------------------------

  inicios_validacion <- c(
    pos_ruptura - 8,
    pos_ruptura - 6,
    pos_ruptura - 4,
    pos_ruptura - 2
  )

  inicios_validacion <- inicios_validacion[inicios_validacion > 1]

  tablas_validacion <- list()

  # ----------------------------------------------------------
  # 4. Evaluación de todos los modelos en cada ventana
  # ----------------------------------------------------------

  for (v in seq_along(inicios_validacion)) {

    pos_inicio_val <- inicios_validacion[v]
    n_meses_train_val <- pos_inicio_val - 1
    y_train_val <- data_midas$y[1:n_meses_train_val]

    predicciones_v <- list()

    for (i in seq_len(nrow(combinaciones))) {

      combo <- combinaciones$regresores[[i]]
      id_modelo <- combinaciones$id_modelo[i]
      lag_modelo <- combinaciones$meses_lag_midas[i]
      ar_modelo <- combinaciones$incluir_ar[i]
      desfase_modelo <- combinaciones$desplazamiento_retardos[i]
      id_error_modelo <- combinaciones$id_error[i]
      especificacion_error_modelo <- combinaciones$especificacion_error[i]

      predicciones_v[[i]] <- obtener_predicciones_2m_8_actualizaciones(
        data_midas = data_midas,
        fechas_y = fechas_y,
        frecuencias_regresores = frecuencias,
        regresores_combo = combo,
        id_modelo = id_modelo,
        n_meses_train = n_meses_train_val,
        meses_lag_midas = lag_modelo,
        semanas_por_mes = semanas_por_mes,
        incluir_ar = ar_modelo,
        desplazamiento_retardos = desfase_modelo,
        id_error = id_error_modelo,
        especificacion_error = especificacion_error_modelo,
        alpha = alpha,
        ljung_lag = ljung_lag
      ) %>%
        mutate(
          ventana_validacion = v,
          fecha_inicio_validacion = fechas_y[pos_inicio_val]
        )
    }

    tabla_pred_v <- bind_rows(predicciones_v)

    resumen_v <- resumir_metricas_modelo_limpio(
      tabla_predicciones = tabla_pred_v,
      y_train = y_train_val,
      m_mase = m_mase,
      alpha = alpha,
      margen_rmsfe = margen_rmsfe,
      min_obs_dm = min_obs_dm
    )

    tabla_res_v <- resumen_v$tabla_resultados %>%
      mutate(
        ventana_validacion = v,
        fecha_inicio_validacion = fechas_y[pos_inicio_val]
      )

    tablas_validacion[[v]] <- list(
      ventana = v,
      fecha_inicio_validacion = fechas_y[pos_inicio_val],
      tabla_predicciones = tabla_pred_v,
      tabla_resultados = tabla_res_v,
      seleccion_modelo_limpio = resumen_v$seleccion_modelo_limpio
    )
  }

  # ----------------------------------------------------------
  # 5. Unión de resultados de validación
  # ----------------------------------------------------------

  tabla_predicciones_validacion <- map_dfr(tablas_validacion, "tabla_predicciones")
  tabla_resultados_validacion <- map_dfr(tablas_validacion, "tabla_resultados")

  # ----------------------------------------------------------
  # 6. Promedio de resultados por modelo
  # ----------------------------------------------------------
  # Cada modelo se identifica no solo por sus regresores y estructura MIDAS,
  # sino también por la estructura SARIMA utilizada para corregir residuos.
  # ----------------------------------------------------------

  tabla_validacion_base <- tabla_resultados_validacion %>%
    group_by(
      id_modelo,
      regresores,
      meses_lag_midas,
      incluir_ar,
      desplazamiento_retardos,
      id_error,
      especificacion_error
    ) %>%
    summarise(
      n_predicciones = media_na(n_predicciones),
      RMSFE = media_na(RMSFE),
      MAE = media_na(MAE),
      MASE = media_na(MASE),
      error_medio = media_na(error_medio),
      AIC = media_na(AIC),
      AICc = media_na(AICc),
      BIC = media_na(BIC),
      ljung_p_value = media_na(ljung_p_value),
      diagnostico_ljung = resumir_diagnostico_ljung(diagnostico_ljung),
      n_fallos = media_na(n_fallos),
      .groups = "drop"
    ) %>%
    filter(
      !is.na(RMSFE),
      !is.na(MAE),
      is.finite(RMSFE),
      is.finite(MAE)
    ) %>%
    mutate(
      ranking_RMSFE = rank(RMSFE, ties.method = "min", na.last = "keep"),
      ranking_MAE = rank(MAE, ties.method = "min", na.last = "keep"),
      ranking_MASE = rank(MASE, ties.method = "min", na.last = "keep"),
      ranking_AICc = rank(AICc, ties.method = "min", na.last = "keep"),
      ranking_BIC = rank(BIC, ties.method = "min", na.last = "keep")
    )

  # ----------------------------------------------------------
  # 7. Aplicación del criterio de modelo limpio sobre la media
  #    de validación
  # ----------------------------------------------------------

  seleccion_validacion_media <- aplicar_criterio_modelo_limpio_midas(
    tabla_resultados = tabla_validacion_base,
    tabla_predicciones = tabla_predicciones_validacion,
    alpha = alpha,
    margen_rmsfe = margen_rmsfe,
    min_obs_dm = min_obs_dm
  )

  tabla_validacion_media <- seleccion_validacion_media$tabla_final
  mejor_modelo <- seleccion_validacion_media$mejor_modelo

  # ----------------------------------------------------------
  # 8. Recuperación de la especificación del mejor modelo
  # ----------------------------------------------------------

  fila_mejor_combo <- combinaciones %>%
    filter(id_modelo == mejor_modelo$id_modelo[1]) %>%
    slice(1)

  if (nrow(fila_mejor_combo) == 0) {
    stop("No se ha podido recuperar la especificación del mejor modelo.")
  }

  combo_mejor <- fila_mejor_combo$regresores[[1]]
  lag_mejor <- fila_mejor_combo$meses_lag_midas[[1]]
  ar_mejor <- fila_mejor_combo$incluir_ar[[1]]
  desfase_mejor <- fila_mejor_combo$desplazamiento_retardos[[1]]
  id_error_mejor <- fila_mejor_combo$id_error[[1]]
  especificacion_error_mejor <- fila_mejor_combo$especificacion_error[[1]]

  # ----------------------------------------------------------
  # 9. Predicción final ex ante
  # ----------------------------------------------------------
  # Una vez seleccionado el modelo con validación previa, se reentrena
  # usando los datos disponibles hasta la ruptura y se predicen los dos
  # meses posteriores con ocho actualizaciones semanales.
  # ----------------------------------------------------------

  n_meses_train_final <- pos_ruptura
  y_train_final <- data_midas$y[1:n_meses_train_final]

  tabla_predicciones_finales <- obtener_predicciones_2m_8_actualizaciones(
    data_midas = data_midas,
    fechas_y = fechas_y,
    frecuencias_regresores = frecuencias,
    regresores_combo = combo_mejor,
    id_modelo = mejor_modelo$id_modelo[1],
    n_meses_train = n_meses_train_final,
    meses_lag_midas = lag_mejor,
    semanas_por_mes = semanas_por_mes,
    incluir_ar = ar_mejor,
    desplazamiento_retardos = desfase_mejor,
    id_error = id_error_mejor,
    especificacion_error = especificacion_error_mejor,
    alpha = alpha,
    ljung_lag = ljung_lag
  )

  resumen_final <- resumir_metricas_modelo_limpio(
    tabla_predicciones = tabla_predicciones_finales,
    y_train = y_train_final,
    m_mase = m_mase,
    alpha = alpha,
    margen_rmsfe = margen_rmsfe,
    min_obs_dm = min_obs_dm
  )

  # ----------------------------------------------------------
  # 10. Salida
  # ----------------------------------------------------------

  list(
    tipo = "ex_ante",
    fecha_ruptura = fecha_ruptura,
    usar_correccion_sarima = usar_correccion_sarima,
    tabla_errores_sarima = tabla_errores_sarima,
    errores_sarima_seleccionados = errores_sarima,
    combinaciones = combinaciones,
    tablas_validacion = tablas_validacion,
    tabla_predicciones_validacion = tabla_predicciones_validacion,
    tabla_resultados_validacion = tabla_resultados_validacion,
    tabla_validacion_media = tabla_validacion_media,
    mejor_modelo = mejor_modelo,
    seleccion_validacion_media = seleccion_validacion_media,
    tabla_predicciones_finales = tabla_predicciones_finales,
    tabla_resultados_finales = resumen_final$tabla_resultados,
    seleccion_final = resumen_final$seleccion_modelo_limpio
  )
}

# ============================================================



# ============================================================
# 11B. FUNCIÓN PRINCIPAL REDEFINIDA:
#      seleccionar_midas_automatico_2_m_ex_post()
# ============================================================
# Unión metodológica con regresión dinámica:
#   - Conserva toda la lógica MIDAS original.
#   - Añade diagnóstico de estacionariedad de y y regresores antes del ajuste.
#   - Devuelve el diagnóstico junto con las tablas habituales.
# ============================================================

seleccionar_midas_automatico_2_m_ex_post <- function(objeto_midas,
                                                     regresores_candidatos,
                                                     fecha_inicio_test,
                                                     max_regresores = 4,
                                                     meses_lag_midas = 3,
                                                     semanas_por_mes = 4,
                                                     incluir_ar = TRUE,
                                                     desplazamiento_retardos = 0,
                                                     alpha = 0.05,
                                                     ljung_lag = 24,
                                                     margen_rmsfe = 0.10,
                                                     min_obs_dm = 10,
                                                     m_mase = 12,
                                                     usar_correccion_sarima = TRUE,
                                                     n_errores_sarima = 2,
                                                     forzar_incluir_auto_sarima = TRUE,
                                                     m_sarima = 12,
                                                     comprobar_estacionariedad = TRUE,
                                                     detener_si_no_estacionaria = FALSE) {

  data_midas <- objeto_midas$data_midas
  fechas_y <- objeto_midas$fechas_y
  frecuencias <- objeto_midas$frecuencias[regresores_candidatos]

  if (inherits(fechas_y, "yearmonth")) {
    fecha_inicio_test <- tsibble::yearmonth(fecha_inicio_test)
  }

  pos_inicio_test <- which(fechas_y == fecha_inicio_test)

  if (length(pos_inicio_test) != 1) {
    stop("fecha_inicio_test debe coincidir exactamente con una fecha de fechas_y.")
  }

  n_meses_train <- pos_inicio_test - 1
  y_train <- data_midas$y[1:n_meses_train]

  diagnostico_estacionariedad <- tibble()
  variables_no_estacionarias <- character(0)

  if (comprobar_estacionariedad) {
    diagnostico_estacionariedad <- comprobar_estacionariedad_midas(
      objeto_midas = objeto_midas,
      regresores = regresores_candidatos,
      n_meses_train = n_meses_train,
      alpha = alpha,
      detener_si_no_estacionaria = detener_si_no_estacionaria
    )

    variables_no_estacionarias <- extraer_variables_no_estacionarias_midas(
      diagnostico_estacionariedad
    )
  }

  seleccion_errores_sarima <- NULL

  if (usar_correccion_sarima) {
    seleccion_errores_sarima <- seleccionar_errores_sarima_objetivo(
      y_train = y_train,
      m = m_sarima,
      n_errores = n_errores_sarima,
      forzar_incluir_auto = forzar_incluir_auto_sarima
    )

    errores_sarima <- seleccion_errores_sarima$errores_seleccionados
  } else {
    errores_sarima <- tibble(
      id_error = "SIN_SARIMA",
      especificacion_error = "sin_correccion_sarima"
    )
  }

  combinaciones <- crear_combinaciones_regresores(
    regresores = regresores_candidatos,
    max_regresores = max_regresores,
    meses_lag_midas = meses_lag_midas,
    incluir_ar = incluir_ar,
    desplazamiento_retardos = desplazamiento_retardos,
    errores_sarima = errores_sarima
  )

  predicciones <- list()

  for (i in seq_len(nrow(combinaciones))) {
    
    cat("Ex post - modelo", i, "de", nrow(combinaciones), "\n")

    combo <- combinaciones$regresores[[i]]
    id_modelo <- combinaciones$id_modelo[i]
    lag_modelo <- combinaciones$meses_lag_midas[i]
    ar_modelo <- combinaciones$incluir_ar[i]
    desfase_modelo <- combinaciones$desplazamiento_retardos[i]
    id_error_modelo <- combinaciones$id_error[i]
    especificacion_error_modelo <- combinaciones$especificacion_error[i]

    predicciones[[i]] <- obtener_predicciones_2m_8_actualizaciones(
      data_midas = data_midas,
      fechas_y = fechas_y,
      frecuencias_regresores = frecuencias,
      regresores_combo = combo,
      id_modelo = id_modelo,
      n_meses_train = n_meses_train,
      meses_lag_midas = lag_modelo,
      semanas_por_mes = semanas_por_mes,
      incluir_ar = ar_modelo,
      desplazamiento_retardos = desfase_modelo,
      id_error = id_error_modelo,
      especificacion_error = especificacion_error_modelo,
      alpha = alpha,
      ljung_lag = ljung_lag
    )
  }

  tabla_predicciones <- bind_rows(predicciones)

  resumen <- resumir_metricas_modelo_limpio(
    tabla_predicciones = tabla_predicciones,
    y_train = y_train,
    m_mase = m_mase,
    alpha = alpha,
    margen_rmsfe = margen_rmsfe,
    min_obs_dm = min_obs_dm
  )

  list(
    tipo = "ex_post",
    fecha_inicio_test = fecha_inicio_test,
    n_meses_train = n_meses_train,
    combinaciones = combinaciones,
    diagnostico_estacionariedad = diagnostico_estacionariedad,
    variables_no_estacionarias = variables_no_estacionarias,
    tabla_errores_sarima = if (is.null(seleccion_errores_sarima)) NULL else seleccion_errores_sarima$tabla_errores,
    errores_sarima_seleccionados = errores_sarima,
    tabla_predicciones = tabla_predicciones,
    tabla_resultados = resumen$tabla_resultados,
    mejor_modelo = resumen$mejor_modelo,
    seleccion_modelo_limpio = resumen$seleccion_modelo_limpio,
    denominador_MASE = resumen$denominador_MASE
  )
}


# ============================================================
# 12B. FUNCIÓN PRINCIPAL REDEFINIDA:
#      seleccionar_midas_automatico_2_m_ex_ante()
# ============================================================
# Unión metodológica con regresión dinámica:
#   - Conserva la validación previa ex ante y la predicción final posterior.
#   - Añade diagnóstico de estacionariedad en el train disponible hasta ruptura.
#   - Mantiene la corrección SARIMA de residuos.
# ============================================================

seleccionar_midas_automatico_2_m_ex_ante <- function(objeto_midas,
                                                     regresores_candidatos,
                                                     fecha_ruptura = as.Date("2020-03-01"),
                                                     max_regresores = 4,
                                                     meses_lag_midas = 3,
                                                     semanas_por_mes = 4,
                                                     incluir_ar = TRUE,
                                                     desplazamiento_retardos = 0,
                                                     usar_correccion_sarima = TRUE,
                                                     n_errores_sarima = 2,
                                                     forzar_incluir_auto_sarima = TRUE,
                                                     m_sarima = 12,
                                                     alpha = 0.05,
                                                     ljung_lag = 24,
                                                     margen_rmsfe = 0.10,
                                                     min_obs_dm = 10,
                                                     m_mase = 12,
                                                     comprobar_estacionariedad = TRUE,
                                                     detener_si_no_estacionaria = FALSE) {

  data_midas <- objeto_midas$data_midas
  fechas_y <- objeto_midas$fechas_y
  frecuencias <- objeto_midas$frecuencias[regresores_candidatos]

  if (inherits(fechas_y, "yearmonth")) {
    fecha_ruptura <- tsibble::yearmonth(fecha_ruptura)
  }

  pos_ruptura <- which(fechas_y == fecha_ruptura)

  if (length(pos_ruptura) != 1) {
    stop("fecha_ruptura debe coincidir exactamente con una fecha de fechas_y.")
  }

  y_train_sarima <- data_midas$y[1:pos_ruptura]

  diagnostico_estacionariedad <- tibble()
  variables_no_estacionarias <- character(0)

  if (comprobar_estacionariedad) {
    diagnostico_estacionariedad <- comprobar_estacionariedad_midas(
      objeto_midas = objeto_midas,
      regresores = regresores_candidatos,
      n_meses_train = pos_ruptura,
      alpha = alpha,
      detener_si_no_estacionaria = detener_si_no_estacionaria
    )

    variables_no_estacionarias <- extraer_variables_no_estacionarias_midas(
      diagnostico_estacionariedad
    )
  }

  seleccion_errores_sarima <- NULL

  if (usar_correccion_sarima) {

    seleccion_errores_sarima <- seleccionar_errores_sarima_objetivo(
      y_train = y_train_sarima,
      m = m_sarima,
      n_errores = n_errores_sarima,
      forzar_incluir_auto = forzar_incluir_auto_sarima
    )

    tabla_errores_sarima <- seleccion_errores_sarima$tabla_errores
    errores_sarima <- seleccion_errores_sarima$errores_seleccionados

  } else {

    tabla_errores_sarima <- tibble(
      id_error = "SIN_SARIMA",
      especificacion_error = "sin_correccion_sarima",
      AIC = NA_real_,
      AICc = NA_real_,
      BIC = NA_real_
    )

    errores_sarima <- tibble(
      id_error = "SIN_SARIMA",
      especificacion_error = "sin_correccion_sarima"
    )
  }

  if (is.null(errores_sarima) || nrow(errores_sarima) == 0) {
    warning("No se han obtenido estructuras SARIMA válidas. Se ejecutará MIDAS sin corrección SARIMA.")

    errores_sarima <- tibble(
      id_error = "SIN_SARIMA",
      especificacion_error = "sin_correccion_sarima"
    )
  }

  if (usar_correccion_sarima && all(errores_sarima$id_error == "SIN_SARIMA")) {
    warning(
      "Se ha solicitado usar_correccion_sarima = TRUE, pero no se ha seleccionado ninguna estructura ARMA/SARMA válida. ",
      "Revisa resultado$tabla_errores_sarima para ver los errores de ajuste."
    )
  }

  combinaciones <- crear_combinaciones_regresores(
    regresores = regresores_candidatos,
    max_regresores = max_regresores,
    meses_lag_midas = meses_lag_midas,
    incluir_ar = incluir_ar,
    desplazamiento_retardos = desplazamiento_retardos,
    errores_sarima = errores_sarima
  )

  inicios_validacion <- c(
    pos_ruptura - 8,
    pos_ruptura - 6,
    pos_ruptura - 4,
    pos_ruptura - 2
  )

  inicios_validacion <- inicios_validacion[inicios_validacion > 1]

  tablas_validacion <- list()

  for (v in seq_along(inicios_validacion)) {

    pos_inicio_val <- inicios_validacion[v]
    n_meses_train_val <- pos_inicio_val - 1
    y_train_val <- data_midas$y[1:n_meses_train_val]

    predicciones_v <- list()

    for (i in seq_len(nrow(combinaciones))) {
      
      cat("Ex ante - ventana", v, "de", length(inicios_validacion), "\n")

      combo <- combinaciones$regresores[[i]]
      id_modelo <- combinaciones$id_modelo[i]
      lag_modelo <- combinaciones$meses_lag_midas[i]
      ar_modelo <- combinaciones$incluir_ar[i]
      desfase_modelo <- combinaciones$desplazamiento_retardos[i]
      id_error_modelo <- combinaciones$id_error[i]
      especificacion_error_modelo <- combinaciones$especificacion_error[i]

      predicciones_v[[i]] <- obtener_predicciones_2m_8_actualizaciones(
        data_midas = data_midas,
        fechas_y = fechas_y,
        frecuencias_regresores = frecuencias,
        regresores_combo = combo,
        id_modelo = id_modelo,
        n_meses_train = n_meses_train_val,
        meses_lag_midas = lag_modelo,
        semanas_por_mes = semanas_por_mes,
        incluir_ar = ar_modelo,
        desplazamiento_retardos = desfase_modelo,
        id_error = id_error_modelo,
        especificacion_error = especificacion_error_modelo,
        alpha = alpha,
        ljung_lag = ljung_lag
      ) %>%
        mutate(
          ventana_validacion = v,
          fecha_inicio_validacion = fechas_y[pos_inicio_val]
        )
    }

    tabla_pred_v <- bind_rows(predicciones_v)

    resumen_v <- resumir_metricas_modelo_limpio(
      tabla_predicciones = tabla_pred_v,
      y_train = y_train_val,
      m_mase = m_mase,
      alpha = alpha,
      margen_rmsfe = margen_rmsfe,
      min_obs_dm = min_obs_dm
    )

    tabla_res_v <- resumen_v$tabla_resultados %>%
      mutate(
        ventana_validacion = v,
        fecha_inicio_validacion = fechas_y[pos_inicio_val]
      )

    tablas_validacion[[v]] <- list(
      ventana = v,
      fecha_inicio_validacion = fechas_y[pos_inicio_val],
      tabla_predicciones = tabla_pred_v,
      tabla_resultados = tabla_res_v,
      seleccion_modelo_limpio = resumen_v$seleccion_modelo_limpio
    )
  }

  tabla_predicciones_validacion <- map_dfr(tablas_validacion, "tabla_predicciones")
  tabla_resultados_validacion <- map_dfr(tablas_validacion, "tabla_resultados")

  tabla_validacion_base <- tabla_resultados_validacion %>%
    group_by(
      id_modelo,
      regresores,
      meses_lag_midas,
      incluir_ar,
      desplazamiento_retardos,
      id_error,
      especificacion_error
    ) %>%
    summarise(
      n_predicciones = media_na(n_predicciones),
      RMSFE = media_na(RMSFE),
      MAE = media_na(MAE),
      MASE = media_na(MASE),
      error_medio = media_na(error_medio),
      AIC = media_na(AIC),
      AICc = media_na(AICc),
      BIC = media_na(BIC),
      ljung_p_value = media_na(ljung_p_value),
      diagnostico_ljung = resumir_diagnostico_ljung(diagnostico_ljung),
      n_fallos = media_na(n_fallos),
      .groups = "drop"
    ) %>%
    filter(
      !is.na(RMSFE),
      !is.na(MAE),
      is.finite(RMSFE),
      is.finite(MAE)
    ) %>%
    mutate(
      ranking_RMSFE = rank(RMSFE, ties.method = "min", na.last = "keep"),
      ranking_MAE = rank(MAE, ties.method = "min", na.last = "keep"),
      ranking_MASE = rank(MASE, ties.method = "min", na.last = "keep"),
      ranking_AICc = rank(AICc, ties.method = "min", na.last = "keep"),
      ranking_BIC = rank(BIC, ties.method = "min", na.last = "keep")
    )

  seleccion_validacion_media <- aplicar_criterio_modelo_limpio_midas(
    tabla_resultados = tabla_validacion_base,
    tabla_predicciones = tabla_predicciones_validacion,
    alpha = alpha,
    margen_rmsfe = margen_rmsfe,
    min_obs_dm = min_obs_dm
  )

  tabla_validacion_media <- seleccion_validacion_media$tabla_final
  mejor_modelo <- seleccion_validacion_media$mejor_modelo

  fila_mejor_combo <- combinaciones %>%
    filter(id_modelo == mejor_modelo$id_modelo[1]) %>%
    slice(1)

  if (nrow(fila_mejor_combo) == 0) {
    stop("No se ha podido recuperar la especificación del mejor modelo ex ante.")
  }

  combo_mejor <- fila_mejor_combo$regresores[[1]]
  lag_mejor <- fila_mejor_combo$meses_lag_midas[[1]]
  ar_mejor <- fila_mejor_combo$incluir_ar[[1]]
  desfase_mejor <- fila_mejor_combo$desplazamiento_retardos[[1]]
  id_error_mejor <- fila_mejor_combo$id_error[[1]]
  especificacion_error_mejor <- fila_mejor_combo$especificacion_error[[1]]

  n_meses_train_final <- pos_ruptura
  y_train_final <- data_midas$y[1:n_meses_train_final]
  
  

  # ----------------------------------------------------------
  # 9B. Evaluación final en test de TODOS los modelos,
  #     pero conservando la selección ex ante de validación.
  # ----------------------------------------------------------
  
  nombres_top3_modelos_ex_ante <- tabla_validacion_media %>%
    slice_head(n = 3) %>%
    pull(id_modelo)
  
  predicciones_finales_todos <- list()
  
  for (i in seq_len(nrow(combinaciones))) {
    
    cat("Ex ante final-test - modelo", i, "de", nrow(combinaciones), "\n")
    
    combo <- combinaciones$regresores[[i]]
    id_modelo <- combinaciones$id_modelo[i]
    lag_modelo <- combinaciones$meses_lag_midas[i]
    ar_modelo <- combinaciones$incluir_ar[i]
    desfase_modelo <- combinaciones$desplazamiento_retardos[i]
    id_error_modelo <- combinaciones$id_error[i]
    especificacion_error_modelo <- combinaciones$especificacion_error[i]
    
    predicciones_finales_todos[[i]] <- obtener_predicciones_2m_8_actualizaciones(
      data_midas = data_midas,
      fechas_y = fechas_y,
      frecuencias_regresores = frecuencias,
      regresores_combo = combo,
      id_modelo = id_modelo,
      n_meses_train = n_meses_train_final,
      meses_lag_midas = lag_modelo,
      semanas_por_mes = semanas_por_mes,
      incluir_ar = ar_modelo,
      desplazamiento_retardos = desfase_modelo,
      id_error = id_error_modelo,
      especificacion_error = especificacion_error_modelo,
      alpha = alpha,
      ljung_lag = ljung_lag
    )
  }
  
  tabla_predicciones_finales_todos <- bind_rows(predicciones_finales_todos)
  
  resumen_final_todos <- resumir_metricas_modelo_limpio(
    tabla_predicciones = tabla_predicciones_finales_todos,
    y_train = y_train_final,
    m_mase = m_mase,
    alpha = alpha,
    margen_rmsfe = margen_rmsfe,
    min_obs_dm = min_obs_dm
  )
  
  # ----------------------------------------------------------
  # Tabla de orden ex ante procedente de la validación.
  # ----------------------------------------------------------
  
  tabla_orden_ex_ante <- tabla_validacion_media %>%
    mutate(
      orden_ex_ante = row_number()
    ) %>%
    transmute(
      id_modelo,
      orden_ex_ante,
      
      seleccionado_modelo_limpio_ex_ante = seleccionado_modelo_limpio,
      candidato_modelo_limpio_ex_ante = candidato_modelo_limpio,
      residuos_ok_modelo_limpio_ex_ante = residuos_ok_modelo_limpio,
      prioridad_residuos_modelo_limpio_ex_ante = prioridad_residuos_modelo_limpio,
      criterio_modelo_limpio_ex_ante = criterio_modelo_limpio,
      
      RMSFE_validacion = RMSFE,
      MAE_validacion = MAE,
      MASE_validacion = MASE,
      AIC_validacion = AIC,
      AICc_validacion = AICc,
      BIC_validacion = BIC,
      
      ranking_RMSFE_validacion = ranking_RMSFE,
      ranking_MAE_validacion = ranking_MAE,
      ranking_MASE_validacion = ranking_MASE,
      ranking_AICc_validacion = ranking_AICc,
      ranking_BIC_validacion = ranking_BIC,
      
      ljung_p_value_validacion = ljung_p_value,
      diagnostico_ljung_validacion = diagnostico_ljung,
      
      rmsfe_minimo_validacion = rmsfe_minimo,
      margen_rmsfe_validacion = margen_rmsfe,
      limite_rmsfe_candidato_validacion = limite_rmsfe_candidato,
      diferencia_relativa_RMSFE_validacion = diferencia_relativa_RMSFE,
      
      dm_stat_vs_mejor_rmsfe_validacion = dm_stat_vs_mejor_rmsfe,
      dm_p_value_vs_mejor_rmsfe_validacion = dm_p_value_vs_mejor_rmsfe,
      diagnostico_dm_validacion = diagnostico_dm
    )
  
  # ----------------------------------------------------------
  # Tabla completa de métricas de test, ordenada según
  # la selección ex ante.
  # ----------------------------------------------------------
  
  tabla_resultados_finales_completa <- resumen_final_todos$tabla_resultados %>%
    rename(
      seleccionado_modelo_limpio_test = seleccionado_modelo_limpio,
      candidato_modelo_limpio_test = candidato_modelo_limpio,
      residuos_ok_modelo_limpio_test = residuos_ok_modelo_limpio,
      prioridad_residuos_modelo_limpio_test = prioridad_residuos_modelo_limpio,
      criterio_modelo_limpio_test = criterio_modelo_limpio,
      rmsfe_minimo_test = rmsfe_minimo,
      margen_rmsfe_test = margen_rmsfe,
      limite_rmsfe_candidato_test = limite_rmsfe_candidato,
      diferencia_relativa_RMSFE_test = diferencia_relativa_RMSFE,
      dm_stat_vs_mejor_rmsfe_test = dm_stat_vs_mejor_rmsfe,
      dm_p_value_vs_mejor_rmsfe_test = dm_p_value_vs_mejor_rmsfe,
      diagnostico_dm_test = diagnostico_dm
    ) %>%
    left_join(
      tabla_orden_ex_ante,
      by = "id_modelo"
    ) %>%
    mutate(
      tipo_seleccion = "ex_ante",
      periodo_metricas = "test_2m_8_actualizaciones",
      seleccionado_ex_ante_test = coalesce(seleccionado_modelo_limpio_ex_ante, FALSE),
      top3_ex_ante_test = id_modelo %in% nombres_top3_modelos_ex_ante
    ) %>%
    arrange(
      is.na(orden_ex_ante),
      orden_ex_ante,
      RMSFE,
      MAE
    )
  
  metricas_top3_ex_ante_test <- tabla_resultados_finales_completa %>%
    filter(top3_ex_ante_test) %>%
    arrange(orden_ex_ante)
  
  # ----------------------------------------------------------
  # Para mantener compatibilidad con el resto del script:
  # conservamos también la tabla y predicciones SOLO del mejor
  # modelo seleccionado ex ante.
  # ----------------------------------------------------------
  
  tabla_predicciones_finales <- tabla_predicciones_finales_todos %>%
    filter(id_modelo == mejor_modelo$id_modelo[1])
  
  resumen_final <- resumir_metricas_modelo_limpio(
    tabla_predicciones = tabla_predicciones_finales,
    y_train = y_train_final,
    m_mase = m_mase,
    alpha = alpha,
    margen_rmsfe = margen_rmsfe,
    min_obs_dm = min_obs_dm
  )

  list(
    tipo = "ex_ante",
    fecha_ruptura = fecha_ruptura,
    usar_correccion_sarima = usar_correccion_sarima,
    n_meses_train_final = n_meses_train_final,
    diagnostico_estacionariedad = diagnostico_estacionariedad,
    variables_no_estacionarias = variables_no_estacionarias,
    tabla_errores_sarima = tabla_errores_sarima,
    errores_sarima_seleccionados = errores_sarima,
    combinaciones = combinaciones,
    
    tablas_validacion = tablas_validacion,
    tabla_predicciones_validacion = tabla_predicciones_validacion,
    tabla_resultados_validacion = tabla_resultados_validacion,
    tabla_validacion_media = tabla_validacion_media,
    mejor_modelo = mejor_modelo,
    seleccion_validacion_media = seleccion_validacion_media,
    
    nombres_top3_modelos_ex_ante = nombres_top3_modelos_ex_ante,
    
    tabla_predicciones_finales = tabla_predicciones_finales,
    tabla_resultados_finales = resumen_final$tabla_resultados,
    seleccion_final = resumen_final$seleccion_modelo_limpio,
    
    tabla_predicciones_finales_todos = tabla_predicciones_finales_todos,
    tabla_resultados_finales_completa = tabla_resultados_finales_completa,
    tabla_final_ex_ante_test = tabla_resultados_finales_completa,
    metricas_top3_ex_ante_test = metricas_top3_ex_ante_test,
    seleccion_final_todos = resumen_final_todos$seleccion_modelo_limpio
  )
}


# ============================================================
# 13. PARTICIÓN Y EJECUCIÓN DE EVENTOS MIDAS
# ============================================================
# Unión metodológica con regresión dinámica:
#   - Envoltorio por evento.
#   - Ejecución de tres fechas de ruptura.
#   - Tablas de resumen y gráficos por evento.
# ============================================================

seleccionar_midas_evento_2m_9_10 <- function(objeto_midas,
                                             fecha_ruptura,
                                             nombre_evento = NULL,
                                             regresores_candidatos,
                                             max_regresores = 4,
                                             meses_lag_midas = 3,
                                             semanas_por_mes = 4,
                                             incluir_ar = TRUE,
                                             desplazamiento_retardos = 0,
                                             usar_correccion_sarima = TRUE,
                                             n_errores_sarima = 2,
                                             forzar_incluir_auto_sarima = TRUE,
                                             m_sarima = 12,
                                             alpha = 0.05,
                                             ljung_lag = 24,
                                             margen_rmsfe = 0.10,
                                             min_obs_dm = 10,
                                             m_mase = 12,
                                             comprobar_estacionariedad = TRUE,
                                             detener_si_no_estacionaria = FALSE,
                                             ejecutar_ex_post = TRUE,
                                             ejecutar_ex_ante = TRUE) {

  resultado_ex_post <- NULL
  resultado_ex_ante <- NULL

  fecha_inicio_test <- tsibble::yearmonth(fecha_ruptura) + 1
  fecha_ruptura_ym <- tsibble::yearmonth(fecha_ruptura)

  cat("\n")
  cat("############################################################\n")
  cat("EVENTO MIDAS:", ifelse(is.null(nombre_evento), as.character(fecha_ruptura_ym), nombre_evento), "\n")
  cat("Fecha de ruptura:", as.character(fecha_ruptura_ym), "\n")
  cat("Fecha inicio test ex post:", as.character(fecha_inicio_test), "\n")
  cat("Regresores:", paste(regresores_candidatos, collapse = ", "), "\n")
  cat("############################################################\n\n")

  if (ejecutar_ex_post) {
    resultado_ex_post <- seleccionar_midas_automatico_2_m_ex_post(
      objeto_midas = objeto_midas,
      regresores_candidatos = regresores_candidatos,
      fecha_inicio_test = fecha_inicio_test,
      max_regresores = max_regresores,
      meses_lag_midas = meses_lag_midas,
      semanas_por_mes = semanas_por_mes,
      incluir_ar = incluir_ar,
      desplazamiento_retardos = desplazamiento_retardos,
      alpha = alpha,
      ljung_lag = ljung_lag,
      margen_rmsfe = margen_rmsfe,
      min_obs_dm = min_obs_dm,
      m_mase = m_mase,
      usar_correccion_sarima = usar_correccion_sarima,
      n_errores_sarima = n_errores_sarima,
      forzar_incluir_auto_sarima = forzar_incluir_auto_sarima,
      m_sarima = m_sarima,
      comprobar_estacionariedad = comprobar_estacionariedad,
      detener_si_no_estacionaria = detener_si_no_estacionaria
    )
  }

  if (ejecutar_ex_ante) {
    resultado_ex_ante <- seleccionar_midas_automatico_2_m_ex_ante(
      objeto_midas = objeto_midas,
      regresores_candidatos = regresores_candidatos,
      fecha_ruptura = fecha_ruptura_ym,
      max_regresores = max_regresores,
      meses_lag_midas = meses_lag_midas,
      semanas_por_mes = semanas_por_mes,
      incluir_ar = incluir_ar,
      desplazamiento_retardos = desplazamiento_retardos,
      usar_correccion_sarima = usar_correccion_sarima,
      n_errores_sarima = n_errores_sarima,
      forzar_incluir_auto_sarima = forzar_incluir_auto_sarima,
      m_sarima = m_sarima,
      alpha = alpha,
      ljung_lag = ljung_lag,
      margen_rmsfe = margen_rmsfe,
      min_obs_dm = min_obs_dm,
      m_mase = m_mase,
      comprobar_estacionariedad = comprobar_estacionariedad,
      detener_si_no_estacionaria = detener_si_no_estacionaria
    )
  }

  pred_mejor_ex_post <- NULL
  pred_mejor_ex_ante <- NULL

  if (!is.null(resultado_ex_post)) {
    id_mejor_ex_post <- resultado_ex_post$mejor_modelo$id_modelo[1]
    pred_mejor_ex_post <- resultado_ex_post$tabla_predicciones %>%
      filter(id_modelo == id_mejor_ex_post) %>%
      arrange(actualizacion)
  }

  if (!is.null(resultado_ex_ante)) {
    id_mejor_ex_ante <- resultado_ex_ante$mejor_modelo$id_modelo[1]
    pred_mejor_ex_ante <- resultado_ex_ante$tabla_predicciones_finales %>%
      filter(id_modelo == id_mejor_ex_ante) %>%
      arrange(actualizacion)
  }

  comparacion_pred_real <- bind_rows(
    if (!is.null(pred_mejor_ex_post)) {
      pred_mejor_ex_post %>%
        mutate(enfoque = "ex_post")
    },
    if (!is.null(pred_mejor_ex_ante)) {
      pred_mejor_ex_ante %>%
        mutate(enfoque = "ex_ante")
    }
  )

  grafico_pred_real_evento <- NULL
  grafico_error_evento <- NULL

  if (nrow(comparacion_pred_real) > 0) {
    grafico_pred_real_evento <- comparacion_pred_real %>%
      ggplot(aes(x = actualizacion)) +
      geom_line(aes(y = real, colour = "Valor real"), linewidth = 1) +
      geom_line(aes(y = prediccion, colour = enfoque), linewidth = 1, linetype = "dashed") +
      geom_point(aes(y = prediccion, colour = enfoque)) +
      scale_x_continuous(breaks = 1:8) +
      labs(
        title = paste(
          "Valores reales vs predicciones MIDAS -",
          ifelse(is.null(nombre_evento), as.character(fecha_ruptura_ym), nombre_evento)
        ),
        subtitle = "Ex post y ex ante evaluados sobre los dos meses posteriores a la ruptura",
        x = "Actualización semanal",
        y = "Tasa logarítmica mensual",
        colour = ""
      )

    grafico_error_evento <- comparacion_pred_real %>%
      ggplot(aes(x = actualizacion, y = error, colour = enfoque)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = 1:8) +
      labs(
        title = paste(
          "Error de predicción MIDAS -",
          ifelse(is.null(nombre_evento), as.character(fecha_ruptura_ym), nombre_evento)
        ),
        subtitle = "Error = real - predicción",
        x = "Actualización semanal",
        y = "Error",
        colour = ""
      )
  }

  list(
    nombre_evento = nombre_evento,
    fecha_ruptura = fecha_ruptura_ym,
    fecha_inicio_test = fecha_inicio_test,
    regresores_candidatos = regresores_candidatos,
    resultado_ex_post = resultado_ex_post,
    resultado_ex_ante = resultado_ex_ante,
    comparacion_pred_real = comparacion_pred_real,
    grafico_pred_real_evento = grafico_pred_real_evento,
    grafico_error_evento = grafico_error_evento,
    diagnostico_estacionariedad = if (!is.null(resultado_ex_ante)) resultado_ex_ante$diagnostico_estacionariedad else resultado_ex_post$diagnostico_estacionariedad,
    variables_no_estacionarias = if (!is.null(resultado_ex_ante)) resultado_ex_ante$variables_no_estacionarias else resultado_ex_post$variables_no_estacionarias
  )
}


ejecutar_midas_tres_eventos_2m_9_10 <- function(objeto_midas,
                                                regresores_candidatos,
                                                max_regresores = 4,
                                                meses_lag_midas = 3,
                                                semanas_por_mes = 4,
                                                incluir_ar = TRUE,
                                                desplazamiento_retardos = 0,
                                                usar_correccion_sarima = TRUE,
                                                n_errores_sarima = 2,
                                                forzar_incluir_auto_sarima = TRUE,
                                                m_sarima = 12,
                                                alpha = 0.05,
                                                ljung_lag = 24,
                                                margen_rmsfe = 0.10,
                                                min_obs_dm = 10,
                                                m_mase = 12,
                                                comprobar_estacionariedad = TRUE,
                                                detener_si_no_estacionaria = FALSE,
                                                puntos_ruptura = NULL,
                                                continuar_si_error = TRUE,
                                                ejecutar_ex_post = TRUE,
                                                ejecutar_ex_ante = TRUE) {

  if (is.null(puntos_ruptura)) {
    puntos_ruptura <- tibble(
      evento = c(
        "Post-confinamiento",
        "Repunte inflacionario de 2021",
        "Inicio invasión rusa en Ucrania"
      ),
      fecha_ruptura = c(
        "2020 Mar",
        "2021 Mar",
        "2022 Feb"
      )
    )
  }

  resultados_eventos <- pmap(
    list(puntos_ruptura$evento, puntos_ruptura$fecha_ruptura),
    function(evento, fecha_ruptura) {

      if (continuar_si_error) {
        tryCatch(
          seleccionar_midas_evento_2m_9_10(
            objeto_midas = objeto_midas,
            fecha_ruptura = fecha_ruptura,
            nombre_evento = evento,
            regresores_candidatos = regresores_candidatos,
            max_regresores = max_regresores,
            meses_lag_midas = meses_lag_midas,
            semanas_por_mes = semanas_por_mes,
            incluir_ar = incluir_ar,
            desplazamiento_retardos = desplazamiento_retardos,
            usar_correccion_sarima = usar_correccion_sarima,
            n_errores_sarima = n_errores_sarima,
            forzar_incluir_auto_sarima = forzar_incluir_auto_sarima,
            m_sarima = m_sarima,
            alpha = alpha,
            ljung_lag = ljung_lag,
            margen_rmsfe = margen_rmsfe,
            min_obs_dm = min_obs_dm,
            m_mase = m_mase,
            comprobar_estacionariedad = comprobar_estacionariedad,
            detener_si_no_estacionaria = detener_si_no_estacionaria,
            ejecutar_ex_post = ejecutar_ex_post,
            ejecutar_ex_ante = ejecutar_ex_ante
          ),
          error = function(e) {
            list(
              nombre_evento = evento,
              fecha_ruptura = tsibble::yearmonth(fecha_ruptura),
              error = TRUE,
              mensaje_error = conditionMessage(e)
            )
          }
        )
      } else {
        seleccionar_midas_evento_2m_9_10(
          objeto_midas = objeto_midas,
          fecha_ruptura = fecha_ruptura,
          nombre_evento = evento,
          regresores_candidatos = regresores_candidatos,
          max_regresores = max_regresores,
          meses_lag_midas = meses_lag_midas,
          semanas_por_mes = semanas_por_mes,
          incluir_ar = incluir_ar,
          desplazamiento_retardos = desplazamiento_retardos,
          usar_correccion_sarima = usar_correccion_sarima,
          n_errores_sarima = n_errores_sarima,
          forzar_incluir_auto_sarima = forzar_incluir_auto_sarima,
          m_sarima = m_sarima,
          alpha = alpha,
          ljung_lag = ljung_lag,
          margen_rmsfe = margen_rmsfe,
          min_obs_dm = min_obs_dm,
          m_mase = m_mase,
          comprobar_estacionariedad = comprobar_estacionariedad,
          detener_si_no_estacionaria = detener_si_no_estacionaria,
          ejecutar_ex_post = ejecutar_ex_post,
          ejecutar_ex_ante = ejecutar_ex_ante
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
        fila_ex_post <- if (!is.null(res$resultado_ex_post)) {
          res$resultado_ex_post$mejor_modelo %>%
            mutate(enfoque = "ex_post")
        } else tibble()

        fila_ex_ante <- if (!is.null(res$resultado_ex_ante)) {
          res$resultado_ex_ante$tabla_resultados_finales %>%
            mutate(enfoque = "ex_ante")
        } else tibble()

        bind_rows(fila_ex_post, fila_ex_ante) %>%
          mutate(
            evento = evento,
            fecha_ruptura = as.character(res$fecha_ruptura),
            fecha_inicio_test = as.character(res$fecha_inicio_test),
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
          fecha_ruptura = as.character(res$fecha_ruptura),
          mensaje_error = res$mensaje_error
        )
      }
    )
  }

  cat("\n")
  cat("============================================================\n")
  cat("RESUMEN FINAL: MIDAS 9/10 EN EVENTOS\n")
  cat("============================================================\n\n")

  if (nrow(tabla_resumen_eventos) > 0) {
    print(tabla_resumen_eventos, n = Inf, width = Inf)
  }

  if (nrow(tabla_errores_eventos) > 0) {
    cat("\nEventos con error:\n")
    print(tabla_errores_eventos, n = Inf, width = Inf)
  }

  cat("\n")
  cat("Nota metodológica: ex ante selecciona modelo con validación previa y evalúa\n")
  cat("las predicciones finales en los dos meses posteriores a la ruptura. Ex post\n")
  cat("selecciona directamente sobre esas ocho actualizaciones del periodo posterior.\n")
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
# 14. EJEMPLO DE USO ORGANIZADO
# ============================================================
# Mantiene la lógica del script MIDAS, pero se organiza como el de regresión
# dinámica:
#   A. Carga de datos y creación del objeto MIDAS.
#   B. Ejecución de un evento ex post y ex ante.
#   C. Ejecución de los tres eventos.
#   D. Visualización de resultados.
# ============================================================

# ------------------------------------------------------------
# 14.A. Carga de datos y construcción del objeto MIDAS
# ------------------------------------------------------------

setwd("C:/Users/rober/Desktop/UNIR/TFM/ubicacion_datos/datos_midas/desde_2005")

df_ipc_m <- read_excel("df_ipc_m.xlsx") %>%
  mutate(fecha_mensual = yearmonth(fecha_mensual)) %>%
  arrange(fecha_mensual)

df_br_4sem <- read_excel("df_br_4sem/dollars_per_barrel_tasa_log.xlsx")

df_neer_4sem <- read_excel("df_neer_4sem/neer_aprox_tasa_log.xlsx")

df_omie_1_28 <- read_excel("df_omie_1_28/precio_marginal_tasa_log.xlsx") %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA_real_, .x)))

df_gasolina_4sem <- read_excel("df_gas_die_4sem/gasolina_95_tasa_log.xlsx")

df_diesel_4sem <- read_excel("df_gas_die_4sem/diesel_tasa_log.xlsx")

lista_regresores <- list(
  brent = df_br_4sem,
  neer = df_neer_4sem,
  omie = df_omie_1_28,
  gasolina = df_gasolina_4sem,
  diesel = df_diesel_4sem
)

frecuencias_regresores <- c(
  brent = 4,
  neer = 4,
  omie = 28,
  gasolina = 4,
  diesel = 4
)

#Quito OMIE para no complicar tanto el ajuste (tiene 28 registros por mes)
regresores_candidatos <- c("neer", "gasolina", "brent", "diesel")

objeto_midas <- construir_objeto_midas_basico(
  df_y = df_ipc_m,
  lista_regresores = lista_regresores,
  frecuencias_regresores = frecuencias_regresores,
  variable_y = "y_ipc_general",
  fecha_y = "fecha_mensual"
)

resultados_midas_eventos_prueba <- ejecutar_midas_tres_eventos_2m_9_10(
  objeto_midas = objeto_midas,
  regresores_candidatos = c("brent"),
  max_regresores = 1,
  meses_lag_midas = 1,
  incluir_ar = TRUE,
  desplazamiento_retardos = 0,
  usar_correccion_sarima = FALSE,
  comprobar_estacionariedad = FALSE,
  detener_si_no_estacionaria = FALSE,
  continuar_si_error = TRUE,
  ejecutar_ex_post = TRUE,
  ejecutar_ex_ante = TRUE
)

View(resultados_midas_eventos_prueba$resultados_eventos$"Inicio invasión rusa en Ucrania"$resultado_ex_post$tabla_resultados)


# ------------------------------------------------------------
# 14.B. Ejecución de un evento: post-confinamiento
# ------------------------------------------------------------

resultado_midas_post_confinamiento <- seleccionar_midas_evento_2m_9_10(
  objeto_midas = objeto_midas,
  fecha_ruptura = "2020 Mar",
  nombre_evento = "Post-confinamiento",
  regresores_candidatos = regresores_candidatos,
  max_regresores = 3,
  meses_lag_midas = c(1, 2),
  incluir_ar = c(TRUE),
  desplazamiento_retardos = c(0, 1),
  usar_correccion_sarima = TRUE,
  n_errores_sarima = 2,
  forzar_incluir_auto_sarima = TRUE,
  m_sarima = 12,
  comprobar_estacionariedad = TRUE,
  detener_si_no_estacionaria = FALSE,
  ejecutar_ex_post = TRUE,
  ejecutar_ex_ante = TRUE
)



# ------------------------------------------------------------
# 14.C. Ejecución de los tres eventos
# ------------------------------------------------------------

#Versión asequible a nivel de carga computacional: 
resultados_midas_eventos_prueba <- ejecutar_midas_tres_eventos_2m_9_10(
  objeto_midas = objeto_midas,
  regresores_candidatos = regresores_candidatos,
  max_regresores = 3,
  meses_lag_midas = 1,
  incluir_ar = TRUE,
  desplazamiento_retardos = 0,
  usar_correccion_sarima = TRUE,
  n_errores_sarima = 2,
  forzar_incluir_auto_sarima = TRUE,
  m_sarima = 12,
  comprobar_estacionariedad = TRUE,
  detener_si_no_estacionaria = FALSE,
  continuar_si_error = TRUE,
  ejecutar_ex_post = TRUE,
  ejecutar_ex_ante = TRUE
)

#Versión no asequible a nivel de carga computacional: 
resultados_midas_eventos <- ejecutar_midas_tres_eventos_2m_9_10(
  objeto_midas = objeto_midas,
  regresores_candidatos = regresores_candidatos,
  max_regresores = 3,
  meses_lag_midas = c(1, 2),
  incluir_ar = c(TRUE),
  desplazamiento_retardos = c(0, 1),
  usar_correccion_sarima = TRUE,
  n_errores_sarima = 2,
  forzar_incluir_auto_sarima = TRUE,
  m_sarima = 12,
  comprobar_estacionariedad = TRUE,
  detener_si_no_estacionaria = FALSE,
  continuar_si_error = FALSE,
  ejecutar_ex_post = TRUE,
  ejecutar_ex_ante = TRUE
)

# ------------------------------------------------------------
# 14.D. Visualización y consulta de resultados
# ------------------------------------------------------------

View(resultados_midas_eventos_prueba$resultados_eventos$"Post-confinamiento"$diagnostico_estacionariedad)
View(resultados_midas_eventos_prueba$resultados_eventos$"Repunte inflacionario de 2021"$diagnostico_estacionariedad )
names(resultados_midas_eventos_prueba$resultados_eventos$"Inicio invasión rusa en Ucrania")

View(resultados_midas_eventos_prueba$tabla_resumen_eventos)

resultado_post_confinamiento <- resultados_midas_eventos$resultados_eventos$`Post-confinamiento`
resultado_repunte_2021 <- resultados_midas_eventos$resultados_eventos$`Repunte inflacionario de 2021`
resultado_ucrania_2022 <- resultados_midas_eventos$resultados_eventos$`Inicio invasión rusa en Ucrania`

# Diagnóstico de estacionariedad por evento
View(resultado_post_confinamiento$diagnostico_estacionariedad)
View(resultado_repunte_2021$diagnostico_estacionariedad)
View(resultado_ucrania_2022$diagnostico_estacionariedad)

# Gráficos comparativos ex post / ex ante
resultado_post_confinamiento$grafico_pred_real_evento
resultado_repunte_2021$grafico_pred_real_evento
resultado_ucrania_2022$grafico_pred_real_evento

resultado_post_confinamiento$grafico_error_evento
resultado_repunte_2021$grafico_error_evento
resultado_ucrania_2022$grafico_error_evento

# Tablas ex post y ex ante del primer evento
View(resultado_post_confinamiento$resultado_ex_post$tabla_resultados)
View(resultado_post_confinamiento$resultado_ex_post$tabla_predicciones)

View(resultado_post_confinamiento$resultado_ex_ante$tabla_validacion_media)
View(resultado_post_confinamiento$resultado_ex_ante$tabla_predicciones_finales)
View(resultado_post_confinamiento$resultado_ex_ante$tabla_resultados_finales)

# Comprobación de estructuras SARIMA usadas
resultado_post_confinamiento$resultado_ex_post$combinaciones %>% count(id_error)
resultado_post_confinamiento$resultado_ex_ante$combinaciones %>% count(id_error)

# Comparación de predicción real del evento
View(resultado_post_confinamiento$comparacion_pred_real)

# Errores de ejecución, si los hubiera
resultados_midas_eventos$tabla_errores_eventos

#Exportación de tablas ex ante durante el periodo de test
midas_2020_exante_completa <- resultados_midas_eventos$resultados_eventos$`Post-confinamiento`$resultado_ex_ante$tabla_final_ex_ante_test
midas_2021_exante_completa <- resultados_midas_eventos$resultados_eventos$`Repunte inflacionario de 2021`$resultado_ex_ante$tabla_final_ex_ante_test
midas_2022_exante_completa <- resultados_midas_eventos$resultados_eventos$`Inicio invasión rusa en Ucrania`$resultado_ex_ante$tabla_final_ex_ante_test

library(writexl)

ruta_salida <- "C:/Users/rober/Desktop/UNIR/TFM/ubicacion_datos/tablas_para_visualizaciones"

write_xlsx(
  midas_2020_exante_completa,
  file.path(ruta_salida, "midas_2020_exante_test_completa.xlsx")
)

write_xlsx(
  midas_2021_exante_completa,
  file.path(ruta_salida, "midas_2021_exante_test_completa.xlsx")
)

write_xlsx(
  midas_2022_exante_completa,
  file.path(ruta_salida, "midas_2022_exante_test_completa.xlsx")
)
