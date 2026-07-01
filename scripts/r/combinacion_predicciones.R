# ============================================================
# COMBINACIÓN DE PREDICCIONES SIN ForecastComb
# SARIMA + ETS + REGRESIÓN DINÁMICA + MIDAS
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(writexl)
library(ggplot2)
library(scales)

# ============================================================
# 0. CARPETAS
# ============================================================

carpeta_datos <- "C:/Users/rober/Desktop/UNIR/TFM/ubicacion_datos/comb_preds"

carpeta_salida <- file.path(carpeta_datos, "salidas_combinacion")

if (!dir.exists(carpeta_salida)) {
  dir.create(carpeta_salida, recursive = TRUE)
}

# ============================================================
# 1. ARCHIVOS A IMPORTAR
# ============================================================
# Rellena aquí los nombres reales de tus archivos.
# La idea es una fila por evento y enfoque.

archivos_predicciones <- tibble::tribble(
  ~evento,        ~fecha_ruptura, ~enfoque,   ~sarima,                         ~ets,                         ~regdin,                         ~midas,
  
  "marzo_2020",   "2020-03-01",   "ex_post",  "SARIMA/pred_2020_sarima_expost.xlsx",   "ETS/pred_2020_ets_expost.xlsx",   "regresion_dinamica/pred_2020_regdin_expost.xlsx",   "MIDAS/2020_resultados_midas_expost_exante_resumen.xlsx",
  "marzo_2020",   "2020-03-01",   "ex_ante",  "SARIMA/pred_2020_sarima_exante.xlsx",   "ETS/pred_2020_ets_exante.xlsx",   "regresion_dinamica/pred_2020_regdin_exante.xlsx",   "MIDAS/2020_resultados_midas_expost_exante_resumen.xlsx",
  
  "marzo_2021",   "2021-03-01",   "ex_post",  "SARIMA/pred_2021_sarima_expost.xlsx",   "ETS/pred_2021_ets_expost.xlsx",   "regresion_dinamica/pred_2021_regdin_expost.xlsx",   "MIDAS/2021_resultados_midas_expost_exante_resumen.xlsx",
  "marzo_2021",   "2021-03-01",   "ex_ante",  "SARIMA/pred_2021_sarima_exante.xlsx",   "ETS/pred_2021_ets_exante.xlsx",   "regresion_dinamica/pred_2021_regdin_exante.xlsx",   "MIDAS/2021_resultados_midas_expost_exante_resumen.xlsx",
  
  "febrero_2022", "2022-02-01",   "ex_post",  "SARIMA/pred_2022_sarima_expost.xlsx",   "ETS/pred_2022_ets_expost.xlsx",   "regresion_dinamica/pred_2022_regdin_expost.xlsx",   "MIDAS/2022_resultados_midas_expost_exante_resumen.xlsx",
  "febrero_2022", "2022-02-01",   "ex_ante",  "SARIMA/pred_2022_sarima_exante.xlsx",   "ETS/pred_2022_ets_exante.xlsx",   "regresion_dinamica/pred_2022_regdin_exante.xlsx",   "MIDAS/2022_resultados_midas_expost_exante_resumen.xlsx"
) %>%
  mutate(
    sarima = file.path(carpeta_datos, sarima),
    ets    = file.path(carpeta_datos, ets),
    regdin = file.path(carpeta_datos, regdin),
    midas  = file.path(carpeta_datos, midas)
  )

# ============================================================
# 2. FUNCIONES AUXILIARES SENCILLAS
# ============================================================

normalizar_fecha <- function(x) {
  
  if (inherits(x, "Date")) {
    return(x)
  }
  
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1970-01-01"))
  }
  
  as.Date(x)
}


buscar_columna <- function(df, opciones) {
  
  encontrada <- opciones[opciones %in% names(df)]
  
  if (length(encontrada) == 0) {
    return(NA_character_)
  }
  
  encontrada[1]
}


calcular_metricas <- function(real, prediccion) {
  
  error <- real - prediccion
  
  tibble(
    RMSFE = sqrt(mean(error^2, na.rm = TRUE)),
    MAE = mean(abs(error), na.rm = TRUE),
    error_medio = mean(error, na.rm = TRUE),
    n = sum(!is.na(error))
  )
}

# ============================================================
# 3. IMPORTAR MEJOR MODELO DE SARIMA, ETS Y REGRESIÓN DINÁMICA
# ============================================================
# Se asume que el mejor modelo aparece en primera posición en el Excel.

leer_mejor_modelo_mensual <- function(ruta, familia) {
  
  if (!file.exists(ruta)) {
    stop("No existe el archivo: ", ruta)
  }
  
  df <- read_excel(ruta)
  
  col_modelo <- buscar_columna(df, c(".model", "modelo", "id_modelo"))
  col_fecha  <- buscar_columna(df, c("fecha_mensual", "fecha", "fecha_objetivo"))
  col_pred   <- buscar_columna(df, c("prediccion", ".mean", "pred", "y_pred"))
  col_real   <- buscar_columna(df, c("real", "y_real", "observado", "y_ipc_general"))
  
  if (is.na(col_modelo)) stop("No encuentro columna de modelo en: ", ruta)
  if (is.na(col_fecha))  stop("No encuentro columna de fecha en: ", ruta)
  if (is.na(col_pred))   stop("No encuentro columna de predicción en: ", ruta)
  
  mejor_modelo <- df %>%
    filter(!is.na(.data[[col_modelo]])) %>%
    pull(.data[[col_modelo]]) %>%
    first()
  
  df %>%
    filter(.data[[col_modelo]] == mejor_modelo) %>%
    transmute(
      fecha_mensual = normalizar_fecha(.data[[col_fecha]]),
      familia = familia,
      modelo = as.character(.data[[col_modelo]]),
      prediccion = as.numeric(.data[[col_pred]]),
      real = if (!is.na(col_real)) as.numeric(.data[[col_real]]) else NA_real_
    ) %>%
    filter(!is.na(fecha_mensual), !is.na(prediccion))
}

# ============================================================
# 4. IMPORTAR MEJOR MODELO MIDAS
# ============================================================
# En MIDAS las predicciones están en pestañas distintas:
#   - Ex ante: "predicciones_finales_ex_ante"
#   - Ex post: "predicciones_ex_post"
#
# Se toma el primer modelo que aparece en la tabla.
# Después, como MIDAS genera predicciones semanales, se toman
# las posiciones 4 y 8 de ese modelo, que corresponden al cierre mensual.
# ============================================================

leer_mejor_modelo_midas <- function(ruta, enfoque) {
  
  if (!file.exists(ruta)) {
    stop("No existe el archivo MIDAS: ", ruta)
  }
  
  if (enfoque == "ex_ante") {
    hoja_predicciones <- "predicciones_finales_ex_ante"
  } else if (enfoque == "ex_post") {
    hoja_predicciones <- "predicciones_ex_post"
  } else {
    stop("El enfoque debe ser 'ex_ante' o 'ex_post'.")
  }
  
  hojas_disponibles <- excel_sheets(ruta)
  
  if (!hoja_predicciones %in% hojas_disponibles) {
    stop(
      "No encuentro la hoja '", hoja_predicciones,
      "' en el archivo MIDAS: ", ruta
    )
  }
  
  df <- read_excel(ruta, sheet = hoja_predicciones)
  
  col_modelo <- buscar_columna(df, c("id_modelo", ".model", "modelo"))
  col_fecha  <- buscar_columna(df, c("fecha_objetivo", "fecha_mensual", "fecha"))
  col_pred   <- buscar_columna(df, c("prediccion", ".mean", "pred", "y_pred"))
  col_real   <- buscar_columna(df, c("real", "y_real", "observado", "y_ipc_general"))
  
  if (is.na(col_modelo)) stop("No encuentro columna de modelo MIDAS en: ", ruta)
  if (is.na(col_fecha))  stop("No encuentro columna de fecha MIDAS en: ", ruta)
  if (is.na(col_pred))   stop("No encuentro columna de predicción MIDAS en: ", ruta)
  
  # Se toma el primer modelo que aparece.
  # Según tu planteamiento, ese es el mejor modelo.
  mejor_modelo <- df %>%
    filter(!is.na(.data[[col_modelo]])) %>%
    pull(.data[[col_modelo]]) %>%
    first()
  
  df_midas <- df %>%
    filter(.data[[col_modelo]] == mejor_modelo) %>%
    mutate(
      fecha_mensual = normalizar_fecha(.data[[col_fecha]])
    ) %>%
    arrange(fecha_mensual) %>%
    mutate(
      posicion_prediccion = row_number()
    )
  
  # Nos quedamos solo con las posiciones 4 y 8:
  # cierre del primer mes y cierre del segundo mes.
  df_midas %>%
    filter(posicion_prediccion %in% c(4, 8)) %>%
    transmute(
      fecha_mensual = fecha_mensual,
      familia = "MIDAS",
      modelo = as.character(.data[[col_modelo]]),
      prediccion = as.numeric(.data[[col_pred]]),
      real = if (!is.na(col_real)) as.numeric(.data[[col_real]]) else NA_real_
    ) %>%
    filter(
      !is.na(fecha_mensual),
      !is.na(prediccion)
    )
}

# ============================================================
# 5. CREAR TABLA ANCHA PARA UN EVENTO Y UN ENFOQUE
# ============================================================

crear_tabla_predicciones <- function(evento,
                                     fecha_ruptura,
                                     enfoque,
                                     ruta_sarima,
                                     ruta_ets,
                                     ruta_regdin,
                                     ruta_midas) {
  
  pred_sarima <- leer_mejor_modelo_mensual(ruta_sarima, "SARIMA")
  pred_ets    <- leer_mejor_modelo_mensual(ruta_ets, "ETS")
  pred_regdin <- leer_mejor_modelo_mensual(ruta_regdin, "REGDIN")
  pred_midas  <- leer_mejor_modelo_midas(ruta_midas, enfoque)
  
  predicciones_largas <- bind_rows(
    pred_sarima,
    pred_ets,
    pred_regdin,
    pred_midas
  )
  
  reales <- predicciones_largas %>%
    filter(!is.na(real)) %>%
    group_by(fecha_mensual) %>%
    summarise(real = first(real), .groups = "drop")
  
  predicciones_largas %>%
    select(fecha_mensual, familia, prediccion) %>%
    pivot_wider(
      names_from = familia,
      values_from = prediccion
    ) %>%
    left_join(reales, by = "fecha_mensual") %>%
    mutate(
      evento = evento,
      fecha_ruptura = as.Date(fecha_ruptura),
      enfoque = enfoque
    ) %>%
    select(
      evento,
      fecha_ruptura,
      enfoque,
      fecha_mensual,
      real,
      SARIMA,
      ETS,
      REGDIN,
      MIDAS
    ) %>%
    arrange(fecha_mensual)
}

# ============================================================
# 6. FUNCIÓN PARA COMBINAR PREDICCIONES
# ============================================================

combinar_predicciones <- function(tabla,
                                  modelos,
                                  nombre_combinacion) {
  
  modelos <- modelos[modelos %in% names(tabla)]
  
  datos <- tabla %>%
    select(fecha_mensual, real, all_of(modelos)) %>%
    filter(!is.na(real)) %>%
    filter(if_all(all_of(modelos), ~ !is.na(.x) & is.finite(.x)))
  
  if (nrow(datos) == 0) {
    warning("No hay datos suficientes para: ", nombre_combinacion)
    return(NULL)
  }
  
  observed_vector <- datos$real
  
  prediction_matrix <- datos %>%
    select(all_of(modelos)) %>%
    as.matrix()
  
  # Escala del MASE.
  # Se usa como referencia el error naive de las observaciones reales
  # disponibles en la combinación.
  if (length(observed_vector) >= 2) {
    escala_mase <- mean(abs(diff(observed_vector)), na.rm = TRUE)
  } else {
    escala_mase <- NA_real_
  }
  
  if (!is.finite(escala_mase) || escala_mase == 0) {
    escala_mase <- NA_real_
  }
  
  # ----------------------------------------------------------
  # 1. Media simple
  # ----------------------------------------------------------
  
  pred_media_simple <- rowMeans(prediction_matrix)
  
  # ----------------------------------------------------------
  # 2. Media ponderada por RMSFE inverso
  # ----------------------------------------------------------
  # Aquí el RMSFE se calcula con las observaciones disponibles.
  # Si luego tienes RMSFE de validación, se puede sustituir fácilmente.
  
  rmsfe_modelos <- apply(
    prediction_matrix,
    2,
    function(pred) sqrt(mean((observed_vector - pred)^2, na.rm = TRUE))
  )
  
  rmsfe_sarima <- if ("SARIMA" %in% names(rmsfe_modelos)) {
    as.numeric(rmsfe_modelos["SARIMA"])
  } else {
    NA_real_
  }
  
  rmsfe_ets <- if ("ETS" %in% names(rmsfe_modelos)) {
    as.numeric(rmsfe_modelos["ETS"])
  } else {
    NA_real_
  }
  
  rmsfe_regdin <- if ("REGDIN" %in% names(rmsfe_modelos)) {
    as.numeric(rmsfe_modelos["REGDIN"])
  } else {
    NA_real_
  }
  
  rmsfe_midas <- if ("MIDAS" %in% names(rmsfe_modelos)) {
    as.numeric(rmsfe_modelos["MIDAS"])
  } else {
    NA_real_
  }
  
  pesos <- (1 / rmsfe_modelos) / sum(1 / rmsfe_modelos)
  
  pred_ponderada <- as.numeric(prediction_matrix %*% pesos)
  
  # ----------------------------------------------------------
  # Salida ordenada
  # ----------------------------------------------------------
  
  predicciones <- bind_rows(
    tibble(
      fecha_mensual = datos$fecha_mensual,
      real = observed_vector,
      combinacion = nombre_combinacion,
      metodo = "media_simple",
      prediccion_combinada = pred_media_simple,
      error = real - prediccion_combinada,
      modelos_usados = paste(modelos, collapse = " + ")
    ),
    tibble(
      fecha_mensual = datos$fecha_mensual,
      real = observed_vector,
      combinacion = nombre_combinacion,
      metodo = "media_ponderada_inversa_RMSFE",
      prediccion_combinada = pred_ponderada,
      error = real - prediccion_combinada,
      modelos_usados = paste(modelos, collapse = " + ")
    )
  )
  
  metricas <- predicciones %>%
    group_by(combinacion, metodo, modelos_usados) %>%
    summarise(
      RMSFE = sqrt(mean(error^2, na.rm = TRUE)),
      MAE = mean(abs(error), na.rm = TRUE),
      MASE = ifelse(
        !is.na(escala_mase),
        MAE / escala_mase,
        NA_real_
      ),
      error_medio = mean(error, na.rm = TRUE),
      n_predicciones = n(),
      .groups = "drop"
    ) %>%
    mutate(
      mejor_que_SARIMA = ifelse(
        !is.na(rmsfe_sarima) & RMSFE < rmsfe_sarima,
        1L,
        0L
      ),
      mejor_que_ETS = ifelse(
        !is.na(rmsfe_ets) & RMSFE < rmsfe_ets,
        1L,
        0L
      ),
      mejor_que_regresion_dinamica = ifelse(
        !is.na(rmsfe_regdin) & RMSFE < rmsfe_regdin,
        1L,
        0L
      ),
      mejor_que_MIDAS = ifelse(
        !is.na(rmsfe_midas) & RMSFE < rmsfe_midas,
        1L,
        0L
      )
    ) %>%
    select(
      combinacion,
      metodo,
      modelos_usados,
      RMSFE,
      MAE,
      MASE,
      error_medio,
      n_predicciones,
      mejor_que_SARIMA,
      mejor_que_ETS,
      mejor_que_regresion_dinamica,
      mejor_que_MIDAS
    )
  
  pesos_tabla <- tibble(
    combinacion = nombre_combinacion,
    modelo = names(pesos),
    RMSFE_modelo = as.numeric(rmsfe_modelos),
    peso_inverso_RMSFE = as.numeric(pesos)
  )
  
  list(
    observed_vector = observed_vector,
    prediction_matrix = prediction_matrix,
    predicciones = predicciones,
    metricas = metricas,
    pesos = pesos_tabla
  )
}

# ============================================================
# 7. COMBINACIONES DE UN EVENTO Y UN ENFOQUE
# ============================================================

ejecutar_combinaciones <- function(tabla_predicciones) {
  
  # Se definen las combinaciones de predicciones que se quieren comparar.
  # En esta versión se usa únicamente el mejor modelo de cada familia.
  
  combinaciones <- list(
    univariante = c("SARIMA", "ETS"),
    con_predictores = c("REGDIN", "MIDAS"),
    completa = c("SARIMA", "ETS", "REGDIN", "MIDAS"),
    sarima_regdin = c("SARIMA", "REGDIN"),
    ets_regdin = c("ETS", "REGDIN"),
    sarima_midas = c("SARIMA", "MIDAS"),
    ets_midas = c("ETS", "MIDAS")
  )
  
  resultados <- imap(
    combinaciones,
    ~ combinar_predicciones(
      tabla = tabla_predicciones,
      modelos = .x,
      nombre_combinacion = .y
    )
  )
  
  resultados <- compact(resultados)
  
  predicciones_combinadas <- map_dfr(resultados, "predicciones")
  metricas_combinadas <- map_dfr(resultados, "metricas")
  pesos_combinaciones <- map_dfr(resultados, "pesos")
  
  list(
    tabla_predicciones = tabla_predicciones,
    predicciones_combinadas = predicciones_combinadas,
    metricas_combinadas = metricas_combinadas,
    pesos_combinaciones = pesos_combinaciones
  )
}

# ============================================================
# 8. PROCESAR UNA FILA DE LA TABLA DE ARCHIVOS
# ============================================================

procesar_evento_enfoque <- function(evento,
                                    fecha_ruptura,
                                    enfoque,
                                    sarima,
                                    ets,
                                    regdin,
                                    midas) {
  
  cat("\n")
  cat("============================================================\n")
  cat("Procesando:", evento, "-", enfoque, "\n")
  cat("============================================================\n")
  
  tabla_predicciones <- crear_tabla_predicciones(
    evento = evento,
    fecha_ruptura = fecha_ruptura,
    enfoque = enfoque,
    ruta_sarima = sarima,
    ruta_ets = ets,
    ruta_regdin = regdin,
    ruta_midas = midas
  )
  
  resultado <- ejecutar_combinaciones(tabla_predicciones)
  
  list(
    evento = evento,
    fecha_ruptura = fecha_ruptura,
    enfoque = enfoque,
    tabla_predicciones = resultado$tabla_predicciones,
    predicciones_combinadas = resultado$predicciones_combinadas,
    metricas_combinadas = resultado$metricas_combinadas,
    pesos_combinaciones = resultado$pesos_combinaciones
  )
}

# ============================================================
# 9. EJECUTAR TODO
# ============================================================

archivos_predicciones <- archivos_predicciones %>%
  mutate(
    archivos_ok =
      file.exists(sarima) &
      file.exists(ets) &
      file.exists(regdin) &
      file.exists(midas)
  )

if (any(!archivos_predicciones$archivos_ok)) {
  
  cat("\n")
  cat("ATENCIÓN: hay archivos que no existen. Estas filas no se procesarán:\n")
  
  print(
    archivos_predicciones %>%
      filter(!archivos_ok) %>%
      select(evento, enfoque, sarima, ets, regdin, midas)
  )
}


archivos_a_procesar <- archivos_predicciones %>%
  filter(archivos_ok)

resultados <- pmap(
  archivos_a_procesar %>%
    select(evento, fecha_ruptura, enfoque, sarima, ets, regdin, midas),
  procesar_evento_enfoque
)

# ============================================================
# 10. UNIR RESULTADOS
# ============================================================

tabla_predicciones_total <- map_dfr(
  resultados,
  ~ .x$tabla_predicciones
)

predicciones_combinadas_total <- map_dfr(
  resultados,
  ~ .x$predicciones_combinadas %>%
    mutate(
      evento = .x$evento,
      fecha_ruptura = as.Date(.x$fecha_ruptura),
      enfoque = .x$enfoque
    )
)

metricas_combinadas_total <- map_dfr(
  resultados,
  ~ .x$metricas_combinadas %>%
    mutate(
      evento = .x$evento,
      fecha_ruptura = as.Date(.x$fecha_ruptura),
      enfoque = .x$enfoque
    )
)

pesos_combinaciones_total <- map_dfr(
  resultados,
  ~ .x$pesos_combinaciones %>%
    mutate(
      evento = .x$evento,
      fecha_ruptura = as.Date(.x$fecha_ruptura),
      enfoque = .x$enfoque
    )
)

# ============================================================
# 11. EXPORTAR
# ============================================================

ruta_salida <- file.path(
  carpeta_salida,
  "combinacion_predicciones_sin_forecastcomb.xlsx"
)

write_xlsx(
  list(
    predicciones_base = tabla_predicciones_total,
    predicciones_combinadas = predicciones_combinadas_total,
    metricas_combinadas = metricas_combinadas_total,
    pesos_combinaciones = pesos_combinaciones_total
  ),
  path = ruta_salida
)

cat("\n")
cat("============================================================\n")
cat("PROCESO TERMINADO\n")
cat("Archivo exportado en:\n")
cat(ruta_salida, "\n")
cat("============================================================\n")

# ============================================================
# 12. GRÁFICOS TOP 1 EX ANTE: REAL VS PREDICCIÓN COMBINADA
# ============================================================

# Tabla manual con las mejores combinaciones ex ante según tus resultados
top1_ex_ante <- tibble::tribble(
  ~evento,         ~episodio_texto,                                  ~combinacion,        ~metodo,
  "marzo_2020",    "Confinamiento y post-confinamiento",              "ets_midas",         "media_ponderada_inversa_RMSFE",
  "marzo_2021",    "Repunte inflacionario de 2021",                   "con_predictores",   "media_ponderada_inversa_RMSFE",
  "febrero_2022",  "Inicio de la invasión de Rusia en Ucrania",        "ets_regdin",        "media_ponderada_inversa_RMSFE"
)

# Filtramos las predicciones combinadas correspondientes al top 1 ex ante
datos_graficos_top1_ex_ante <- predicciones_combinadas_total %>%
  filter(enfoque == "ex_ante") %>%
  inner_join(
    top1_ex_ante,
    by = c("evento", "combinacion", "metodo")
  ) %>%
  arrange(evento, fecha_mensual)

# Función para crear cada gráfico
crear_grafico_prediccion_combinada <- function(datos_evento) {
  
  # Etiquetas de meses en español
  etiquetas_meses_es <- c(
    "ene.", "feb.", "mar.", "abr.", "may.", "jun.",
    "jul.", "ago.", "sep.", "oct.", "nov.", "dic."
  )
  
  formatear_fecha_es <- function(fechas) {
    anio <- format(fechas, "%Y")
    mes_num <- as.integer(format(fechas, "%m"))
    paste(anio, etiquetas_meses_es[mes_num])
  }
  
  ggplot(datos_evento, aes(x = fecha_mensual)) +
    geom_line(aes(y = prediccion_combinada, colour = "Predicción"),
              linewidth = 1,
              linetype = "dashed") +
    geom_line(aes(y = real, colour = "Valor real"),
              linewidth = 1) +
    scale_colour_manual(
      values = c(
        "Predicción" = "#F8766D",
        "Valor real" = "#00BFC4"
      )
    ) +
    scale_x_date(
      labels = formatear_fecha_es,
      breaks = datos_evento$fecha_mensual
    ) +
    labs(
      x = "Fecha",
      y = "y_ipc_general",
      colour = ""
    ) +
    theme_grey() +
    theme(
      legend.position = "right",
      legend.title = element_blank(),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11)
    )
}
# Creamos los tres gráficos por separado
grafico_top1_exante_2020 <- datos_graficos_top1_ex_ante %>%
  filter(evento == "marzo_2020") %>%
  crear_grafico_prediccion_combinada()

grafico_top1_exante_2021 <- datos_graficos_top1_ex_ante %>%
  filter(evento == "marzo_2021") %>%
  crear_grafico_prediccion_combinada()

grafico_top1_exante_2022 <- datos_graficos_top1_ex_ante %>%
  filter(evento == "febrero_2022") %>%
  crear_grafico_prediccion_combinada()

grafico_top1_exante_2020
grafico_top1_exante_2021
grafico_top1_exante_2022
