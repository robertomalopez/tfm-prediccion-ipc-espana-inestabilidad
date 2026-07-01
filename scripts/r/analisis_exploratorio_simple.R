# ============================================================
# ANÁLISIS EXPLORATORIO DE SERIES TEMPORALES MENSUALES
# ============================================================
# Objetivo del script:
#   Realizar un análisis exploratorio sencillo y legible sobre el dataset mensual.
#   Todas las variables numéricas se asumen ya expresadas como tasas logarítmicas.
#
# Qué hace el script:
#   1. Carga el Excel.
#   2. Identifica la fecha, la variable objetivo y los regresores.
#   3. Revisa cobertura temporal y valores ausentes.
#   4. Grafica las series en tasa logarítmica.
#   5. Calcula estadísticos descriptivos.
#   6. Calcula correlaciones contemporáneas con la variable objetivo.
#   7. Calcula correlaciones retardadas de los regresores con la variable objetivo.
#   8. Aplica contrastes ADF y KPSS de estacionariedad.
#
# IMPORTANTE:
#   Este script no guarda resultados automáticamente.
#   La idea es que puedas ejecutarlo por bloques, mirar las salidas en consola
#   y entender qué está haciendo cada parte.
# ============================================================


# ============================================================
# 1. PAQUETES
# ============================================================
# readxl: lectura del archivo Excel.
# ggplot2: gráficos.
# tseries: contrastes ADF y KPSS.

library(readxl)
library(ggplot2)
library(tseries)


# ============================================================
# 2. CARGA DE DATOS
# ============================================================
# Cambia esta ruta si el archivo Excel no está en tu directorio de trabajo.
# En Spyder/RStudio puedes consultar el directorio activo con getwd().

ruta_excel <- "df_modelo_mensual.xlsx"

# Se lee la primera hoja del Excel.
df <- as.data.frame(read_excel(ruta_excel, sheet = 1))

# Comprobamos las primeras filas y la estructura del dataset.
head(df)
str(df)


# ============================================================
# 3. IDENTIFICACIÓN DE COLUMNAS PRINCIPALES
# ============================================================
# Se asume que:
#   - La columna de fecha se llama fecha_mensual.
#   - La variable objetivo se llama y_ipc_general.
#   - El resto de columnas numéricas son regresores.

columna_fecha <- "fecha_mensual"
variable_objetivo <- "y_ipc_general"

# Convertimos la fecha a formato Date por si Excel la ha importado en otro formato.
df[[columna_fecha]] <- as.Date(df[[columna_fecha]])

# Ordenamos los datos temporalmente.
df <- df[order(df[[columna_fecha]]), ]

# Identificamos variables numéricas.
variables_numericas <- names(df)[sapply(df, is.numeric)]

# Los regresores son todas las variables numéricas excepto la variable objetivo.
regresores <- setdiff(variables_numericas, variable_objetivo)

# Mostramos las variables detectadas.
cat("Variable de fecha:\n")
print(columna_fecha)

cat("\nVariable objetivo:\n")
print(variable_objetivo)

cat("\nVariables numéricas:\n")
print(variables_numericas)

cat("\nRegresores detectados:\n")
print(regresores)


# ============================================================
# 4. COBERTURA TEMPORAL Y VALORES AUSENTES
# ============================================================
# Este bloque revisa cuántas observaciones válidas y cuántos valores ausentes
# tiene cada serie. También muestra la primera y última fecha con dato válido.
# Sirve para comprobar que todas las series comparten una muestra común.

resumen_cobertura <- data.frame()

for (var in variables_numericas) {
  serie <- df[[var]]
  fechas_validas <- df[[columna_fecha]][!is.na(serie)]
  
  fila <- data.frame(
    variable = var,
    n_obs = sum(!is.na(serie)),
    n_na = sum(is.na(serie)),
    primer_dato = min(fechas_validas),
    ultimo_dato = max(fechas_validas)
  )
  
  resumen_cobertura <- rbind(resumen_cobertura, fila)
}

resumen_cobertura


# ============================================================
# 5. INSPECCIÓN GRÁFICA DE LAS SERIES EN TASA LOGARÍTMICA
# ============================================================
# Como todas las variables ya están expresadas como tasas logarítmicas,
# los gráficos muestran directamente las series que se utilizarán en el análisis.
#
# Primero se grafica la variable objetivo. Después se grafican los regresores.

# Gráfico de la variable objetivo: tasa logarítmica del IPC general.
ggplot(df, aes(x = .data[[columna_fecha]], y = .data[[variable_objetivo]])) +
  geom_line() +
  labs(
    title = "Variable objetivo: tasa logarítmica del IPC general",
    x = "Fecha",
    y = "Tasa logarítmica"
  ) +
  theme_minimal()


# Gráficos individuales de cada regresor.
# Se genera un gráfico por variable para que sea fácil observar picos,
# volatilidad y cambios de comportamiento sin mezclar muchas series en la misma figura.

for (var in regresores) {
  print(
    ggplot(df, aes(x = .data[[columna_fecha]], y = .data[[var]])) +
      geom_line() +
      labs(
        title = paste("Regresor:", var),
        x = "Fecha",
        y = "Tasa logarítmica"
      ) +
      theme_minimal()
  )
}


# ============================================================
# 6. ESTADÍSTICOS DESCRIPTIVOS
# ============================================================
# Se calculan estadísticos descriptivos para todas las variables numéricas.
# Permiten comparar media, volatilidad, valores extremos y forma de la distribución.
#
# La desviación típica es especialmente útil para ver qué regresores son más volátiles.
# El mínimo y el máximo ayudan a identificar episodios extremos.
# La asimetría y la curtosis ayudan a detectar distribuciones no normales.

estadisticos <- data.frame()

for (var in variables_numericas) {
  x <- df[[var]]
  x <- x[!is.na(x)]
  
  media <- mean(x)
  desviacion_tipica <- sd(x)
  minimo <- min(x)
  q1 <- quantile(x, 0.25)
  mediana <- median(x)
  q3 <- quantile(x, 0.75)
  maximo <- max(x)
  
  # Asimetría y curtosis calculadas de forma directa para no depender de más paquetes.
  asimetria <- mean((x - media)^3) / desviacion_tipica^3
  curtosis <- mean((x - media)^4) / desviacion_tipica^4
  
  fila <- data.frame(
    variable = var,
    n = length(x),
    media = media,
    desviacion_tipica = desviacion_tipica,
    minimo = minimo,
    q1 = as.numeric(q1),
    mediana = mediana,
    q3 = as.numeric(q3),
    maximo = maximo,
    asimetria = asimetria,
    curtosis = curtosis
  )
  
  estadisticos <- rbind(estadisticos, fila)
}

View(estadisticos)


# ============================================================
# 7. VALORES ATÍPICOS DESCRIPTIVOS
# ============================================================
# No se eliminan valores atípicos. Solo se identifican de forma descriptiva.
# Se usa la regla habitual del rango intercuartílico:
#   valor atípico inferior: x < Q1 - 1.5 * IQR
#   valor atípico superior: x > Q3 + 1.5 * IQR
#
# Esto permite ver qué variables tienen episodios extremos,
# manteniendo la muestra original para la modelización.

resumen_atipicos <- data.frame()

for (var in variables_numericas) {
  x <- df[[var]]
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  limite_inferior <- q1 - 1.5 * iqr
  limite_superior <- q3 + 1.5 * iqr
  
  n_atipicos <- sum(x < limite_inferior | x > limite_superior, na.rm = TRUE)
  
  fila <- data.frame(
    variable = var,
    limite_inferior = as.numeric(limite_inferior),
    limite_superior = as.numeric(limite_superior),
    n_atipicos = n_atipicos
  )
  
  resumen_atipicos <- rbind(resumen_atipicos, fila)
}

resumen_atipicos


# ============================================================
# 8. CORRELACIÓN CONTEMPORÁNEA CON LA VARIABLE OBJETIVO
# ============================================================
# Se calcula la correlación entre la tasa logarítmica del IPC general y cada regresor
# en el mismo mes.
#
# Este análisis no demuestra causalidad. Solo sirve para detectar asociaciones
# lineales preliminares entre cada regresor y la variable objetivo.

correlaciones_contemporaneas <- data.frame()

for (var in regresores) {
  correlacion <- cor(df[[variable_objetivo]], df[[var]], use = "complete.obs")
  
  fila <- data.frame(
    regresor = var,
    correlacion_con_ipc = correlacion
  )
  
  correlaciones_contemporaneas <- rbind(correlaciones_contemporaneas, fila)
}

# Ordenamos por valor absoluto para ver qué regresores tienen mayor asociación lineal.
correlaciones_contemporaneas <- correlaciones_contemporaneas[order(abs(correlaciones_contemporaneas$correlacion_con_ipc), decreasing = TRUE), ]
correlaciones_contemporaneas


# ============================================================
# 9. CORRELACIONES RETARDADAS
# ============================================================
# En economía, el efecto de un regresor puede no ser inmediato.
# Por eso calculamos correlaciones entre:
#   y_t       = tasa logarítmica del IPC en el mes t
#   x_{t-k}   = regresor observado k meses antes
#
# Se calculan retardos de 0 a 3 meses.

max_lag <- 3
correlaciones_retardadas <- data.frame()

y <- df[[variable_objetivo]]

for (var in regresores) {
  x <- df[[var]]
  
  for (k in 0:max_lag) {
    if (k == 0) {
      y_alineado <- y
      x_alineado <- x
    } else {
      y_alineado <- y[(k + 1):length(y)]
      x_alineado <- x[1:(length(x) - k)]
    }
    
    correlacion <- cor(y_alineado, x_alineado, use = "complete.obs")
    
    fila <- data.frame(
      regresor = var,
      retardo_meses = k,
      correlacion = correlacion
    )
    
    correlaciones_retardadas <- rbind(correlaciones_retardadas, fila)
  }
}

correlaciones_retardadas

# Para ver, de forma resumida, el retardo con mayor correlación absoluta por regresor:
mejores_retardos <- data.frame()

for (var in regresores) {
  subtabla <- correlaciones_retardadas[correlaciones_retardadas$regresor == var, ]
  mejor <- subtabla[which.max(abs(subtabla$correlacion)), ]
  mejores_retardos <- rbind(mejores_retardos, mejor)
}

mejores_retardos <- mejores_retardos[order(abs(mejores_retardos$correlacion), decreasing = TRUE), ]
mejores_retardos


# ============================================================
# 10. MATRIZ DE CORRELACIONES ENTRE VARIABLES PRINCIPALES
# ============================================================
# La matriz completa con todas las variables puede ser difícil de leer si hay
# muchos regresores y nombres largos. Por eso, para la inspección gráfica,
# seleccionamos un subconjunto de variables principales.

variables_matriz <- c(
  variable_objetivo,
  "dlog_brent_dollars_per_barrel",
  "dlog_neer_aprox",
  "dlog_omie_precio",
  "dlog_gasolina_95",
  "dlog_diesel"
)

# Nos quedamos solo con las variables que existan realmente en el dataframe.
variables_matriz <- variables_matriz[variables_matriz %in% names(df)]

matriz_correlaciones <- cor(df[variables_matriz], use = "pairwise.complete.obs")

matriz_larga <- as.data.frame(as.table(matriz_correlaciones))
names(matriz_larga) <- c("variable_1", "variable_2", "correlacion")

ggplot(matriz_larga, aes(x = variable_1, y = variable_2, fill = correlacion)) +
  geom_tile() +
  geom_text(aes(label = round(correlacion, 2)), size = 3) +
  labs(
    title = "Matriz de correlaciones entre variables principales",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

matriz_correlaciones_completa <- cor(df[variables_numericas], use = "pairwise.complete.obs")

round(matriz_correlaciones_completa, 3)

# ============================================================
# 11. ESTACIONARIEDAD: ADF Y KPSS
# ============================================================
# Se aplican contrastes de estacionariedad a todas las variables numéricas.
# Como los datos ya están en tasas logarítmicas, lo esperable es que muchas series
# presenten un comportamiento más estable que en niveles.
#
# ADF:
#   H0: la serie tiene raíz unitaria, es decir, no es estacionaria.
#   Si p-valor < 0.05, se rechaza H0 y hay evidencia de estacionariedad.
#
# KPSS:
#   H0: la serie es estacionaria.
#   Si p-valor > 0.05, no se rechaza H0 y hay evidencia compatible con estacionariedad.
#
# La combinación de ambos contrastes ofrece una lectura más equilibrada.

estacionariedad <- data.frame()

for (var in variables_numericas) {
  x <- df[[var]]
  x <- x[!is.na(x)]
  
  adf <- adf.test(x)
  kpss <- kpss.test(x, null = "Level")
  
  fila <- data.frame(
    variable = var,
    adf_pvalor = adf$p.value,
    kpss_pvalor = kpss$p.value,
    conclusion_adf = ifelse(adf$p.value < 0.05,
                            "Rechaza raíz unitaria",
                            "No rechaza raíz unitaria"),
    conclusion_kpss = ifelse(kpss$p.value > 0.05,
                             "Compatible con estacionariedad",
                             "Rechaza estacionariedad")
  )
  
  estacionariedad <- rbind(estacionariedad, fila)
}

estacionariedad


# ============================================================
# 12. LECTURA FINAL DEL ANÁLISIS
# ============================================================
# Al terminar el análisis, conviene revisar especialmente:
#
#   1. Qué variables tienen mayor volatilidad según la desviación típica.
#   2. Qué variables presentan más valores atípicos.
#   3. Qué regresores tienen mayor correlación contemporánea con el IPC.
#   4. Qué regresores se relacionan más con el IPC cuando se introducen retardos.
#   5. Si las tasas logarítmicas parecen estacionarias según ADF y KPSS.
#
# Este análisis no selecciona automáticamente los modelos.
# Su objetivo es entender la estructura de los datos antes de la modelización.
# ============================================================
