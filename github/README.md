# Comparación de modelos para la predicción del IPC general en España en tiempos de inestabilidad inflacionaria

Repositorio asociado al Trabajo Fin de Máster en Análisis y Visualización de Datos Masivos.

## Descripción del proyecto

Este proyecto compara las predicciones del IPC general nacional español realizadas por diferentes modelos econométricos en tres episodios de inestabilidad inflacionaria. El objetivo principal es analizar si la incorporación de datos sobre bienes volátiles, preferiblemente de alta frecuencia, mejora la capacidad predictiva frente a modelos univariantes de referencia. La variable objetivo es la tasa logarítmica mensual del IPC general nacional español. Como regresores externos se utilizan variables relacionadas con precios de bienes energéticos, combustibles y una adaptación del índice NEER construida a partir de datos de diferentes cambios de moneda.

El análisis se centra en tres episodios de inestabilidad:

* Confinamiento y post-confinamiento de 2020.
* Repunte inflacionario de 2021.
* Inicio de la invasión rusa en Ucrania en 2022.

## Modelos considerados

En el proyecto se implementan y comparan las siguientes familias de modelos:

* Modelos SARIMA.
* Modelos de suavización exponencial ETS.
* Modelos de regresión dinámica.
* Modelos MIDAS restringidos con correcciones SARIMA.
* Combinaciones de predicciones mediante la media aritmética y media ponderada por pesos inversamente proporcionales al RMSFE.

## Metodología general

El procedimiento seguido en el proyecto se estructura en las siguientes fases:

1. Recolección de datos procedentes de fuentes públicas.
2. Limpieza y transformación de las series temporales.
3. Construcción de la variable objetivo y de los regresores.
4. Análisis exploratorio de la variable objetivo.
5. Entrenamiento, validación y evaluación de los modelos.
6. Comparación de resultados mediante métricas de error.
7. Análisis visual del conjunto de modelos de cada familia.
8. Combinación de predicciones y análisis de resultados.

La evaluación se realiza mediante esquemas ex ante y ex post. El enfoque ex ante permite simular una situación real de predicción, en la que el modelo se selecciona antes de observar el periodo de inestabilidad y, tras ello, se obtienen sus predicciones asociadas a ese periodo, mientras que el enfoque ex post permite identificar qué modelos habrían funcionado mejor una vez conocido el periodo evaluado.

## Métricas utilizadas

Las principales métricas empleadas para evaluar la precisión predictiva son:

* RMSFE: Root Mean Squared Forecast Error.
* MAE: Mean Absolute Error.
* MASE: Mean Absolute Scaled Error.

Además, se utilizan criterios de información como AICc y BIC, junto con un diagnóstico sobre la existencia de autocorrelación residual, mediante la prueba de Ljung-Box.

## Fuentes de datos

Las series utilizadas proceden de fuentes públicas y oficiales:

* Instituto Nacional de Estadística: serie histórica del IPC general nacional español.
* U.S. Energy Information Administration: precio del Brent.
* European Central Bank: tipos de cambio de moneda utilizados para la construcción del índice NEER adaptado.
* OMIE: precios de la electricidad.
* European Commission: precios de gasolina y diésel.

## Estructura del repositorio

La estructura general del repositorio es la siguiente:

```text
tfm-prediccion-ipc-espana/
│
├── README.md
├── .gitignore
│
├── datos/
│   ├── crudos/
│   └── procesados/
│
├── scripts/
│   ├── python/
│   └── r/
│
├── resultados/
    ├── tablas/
    └── figuras/
```

## Descripción de carpetas

* `datos/crudos/`: contiene los datos originales descargados de las fuentes correspondientes.
* `datos/procesados/`: contiene los datos transformados y preparados para la modelización.
* `scripts/python/`: contiene los scripts utilizados para la limpieza, transformación y visualización de datos.
* `scripts/r/`: contiene los scripts utilizados para el ajuste, validación y evaluación de los modelos econométricos.
* `results/tables/`: contiene las tablas de métricas y resultados generadas durante el análisis.
* `results/figures/`: contiene las figuras y visualizaciones incluidas en la memoria.
* `docs/`: contiene documentación auxiliar del proyecto.

## Orden recomendado de ejecución

Pendiente: se expone el orden en el que se deben ejecutar los scripts para obtener las tablas finales. 

## Software utilizado

El proyecto utiliza principalmente Python y R. Python se emplea para la limpieza, transformación y preparación de los datos, así como para la creación de algunas visualizaciones, mientras que R se emplea para la estimación, validación y evaluación de los modelos econométricos.

## Autor

Roberto Malo López de la Fuente.

