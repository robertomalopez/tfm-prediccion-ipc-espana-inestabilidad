# Comparación de modelos para la predicción del IPC general en España en tiempos de inestabilidad inflacionaria

Repositorio asociado al Trabajo Fin de Máster en Análisis y Visualización de Datos Masivos.

## Descripción del proyecto

Este proyecto compara las predicciones del IPC general nacional español realizadas por diferentes modelos econométricos en tres episodios de inestabilidad inflacionaria. El objetivo principal es analizar si la incorporación de datos sobre bienes volátiles, preferiblemente de alta frecuencia, mejora la capacidad predictiva frente a modelos univariantes de referencia. La variable objetivo es la tasa logarítmica mensual del IPC general nacional español. Como regresores externos se utilizan variables relacionadas con precios de bienes energéticos, combustibles y una adaptación del índice NEER construida a partir de datos de diferentes cambios de moneda.

El análisis se centra en tres episodios de inestabilidad:

* Confinamiento y post-confinamiento de 2020.
* Repunte inflacionario de 2021.
* Inicio de la invasión rusa en Ucrania en 2022.

En el proyecto se implementan y comparan las siguientes familias de modelos:

* Modelos SARIMA.
* Modelos de suavización exponencial ETS.
* Modelos de regresión dinámica.
* Modelos MIDAS restringidos con correcciones SARIMA.
* Combinaciones de predicciones mediante la media aritmética y media ponderada por pesos inversamente proporcionales al RMSFE.

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
* `resultados/tablas/`: contiene las tablas de métricas y resultados generadas durante el análisis.
* `resultados/figuras/`: contiene las figuras y visualizaciones incluidas en la memoria.


## Autor

Roberto Malo López de la Fuente.
