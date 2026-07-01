# -*- coding: utf-8 -*-
"""
Created on Thu Jun 18 17:35:34 2026

@author: rober
"""

import pandas as pd
from pathlib import Path

# ============================================================
# 1. DIRECTORIO DE ENTRADA Y SALIDA
# ============================================================

directorio = Path(
    r"C:\Users\rober\Desktop\UNIR\TFM\ubicacion_datos\tablas_para_visualizaciones\visualizacion_residuos"
)

archivo_salida = directorio / "tabla_residuos_modelos_formato_largo.xlsx"


# ============================================================
# 2. DICCIONARIOS DE CLASIFICACIÓN
# ============================================================

periodos_inestabilidad = {
    "2020": "Confinamiento y post-confinamiento",
    "2021": "Repunte inflacionario de 2021",
    "2022": "Inicio de la invasión de Rusia en Ucrania"
}

familias_modelos = {
    "sarima": "sarima",
    "ets": "ets",
    "regdin": "regdin",
    "midas": "midas"
}


# ============================================================
# 3. FUNCIÓN PARA IDENTIFICAR FAMILIA Y PERIODO
# ============================================================

def identificar_familia_y_periodo(nombre_archivo):
    nombre = nombre_archivo.lower()

    familia = None
    for clave_familia, valor_familia in familias_modelos.items():
        if nombre.startswith(clave_familia):
            familia = valor_familia
            break

    periodo = None
    for anio, descripcion_periodo in periodos_inestabilidad.items():
        if anio in nombre:
            periodo = descripcion_periodo
            break

    if familia is None:
        raise ValueError(f"No se pudo identificar la familia del archivo: {nombre_archivo}")

    if periodo is None:
        raise ValueError(f"No se pudo identificar el periodo del archivo: {nombre_archivo}")

    return familia, periodo



#########################
# VISUALIZACION RESIDUOS#
#########################

# ============================================================
# 4. LECTURA Y UNIÓN DE TODAS LAS TABLAS
# ============================================================

tablas = []

archivos_excel = sorted(directorio.glob("*.xlsx"))

for archivo in archivos_excel:

    # Evitamos leer la propia tabla final si ya existe de una ejecución anterior
    if archivo.name == archivo_salida.name:
        continue

    familia, periodo = identificar_familia_y_periodo(archivo.name)

    df = pd.read_excel(archivo)

    # En SARIMA, ETS y regresión dinámica el nombre del modelo está en .model.
    # En MIDAS está en id_modelo.
    if familia == "midas":
        columna_modelo = "id_modelo"
    else:
        columna_modelo = ".model"

    columnas_necesarias = [columna_modelo, "diagnostico_ljung"]

    columnas_faltantes = [
        col for col in columnas_necesarias
        if col not in df.columns
    ]

    if columnas_faltantes:
        raise ValueError(
            f"En el archivo {archivo.name} faltan las columnas: {columnas_faltantes}"
        )

    df_largo = df[columnas_necesarias].copy()

    df_largo = df_largo.rename(
        columns={
            columna_modelo: "nombre_modelo"
        }
    )

    df_largo["periodo_inestabilidad"] = periodo
    df_largo["familia_modelos"] = familia

    df_largo["archivo_origen"] = archivo.name

    df_largo = df_largo[
        [
            "nombre_modelo",
            "periodo_inestabilidad",
            "familia_modelos",
            "diagnostico_ljung",
            "archivo_origen"
        ]
    ]

    tablas.append(df_largo)


# ============================================================
# 5. TABLA FINAL EN FORMATO LARGO
# ============================================================

tabla_residuos_larga = pd.concat(tablas, ignore_index=True)


# ============================================================
# 6. COMPROBACIONES BÁSICAS
# ============================================================

print("Número de archivos leídos:", len(tablas))
print("Dimensión de la tabla final:", tabla_residuos_larga.shape)

print("\nResumen por familia y periodo:")
print(
    tabla_residuos_larga
    .groupby(["familia_modelos", "periodo_inestabilidad"])
    .size()
    .reset_index(name="n_modelos")
)

print("\nDiagnósticos Ljung-Box:")
print(
    tabla_residuos_larga["diagnostico_ljung"]
    .value_counts(dropna=False)
)


# ============================================================
# 7. EXPORTACIÓN
# ============================================================

tabla_residuos_larga.to_excel(archivo_salida, index=False)

print(f"\nTabla exportada correctamente en:\n{archivo_salida}")


#########################
# VISUALIZACION MASE < 1#
#########################

tablas = []

archivos_excel = sorted(directorio.glob("*.xlsx"))

for archivo in archivos_excel:

    # Evitamos leer la propia tabla final si ya existe de una ejecución anterior
    if archivo.name == archivo_salida.name:
        continue

    familia, periodo = identificar_familia_y_periodo(archivo.name)

    df = pd.read_excel(archivo)

    # En SARIMA, ETS y regresión dinámica el nombre del modelo está en .model.
    # En MIDAS está en id_modelo.
    if familia == "midas":
        columna_modelo = "id_modelo"
    else:
        columna_modelo = ".model"

    columnas_necesarias = [columna_modelo, "MASE"]

    columnas_faltantes = [
        col for col in columnas_necesarias
        if col not in df.columns
    ]

    if columnas_faltantes:
        raise ValueError(
            f"En el archivo {archivo.name} faltan las columnas: {columnas_faltantes}"
        )

    df_largo = df[columnas_necesarias].copy()

    df_largo = df_largo.rename(
        columns={
            columna_modelo: "nombre_modelo"
        }
    )

    df_largo["periodo_inestabilidad"] = periodo
    df_largo["familia_modelos"] = familia

    df_largo["archivo_origen"] = archivo.name

    df_largo = df_largo[
        [
            "nombre_modelo",
            "periodo_inestabilidad",
            "familia_modelos",
            "MASE",
            "archivo_origen"
        ]
    ]

    tablas.append(df_largo)
    
# ============================================================
# TABLA FINAL EN FORMATO LARGO
# ============================================================

tabla_mase_larga = pd.concat(tablas, ignore_index=True)

# ============================================================
# CREAR VARIABLE INDICADORA: MASE MENOR QUE 1
# ============================================================

tabla_mase_larga["MASE"] = pd.to_numeric(
    tabla_mase_larga["MASE"],
    errors="coerce"
)

posicion_mase = tabla_mase_larga.columns.get_loc("MASE")

tabla_mase_larga.insert(
    posicion_mase + 1,
    "menor_que_1",
    (tabla_mase_larga["MASE"] < 1).astype(int))

# ============================================================
# CREAR TABLA AGREGADA: PROPORCIÓN DE MODELOS CON MASE < 1
# ============================================================

tabla_proporcion_mase_menor_1 = (
    tabla_mase_larga
    .groupby(
        [
            "periodo_inestabilidad",
            "familia_modelos"
        ],
        as_index=False
    )
    .agg(
        n_modelos=("menor_que_1", "size"),
        n_modelos_mase_menor_1=("menor_que_1", "sum"),
        proporcion_mase_menor_1=("menor_que_1", "mean")
    )
)

# También creamos la variable en porcentaje, por si es más cómoda para visualizar
tabla_proporcion_mase_menor_1["porcentaje_mase_menor_1"] = (
    tabla_proporcion_mase_menor_1["proporcion_mase_menor_1"] * 100
)


# ============================================================
# EXPORTACIÓN
# ============================================================

directorio = Path(
    r"C:\Users\rober\Desktop\UNIR\TFM\ubicacion_datos\tablas_para_visualizaciones\visualizacion_mase_menor_1"
)

archivo_salida_larga = directorio / "tabla_mase_modelos_formato_largo.xlsx"
archivo_salida_proporcion = directorio / "tabla_proporcion_mase_menor_1_por_familia_periodo.xlsx"

# Tabla larga original
tabla_mase_larga.to_excel(
    archivo_salida_larga,
    index=False
)

# Tabla agregada para la visualización de proporciones
tabla_proporcion_mase_menor_1.to_excel(
    archivo_salida_proporcion,
    index=False
)

print(f"\nTabla larga exportada correctamente en:\n{archivo_salida_larga}")
print(f"\nTabla de proporciones exportada correctamente en:\n{archivo_salida_proporcion}")

# ============================================================
# 7. EXPORTACIÓN
# ============================================================
directorio = Path(
    r"C:\Users\rober\Desktop\UNIR\TFM\ubicacion_datos\tablas_para_visualizaciones\visualizacion_mase_menor_1"
)

archivo_salida = directorio / "tabla_mase_modelos_formato_largo.xlsx"

tabla_mase_larga.to_excel(archivo_salida, index=False)

print(f"\nTabla exportada correctamente en:\n{archivo_salida}")


#####################################################
# VISUALIZACION DISTRIBUCIÓN EX ANTE TEST RMSFE, MAE#
#####################################################

# ============================================================
# 1. DIRECTORIO DE ENTRADA Y SALIDA
# ============================================================

directorio = Path(
    r"C:\Users\rober\Desktop\UNIR\TFM\ubicacion_datos\tablas_para_visualizaciones\visualizacion_distribucion_rmsfe_mae_mase"
)

archivo_salida_rmsfe = directorio / "tabla_rmsfe_larga_exante_test.xlsx"
archivo_salida_mae = directorio / "tabla_mae_larga_exante_test.xlsx"
archivo_salida_mase = directorio / "tabla_mase_larga_exante_test.xlsx"


# ============================================================
# 2. DICCIONARIOS DE CLASIFICACIÓN
# ============================================================

periodos_inestabilidad = {
    "2020": "Confinamiento y post-confinamiento",
    "2021": "Repunte inflacionario de 2021",
    "2022": "Inicio de la invasión de Rusia en Ucrania"
}

familias_modelos = {
    "sarima": "sarima",
    "ets": "ets",
    "regdin": "regdin",
    "midas": "midas"
}


# ============================================================
# 3. FUNCIÓN PARA IDENTIFICAR FAMILIA Y PERIODO
# ============================================================

def identificar_familia_y_periodo(nombre_archivo):
    nombre = nombre_archivo.lower()

    familia = None
    for clave_familia, valor_familia in familias_modelos.items():
        if nombre.startswith(clave_familia):
            familia = valor_familia
            break

    periodo = None
    for anio, descripcion_periodo in periodos_inestabilidad.items():
        if anio in nombre:
            periodo = descripcion_periodo
            break

    if familia is None:
        raise ValueError(f"No se pudo identificar la familia del archivo: {nombre_archivo}")

    if periodo is None:
        raise ValueError(f"No se pudo identificar el periodo del archivo: {nombre_archivo}")

    return familia, periodo


# ============================================================
# 4. FUNCIÓN GENERAL PARA CREAR UNA TABLA LARGA DE UNA MÉTRICA
# ============================================================

def crear_tabla_larga_metrica(directorio, metrica):
    
    tablas = []

    archivos_excel = sorted(directorio.glob("*.xlsx"))

    for archivo in archivos_excel:

        # Evitamos leer posibles tablas ya generadas previamente
        if archivo.name.startswith("tabla_"):
            continue

        familia, periodo = identificar_familia_y_periodo(archivo.name)

        df = pd.read_excel(archivo)

        # En SARIMA, ETS y regresión dinámica el nombre del modelo está en .model.
        # En MIDAS está en id_modelo.
        if familia == "midas":
            columna_modelo = "id_modelo"
        else:
            columna_modelo = ".model"

        columnas_necesarias = [columna_modelo, metrica]

        columnas_faltantes = [
            col for col in columnas_necesarias
            if col not in df.columns
        ]

        if columnas_faltantes:
            raise ValueError(
                f"En el archivo {archivo.name} faltan las columnas: {columnas_faltantes}"
            )

        df_largo = df[columnas_necesarias].copy()

        df_largo = df_largo.rename(
            columns={
                columna_modelo: "nombre_modelo"
            }
        )

        df_largo["periodo_inestabilidad"] = periodo
        df_largo["familia_modelos"] = familia
        df_largo["archivo_origen"] = archivo.name

        # Convertimos la métrica a numérica por seguridad
        df_largo[metrica] = pd.to_numeric(
            df_largo[metrica],
            errors="coerce"
        )

        df_largo = df_largo[
            [
                "nombre_modelo",
                "periodo_inestabilidad",
                "familia_modelos",
                metrica,
                "archivo_origen"
            ]
        ]

        tablas.append(df_largo)

    tabla_larga = pd.concat(tablas, ignore_index=True)

    return tabla_larga


# ============================================================
# 5. CREAR LAS TRES TABLAS LARGAS
# ============================================================

tabla_rmsfe_larga = crear_tabla_larga_metrica(
    directorio=directorio,
    metrica="RMSFE"
)

tabla_mae_larga = crear_tabla_larga_metrica(
    directorio=directorio,
    metrica="MAE"
)

tabla_mase_larga = crear_tabla_larga_metrica(
    directorio=directorio,
    metrica="MASE"
)


# ============================================================
# 6. COMPROBACIONES BÁSICAS
# ============================================================

print("Dimensión tabla RMSFE:", tabla_rmsfe_larga.shape)
print("Dimensión tabla MAE:", tabla_mae_larga.shape)
print("Dimensión tabla MASE:", tabla_mase_larga.shape)

print("\nResumen RMSFE por familia y periodo:")
print(
    tabla_rmsfe_larga
    .groupby(["familia_modelos", "periodo_inestabilidad"])
    .size()
    .reset_index(name="n_modelos")
)

print("\nResumen MAE por familia y periodo:")
print(
    tabla_mae_larga
    .groupby(["familia_modelos", "periodo_inestabilidad"])
    .size()
    .reset_index(name="n_modelos")
)

print("\nResumen MASE por familia y periodo:")
print(
    tabla_mase_larga
    .groupby(["familia_modelos", "periodo_inestabilidad"])
    .size()
    .reset_index(name="n_modelos")
)


# ============================================================
# 7. EXPORTACIÓN
# ============================================================

tabla_rmsfe_larga.to_excel(archivo_salida_rmsfe, index=False)
tabla_mae_larga.to_excel(archivo_salida_mae, index=False)
tabla_mase_larga.to_excel(archivo_salida_mase, index=False)

print("\nTablas exportadas correctamente:")
print(archivo_salida_rmsfe)
print(archivo_salida_mae)
print(archivo_salida_mase)


#####################################################
# VISUALIZACION COMBINACIÓN DE PREDICCIONES         #
#####################################################
# ============================================================
# 1. DIRECTORIO Y ARCHIVO DE ENTRADA
# ============================================================

import numpy as np

directorio = Path(
    r"C:\Users\rober\Desktop\UNIR\TFM\ubicacion_datos\tablas_para_visualizaciones\visualizacion_comb_preds"
)

archivo_entrada = directorio / "combinacion_predicciones_sin_forecastcomb.xlsx"

archivo_salida_expost = directorio / "tabla_superacion_combinaciones_expost.xlsx"
archivo_salida_exante = directorio / "tabla_superacion_combinaciones_exante.xlsx"
archivo_salida_total = directorio / "tabla_superacion_combinaciones_expost_exante.xlsx"


# ============================================================
# 2. LECTURA DE LA HOJA metricas_combinadas
# ============================================================

df = pd.read_excel(
    archivo_entrada,
    sheet_name="metricas_combinadas"
)


# ============================================================
# 3. COLUMNAS DE MODELOS INDIVIDUALES
# ============================================================

columnas_mejora = {
    "SARIMA": "mejor_que_SARIMA",
    "ETS": "mejor_que_ETS",
    "REGDIN": "mejor_que_regresion_dinamica",
    "MIDAS": "mejor_que_MIDAS"
}


# ============================================================
# 4. COMPROBACIÓN DE COLUMNAS NECESARIAS
# ============================================================

columnas_necesarias = [
    "combinacion",
    "metodo",
    "modelos_usados",
    "evento",
    "enfoque"
] + list(columnas_mejora.values())

columnas_faltantes = [
    col for col in columnas_necesarias
    if col not in df.columns
]

if columnas_faltantes:
    raise ValueError(
        f"Faltan las siguientes columnas en la hoja metricas_combinadas: {columnas_faltantes}"
    )


# ============================================================
# 5. CREAR INDICADORES DE MODELOS USADOS Y MODELOS SUPERADOS
# ============================================================

df["modelos_usados_mayus"] = df["modelos_usados"].astype(str).str.upper()

for modelo, columna_mejora in columnas_mejora.items():

    # Indica si ese modelo individual participa en la combinación
    df[f"usa_{modelo}"] = df["modelos_usados_mayus"].str.contains(
        modelo,
        regex=False
    ).astype(int)

    # Aseguramos que la columna mejor_que_* sea numérica
    df[columna_mejora] = pd.to_numeric(
        df[columna_mejora],
        errors="coerce"
    ).fillna(0).astype(int)

    # Solo cuenta como modelo superado si ese modelo participa en la combinación
    df[f"supera_{modelo}"] = df[f"usa_{modelo}"] * df[columna_mejora]


# ============================================================
# 6. NÚMERO DE MODELOS USADOS Y SUPERADOS POR FILA
# ============================================================

columnas_usa = [f"usa_{modelo}" for modelo in columnas_mejora.keys()]
columnas_supera = [f"supera_{modelo}" for modelo in columnas_mejora.keys()]

df["n_modelos_usados"] = df[columnas_usa].sum(axis=1)
df["n_modelos_superados"] = df[columnas_supera].sum(axis=1)

df["porcentaje_modelos_superados_fila"] = (
    df["n_modelos_superados"] / df["n_modelos_usados"]
)


# ============================================================
# 7. AGREGACIÓN SOBRE LAS TRES FECHAS DE RUPTURA
# ============================================================

tabla_superacion = (
    df
    .groupby(["enfoque", "combinacion", "metodo"], as_index=False)
    .agg(
        n_eventos=("evento", "nunique"),
        n_filas=("evento", "size"),
        n_modelos_usados_total=("n_modelos_usados", "sum"),
        n_modelos_superados_total=("n_modelos_superados", "sum"),
        RMSFE_medio=("RMSFE", "mean"),
        MAE_medio=("MAE", "mean"),
        MASE_medio=("MASE", "mean")
    )
)

tabla_superacion["porcentaje_modelos_superados"] = (
    tabla_superacion["n_modelos_superados_total"] /
    tabla_superacion["n_modelos_usados_total"]
)


# ============================================================
# 8. VARIABLE CATEGÓRICA PARA EL COLOR DEL GRÁFICO
# ============================================================

condiciones = [
    tabla_superacion["porcentaje_modelos_superados"] < 0.5,
    tabla_superacion["porcentaje_modelos_superados"] == 1,
    (tabla_superacion["porcentaje_modelos_superados"] >= 0.5) &
    (tabla_superacion["porcentaje_modelos_superados"] < 1)
]

valores = [0, 1, 2]

tabla_superacion["categoria_superacion"] = np.select(
    condiciones,
    valores,
    default=np.nan
).astype(int)

etiquetas_categoria = {
    0: "No supera al menos la mitad",
    1: "Supera a todos los modelos usados",
    2: "Supera al menos la mitad, pero no todos"
}

tabla_superacion["categoria_superacion_etiqueta"] = (
    tabla_superacion["categoria_superacion"]
    .map(etiquetas_categoria)
)


# ============================================================
# 9. VARIABLE PARA EL EJE Y DEL GRÁFICO
# ============================================================

tabla_superacion["combinacion_metodo"] = (
    tabla_superacion["combinacion"].astype(str) +
    " | " +
    tabla_superacion["metodo"].astype(str)
)


# ============================================================
# 10. ORDENACIÓN PARA LA VISUALIZACIÓN
# ============================================================

tabla_superacion = tabla_superacion.sort_values(
    by=[
        "enfoque",
        "porcentaje_modelos_superados",
        "MASE_medio",
        "RMSFE_medio"
    ],
    ascending=[True, False, True, True]
).reset_index(drop=True)


# ============================================================
# 11. SEPARAR TABLAS EX POST Y EX ANTE
# ============================================================

tabla_superacion_expost = (
    tabla_superacion
    .query("enfoque == 'ex_post'")
    .copy()
    .reset_index(drop=True)
)

tabla_superacion_exante = (
    tabla_superacion
    .query("enfoque == 'ex_ante'")
    .copy()
    .reset_index(drop=True)
)


# ============================================================
# 12. EXPORTACIÓN
# ============================================================

tabla_superacion_expost.to_excel(
    archivo_salida_expost,
    index=False
)

tabla_superacion_exante.to_excel(
    archivo_salida_exante,
    index=False
)

tabla_superacion.to_excel(
    archivo_salida_total,
    index=False
)


# ============================================================
# 13. COMPROBACIONES
# ============================================================

print("Tabla ex post:", tabla_superacion_expost.shape)
print("Tabla ex ante:", tabla_superacion_exante.shape)
print("Tabla total:", tabla_superacion.shape)

print("\nResumen ex post:")
print(
    tabla_superacion_expost[
        [
            "combinacion_metodo",
            "porcentaje_modelos_superados",
            "categoria_superacion",
            "categoria_superacion_etiqueta"
        ]
    ]
)

print("\nResumen ex ante:")
print(
    tabla_superacion_exante[
        [
            "combinacion_metodo",
            "porcentaje_modelos_superados",
            "categoria_superacion",
            "categoria_superacion_etiqueta"
        ]
    ]
)


#######################################################
#CUÁL ES LA MEJOR COMBINACIÓN EN CADA FECHA DE RUPTURA#
#######################################################

import pandas as pd
from pathlib import Path

# ============================================================
# 1. DIRECTORIO Y ARCHIVO DE ENTRADA
# ============================================================

directorio = Path(
    r"C:\Users\rober\Desktop\UNIR\TFM\ubicacion_datos\tablas_para_visualizaciones\visualizacion_comb_preds"
)

archivo_entrada = directorio / "combinacion_predicciones_sin_forecastcomb.xlsx"

archivo_salida_total = directorio / "tabla_mejor_combinacion_por_ruptura_expost_exante.xlsx"
archivo_salida_expost = directorio / "tabla_mejor_combinacion_por_ruptura_expost.xlsx"
archivo_salida_exante = directorio / "tabla_mejor_combinacion_por_ruptura_exante.xlsx"


# ============================================================
# 2. LECTURA DE LA HOJA metricas_combinadas
# ============================================================

df = pd.read_excel(
    archivo_entrada,
    sheet_name="metricas_combinadas"
)


# ============================================================
# 3. COMPROBACIÓN DE COLUMNAS NECESARIAS
# ============================================================

columnas_necesarias = [
    "combinacion",
    "metodo",
    "modelos_usados",
    "RMSFE",
    "MAE",
    "MASE",
    "evento",
    "fecha_ruptura",
    "enfoque"
]

columnas_faltantes = [
    col for col in columnas_necesarias
    if col not in df.columns
]

if columnas_faltantes:
    raise ValueError(
        f"Faltan las siguientes columnas en la hoja metricas_combinadas: {columnas_faltantes}"
    )


# ============================================================
# 4. CONVERTIR MÉTRICAS A FORMATO NUMÉRICO
# ============================================================

metricas = ["RMSFE", "MAE", "MASE"]

if "error_medio" in df.columns:
    metricas.append("error_medio")

for col in metricas:
    df[col] = pd.to_numeric(df[col], errors="coerce")


# ============================================================
# 5. CREAR NOMBRE DEL PERIODO DE INESTABILIDAD
# ============================================================

def asignar_periodo_inestabilidad(valor):
    valor = str(valor).lower()

    if "2020" in valor:
        return "Confinamiento y post-confinamiento"
    elif "2021" in valor:
        return "Repunte inflacionario de 2021"
    elif "2022" in valor:
        return "Inicio de la invasión de Rusia en Ucrania"
    else:
        return "Periodo no identificado"


df["periodo_inestabilidad"] = df["evento"].apply(asignar_periodo_inestabilidad)


# ============================================================
# 6. CREAR VARIABLE COMBINACIÓN + MÉTODO
# ============================================================

df["combinacion_metodo"] = (
    df["combinacion"].astype(str) +
    " | " +
    df["metodo"].astype(str)
)


# ============================================================
# 7. SELECCIONAR LA MEJOR COMBINACIÓN POR RUPTURA Y ENFOQUE
# ============================================================
# Criterio principal: menor RMSFE.
# En caso de empate, se desempata por menor MASE y después menor MAE.

df_ordenado = df.sort_values(
    by=[
        "enfoque",
        "fecha_ruptura",
        "RMSFE",
        "MASE",
        "MAE"
    ],
    ascending=[
        True,
        True,
        True,
        True,
        True
    ]
)

tabla_mejor_combinacion = (
    df_ordenado
    .drop_duplicates(
        subset=["fecha_ruptura", "enfoque"],
        keep="first"
    )
    .copy()
    .reset_index(drop=True)
)


# ============================================================
# 8. SELECCIONAR Y ORDENAR COLUMNAS FINALES
# ============================================================

columnas_finales = [
    "periodo_inestabilidad",
    "evento",
    "fecha_ruptura",
    "enfoque",
    "combinacion",
    "metodo",
    "combinacion_metodo",
    "modelos_usados",
    "RMSFE",
    "MAE",
    "MASE"
]

columnas_extra_posibles = [
    "error_medio",
    "n_predicciones",
    "mejor_que_SARIMA",
    "mejor_que_ETS",
    "mejor_que_regresion_dinamica",
    "mejor_que_MIDAS"
]

for col in columnas_extra_posibles:
    if col in tabla_mejor_combinacion.columns:
        columnas_finales.append(col)

tabla_mejor_combinacion = tabla_mejor_combinacion[columnas_finales]


# ============================================================
# 9. SEPARAR EX POST Y EX ANTE
# ============================================================

tabla_mejor_combinacion_expost = (
    tabla_mejor_combinacion
    .query("enfoque == 'ex_post'")
    .copy()
    .reset_index(drop=True)
)

tabla_mejor_combinacion_exante = (
    tabla_mejor_combinacion
    .query("enfoque == 'ex_ante'")
    .copy()
    .reset_index(drop=True)
)


# ============================================================
# 10. EXPORTACIÓN
# ============================================================

tabla_mejor_combinacion.to_excel(
    archivo_salida_total,
    index=False
)

tabla_mejor_combinacion_expost.to_excel(
    archivo_salida_expost,
    index=False
)

tabla_mejor_combinacion_exante.to_excel(
    archivo_salida_exante,
    index=False
)


# ============================================================
# 11. COMPROBACIONES
# ============================================================

print("Tabla total:", tabla_mejor_combinacion.shape)
print("Tabla ex post:", tabla_mejor_combinacion_expost.shape)
print("Tabla ex ante:", tabla_mejor_combinacion_exante.shape)

print("\nMejor combinación por ruptura y enfoque:")
print(
    tabla_mejor_combinacion[
        [
            "periodo_inestabilidad",
            "enfoque",
            "combinacion_metodo",
            "RMSFE",
            "MAE",
            "MASE"
        ]
    ]
)

###############################################
# CREACION CUATRO TABLAS PARA LA VISUALIZACION#
###############################################

import pandas as pd
from pathlib import Path

# ============================================================
# 1. DIRECTORIO Y ARCHIVO DE ENTRADA
# ============================================================

directorio = Path(
    r"C:\Users\rober\Desktop\UNIR\TFM\ubicacion_datos\tablas_para_visualizaciones\visualizacion_comb_preds\superacion_modelos_individuales"
)

archivo_entrada = directorio / "tabla_superacion_combinaciones_expost_exante.xlsx"


# ============================================================
# 2. ARCHIVOS DE SALIDA
# ============================================================

archivo_expost_media_aritmetica = directorio / "tabla_superacion_expost_media_aritmetica.xlsx"
archivo_expost_media_ponderada = directorio / "tabla_superacion_expost_media_ponderada.xlsx"

archivo_exante_media_aritmetica = directorio / "tabla_superacion_exante_media_aritmetica.xlsx"
archivo_exante_media_ponderada = directorio / "tabla_superacion_exante_media_ponderada.xlsx"


# ============================================================
# 3. LECTURA DE LA TABLA TOTAL
# ============================================================

df = pd.read_excel(archivo_entrada)


# ============================================================
# 4. NORMALIZAR TEXTO DE MÉTODO Y ENFOQUE
# ============================================================

df["metodo_min"] = df["metodo"].astype(str).str.lower()
df["enfoque_min"] = df["enfoque"].astype(str).str.lower()


# ============================================================
# 5. FILTRAR LAS CUATRO TABLAS
# ============================================================

tabla_expost_media_aritmetica = (
    df[
        (df["enfoque_min"] == "ex_post") &
        (df["metodo_min"].str.contains("simple", na=False))
    ]
    .copy()
)

tabla_expost_media_ponderada = (
    df[
        (df["enfoque_min"] == "ex_post") &
        (df["metodo_min"].str.contains("ponder", na=False))
    ]
    .copy()
)

tabla_exante_media_aritmetica = (
    df[
        (df["enfoque_min"] == "ex_ante") &
        (df["metodo_min"].str.contains("simple", na=False))
    ]
    .copy()
)

tabla_exante_media_ponderada = (
    df[
        (df["enfoque_min"] == "ex_ante") &
        (df["metodo_min"].str.contains("ponder", na=False))
    ]
    .copy()
)


# ============================================================
# 6. ORDENAR CADA TABLA PARA LA VISUALIZACIÓN
# ============================================================

def preparar_para_visualizacion(tabla):
    tabla = tabla.copy()

    tabla["porcentaje_modelos_superados"] = pd.to_numeric(
        tabla["porcentaje_modelos_superados"],
        errors="coerce"
    )

    columnas_orden = ["porcentaje_modelos_superados"]
    ascendentes = [False]

    if "MASE_medio" in tabla.columns:
        columnas_orden.append("MASE_medio")
        ascendentes.append(True)

    if "RMSFE_medio" in tabla.columns:
        columnas_orden.append("RMSFE_medio")
        ascendentes.append(True)

    tabla = tabla.sort_values(
        by=columnas_orden,
        ascending=ascendentes
    ).reset_index(drop=True)

    tabla["orden_visual"] = range(1, len(tabla) + 1)

    tabla["combinacion_ordenada"] = (
        tabla["orden_visual"].astype(str).str.zfill(2) +
        " - " +
        tabla["combinacion"].astype(str)
    )

    tabla["porcentaje_modelos_superados_100"] = (
        tabla["porcentaje_modelos_superados"] * 100
    )

    return tabla


tabla_expost_media_aritmetica = preparar_para_visualizacion(tabla_expost_media_aritmetica)
tabla_expost_media_ponderada = preparar_para_visualizacion(tabla_expost_media_ponderada)

tabla_exante_media_aritmetica = preparar_para_visualizacion(tabla_exante_media_aritmetica)
tabla_exante_media_ponderada = preparar_para_visualizacion(tabla_exante_media_ponderada)


# ============================================================
# 7. ELIMINAR COLUMNAS AUXILIARES DE TEXTO
# ============================================================

tablas = [
    tabla_expost_media_aritmetica,
    tabla_expost_media_ponderada,
    tabla_exante_media_aritmetica,
    tabla_exante_media_ponderada
]

for tabla in tablas:
    tabla.drop(
        columns=["metodo_min", "enfoque_min"],
        errors="ignore",
        inplace=True
    )


# ============================================================
# 8. EXPORTAR
# ============================================================

tabla_expost_media_aritmetica.to_excel(
    archivo_expost_media_aritmetica,
    index=False
)

tabla_expost_media_ponderada.to_excel(
    archivo_expost_media_ponderada,
    index=False
)

tabla_exante_media_aritmetica.to_excel(
    archivo_exante_media_aritmetica,
    index=False
)

tabla_exante_media_ponderada.to_excel(
    archivo_exante_media_ponderada,
    index=False
)


# ============================================================
# 9. COMPROBACIONES
# ============================================================

print("Ex post - media aritmética:", tabla_expost_media_aritmetica.shape)
print("Ex post - media ponderada:", tabla_expost_media_ponderada.shape)

print("Ex ante - media aritmética:", tabla_exante_media_aritmetica.shape)
print("Ex ante - media ponderada:", tabla_exante_media_ponderada.shape)

print("\nArchivos exportados correctamente:")
print(archivo_expost_media_aritmetica)
print(archivo_expost_media_ponderada)
print(archivo_exante_media_aritmetica)
print(archivo_exante_media_ponderada)

