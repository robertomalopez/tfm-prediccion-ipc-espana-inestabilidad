# -*- coding: utf-8 -*-
"""
Created on Wed May 27 12:06:23 2026

@author: rober
"""

#Creación de esquema para los datos: 
    
    # Importación de datasets de otros scripts de precios ordenados
    # por fecha con sus respectivas frecuencias
    
    # En este script se van a modificar según el siguiente criterio: 
        # Frecuencia diaria: el dataset de moneda tiene omisión de datos en
        # fines de semana, por lo que se agregan a semanales. El dataset de
        # electricidad es el otro que tiene datos diarios, pero en este 
        # caso cuenta con todas las fechas en un mes. Nos quedamos con los 
        # primeros 28 días en todos los meses en todos los años. 
        
        #Frecuencia semanal: aquellos datos que pasen al nivel semanal,
        # se quedarán con los cuatro primeros registros. Esto es porque todos
        #los meses cuentan al menos con 4 semanas. En general, los meses están 
        #compuestos por 4 semanas, y en algunos casos por 5 semanas. 
        
        #Frecuencia mensual: aquella serie que tenga frecuencia mensual no 
        # sufrirá cambios. 
    
    #Una vez se tengan las series ajustadas según el criterio anterior, se obtendrá
    #el dato de la tasa logarítmica para todas las series. 
    
    #Una vez se tengan todas las series, nos quedaremos con los registros entre 
    # 2005 y 2024, ambos incluidos. Esto, para cada serie de datos. 
    
    #Se exportará cada serie con el nombre correspondiente, para poder subir los
    #datos a R e implementar el modelo MIDAS según corresponda. 

# ============================================================
# SCRIPT DE PREPARACIÓN DE DATOS PARA MODELOS MIDAS EN R
# ============================================================

import pandas as pd
import numpy as np
from pathlib import Path

# ============================================================
# 1. IMPORTACIÓN DE DATASETS DESDE OTROS SCRIPTS
# ============================================================

from brent import df_br
from cambio_de_moneda import df_neer_midas
from omiehist import df_omie
from eu_comision_oil import df_gas_die
from indices_ipc_desagregados import df_alimentos
from indices_ipc_desagregados import df_indice

# ============================================================
# PREPARACIÓN DE VARIABLES DIARIAS CON HUECOS DE FIN DE SEMANA
# ============================================================
# df_neer_midas:
#   - Frecuencia diaria, pero sin sábados ni domingos.
#   - Se transforma a frecuencia semanal tomando bloques de fechas consecutivas.
#   - Cada bloque suele corresponder a lunes-viernes.
#   - La fecha semanal será la primera fecha disponible de cada bloque.
#   - Las variables usd_eur y neer_aprox se agregan mediante la media.

df_neer_midas = df_neer_midas.copy()

# Asegurar que la fecha está en formato datetime
df_neer_midas["fecha"] = pd.to_datetime(
    df_neer_midas["fecha"],
    format="%d-%m-%Y"
)

# Ordenar por fecha
df_neer_midas = df_neer_midas.sort_values("fecha").reset_index(drop=True)

# Detectar saltos entre fechas consecutivas
df_neer_midas["diferencia_dias"] = df_neer_midas["fecha"].diff().dt.days

# Crear un nuevo grupo cada vez que la diferencia entre fechas sea mayor que 1
# Esto separa las semanas porque entre viernes y lunes hay un salto de 3 días
df_neer_midas["grupo_semana"] = (df_neer_midas["diferencia_dias"] > 1).cumsum()

# Agregar a frecuencia semanal
df_neer_semanal = (
    df_neer_midas
    .groupby("grupo_semana", as_index=False)
    .agg(
        fecha=("fecha", "first"),
        usd_eur=("usd_eur", "mean"),
        neer_aprox=("neer_aprox", "mean"),
        n_dias_semana=("fecha", "count")
    )
)

# Eliminar la columna auxiliar de grupo
df_neer_semanal = df_neer_semanal.drop(columns=["grupo_semana"])


# ============================================================
# 4. DATASET FINAL df_omie CON DÍAS 1-28
# ============================================================

# Partimos de df_omie ya con fecha en datetime
df_omie = df_omie.copy()
df_omie["fecha"] = pd.to_datetime(df_omie["fecha"], dayfirst=True)
df_omie = df_omie.sort_values("fecha").reset_index(drop=True)

# Crear fecha mensual auxiliar
df_omie["fecha_mensual"] = df_omie["fecha"].dt.to_period("M")

# Identificar meses incompletos entre los días 1-28
cobertura_omie = (
    df_omie[df_omie["fecha"].dt.day.between(1, 28)]
    .groupby("fecha_mensual")["fecha"]
    .nunique()
    .reset_index(name="n_dias_1_28")
)

meses_incompletos = cobertura_omie.loc[
    cobertura_omie["n_dias_1_28"] < 28,
    "fecha_mensual"
]

# Quedarse con días 1-28 y eliminar meses incompletos
df_omie_1_28 = (
    df_omie[
        df_omie["fecha"].dt.day.between(1, 28)
        & ~df_omie["fecha_mensual"].isin(meses_incompletos)
    ]
    .copy()
    .reset_index(drop=True)
)

# Opcional: eliminar columna auxiliar
df_omie_1_28 = df_omie_1_28.drop(columns=["fecha_mensual"])


# ============================================================
# 5. COMPROBACIÓN DE COBERTURA SEMANAL
# ============================================================
# Objetivo:
#   - Comprobar que df_br, df_neer y df_gas_die tienen al menos
#     4 observaciones semanales dentro de cada mes.
#   - Identificar los meses en los que no se cumple esa condición.
# ============================================================

datasets_semanales = {
    "df_br": df_br,
    "df_neer_semanal": df_neer_semanal,
    "df_gas_die": df_gas_die
}

resultados_cobertura_semanal = {}

for nombre, df in datasets_semanales.items():
    
    df_temp = df.copy()
    
    # Convertir fecha a datetime
    df_temp["fecha"] = pd.to_datetime(df_temp["fecha"], dayfirst=True)
    
    # Ordenar por fecha
    df_temp = df_temp.sort_values("fecha").reset_index(drop=True)
    
    # Crear fecha mensual auxiliar
    df_temp["fecha_mensual"] = df_temp["fecha"].dt.to_period("M")
    
    # Contar observaciones disponibles por mes
    cobertura = (
        df_temp
        .groupby("fecha_mensual")
        .agg(
            n_observaciones=("fecha", "count"),
            primera_fecha=("fecha", "min"),
            ultima_fecha=("fecha", "max")
        )
        .reset_index()
    )
    
    # Identificar meses con menos de 4 observaciones
    meses_incompletos = cobertura[
        cobertura["n_observaciones"] < 4
    ].copy()
    
    # Guardar resultados
    resultados_cobertura_semanal[nombre] = meses_incompletos
    
    # Mostrar resumen
    print("\n============================================================")
    print(f"Dataset: {nombre}")
    print("Número de meses con menos de 4 observaciones:", len(meses_incompletos))
    
    if len(meses_incompletos) > 0:
        print("\nMeses incompletos:")
        print(meses_incompletos)
    else:
        print("Todos los meses tienen al menos 4 observaciones.")

# ============================================================
# CORRECCIÓN SENCILLA DE df_gas_die A 4 OBSERVACIONES POR MES
# ============================================================
# Criterio:
#   - Dividir cada mes en 4 partes aproximadas:
#       1) días 1-7
#       2) días 8-14
#       3) días 15-21
#       4) días 22-fin de mes
#   - Si falta una parte, crear una observación copiando el dato
#     de la semana anterior disponible.
# ============================================================

df_gas_die = df_gas_die.copy()

# Asegurar formato fecha
df_gas_die["fecha"] = pd.to_datetime(df_gas_die["fecha"], dayfirst=True)

# Ordenar
df_gas_die = df_gas_die.sort_values("fecha").reset_index(drop=True)

# Crear mes auxiliar
df_gas_die["fecha_mensual"] = df_gas_die["fecha"].dt.to_period("M")

# Función auxiliar sencilla para asignar tramo mensual
def asignar_tramo_mes(dia):
    if dia <= 7:
        return 1
    elif dia <= 14:
        return 2
    elif dia <= 21:
        return 3
    elif dia <= 28:
        return 4
    else: 
        return 5

# Aplicar tramo
df_gas_die["tramo_mes"] = df_gas_die["fecha"].dt.day.apply(asignar_tramo_mes)

# Lista donde se guardan los meses corregidos
lista_meses = []

for fecha_mensual, datos_mes in df_gas_die.groupby("fecha_mensual"):
    
    datos_mes = datos_mes.sort_values("fecha").reset_index(drop=True).copy()
    
    # Eliminar registros correspondientes a la quinta semana del mes
    datos_mes = datos_mes[datos_mes["tramo_mes"] <= 4].copy()
    
    # Tramos que ya existen en ese mes
    tramos_existentes = set(datos_mes["tramo_mes"])
    
    # Tramos que deberían existir
    tramos_esperados = {1, 2, 3, 4}
    
    # Tramos que faltan
    tramos_faltantes = sorted(tramos_esperados - tramos_existentes)
    
    # Marcar las filas originales
    datos_mes["fila_imputada"] = False
    datos_mes["fecha_origen_imputacion"] = pd.NaT
    
    filas_nuevas = []
    
    for tramo_faltante in tramos_faltantes:
        
        # Fecha que se asignará al dato creado
        if tramo_faltante == 1:
            dia_nuevo = 1
        elif tramo_faltante == 2:
            dia_nuevo = 8
        elif tramo_faltante == 3:
            dia_nuevo = 15
        else:
            dia_nuevo = 22
        
        fecha_nueva = fecha_mensual.to_timestamp().replace(day=dia_nuevo)
        
        # Buscar el dato anterior más cercano dentro del mismo mes
        datos_anteriores = datos_mes[datos_mes["fecha"] < fecha_nueva]
        
        if len(datos_anteriores) > 0:
            fila_base = datos_anteriores.iloc[-1].copy()
        
        else:
            # Si falta el primer tramo y no hay dato anterior dentro del mes,
            # se copia el primer dato disponible del mes.
            fila_base = datos_mes.iloc[0].copy()
        
        # Crear nueva fila copiando valores
        fila_nueva = fila_base.copy()
        fila_nueva["fecha"] = fecha_nueva
        fila_nueva["fecha_mensual"] = fecha_mensual
        fila_nueva["tramo_mes"] = tramo_faltante
        fila_nueva["fila_imputada"] = True
        fila_nueva["fecha_origen_imputacion"] = fila_base["fecha"]
        
        filas_nuevas.append(fila_nueva)
    
    # Añadir filas nuevas al mes
    if len(filas_nuevas) > 0:
        datos_mes = pd.concat(
            [datos_mes, pd.DataFrame(filas_nuevas)],
            ignore_index=True
        )
    
    # Ordenar dentro del mes
    datos_mes = datos_mes.sort_values("fecha").reset_index(drop=True)
    
    # Si por algún motivo hay más de 4 registros, quedarse con uno por tramo
    datos_mes = (
        datos_mes
        .sort_values(["tramo_mes", "fecha"])
        .drop_duplicates(subset=["tramo_mes"], keep="first")
        .sort_values("fecha")
        .reset_index(drop=True)
    )
    
    lista_meses.append(datos_mes)

# Dataset corregido
df_gas_die_4sem = (
    pd.concat(lista_meses, ignore_index=True)
    .sort_values("fecha")
    .reset_index(drop=True)
)

# Eliminar columnas auxiliares y conservar solo fecha + variables
columnas_auxiliares = [
    "fecha_mensual",
    "tramo_mes",
    "fila_imputada",
    "fecha_origen_imputacion"
]

df_gas_die_4sem = df_gas_die_4sem.drop(
    columns=[col for col in columnas_auxiliares if col in df_gas_die_4sem.columns]
)

# ============================================================
# SELECCIÓN DE LAS PRIMERAS 4 SEMANAS POR MES
# ============================================================
# Objetivo:
#   - Para df_br y df_neer_semanal, conservar solo las primeras
#     cuatro observaciones disponibles dentro de cada mes.
#   - Si un mes tiene 5 observaciones, se elimina la quinta.
# ============================================================

# ----------------------------
# Brent
# ----------------------------

df_br = df_br.copy()

df_br["fecha"] = pd.to_datetime(df_br["fecha"], dayfirst=True)
df_br = df_br.sort_values("fecha").reset_index(drop=True)

df_br["fecha_mensual"] = df_br["fecha"].dt.to_period("M")

df_br_4sem = (
    df_br
    .groupby("fecha_mensual", group_keys=False)
    .head(4)
    .reset_index(drop=True)
)

df_br_4sem = df_br_4sem.drop(columns=["fecha_mensual"])


# ----------------------------
# NEER semanal
# ----------------------------

df_neer_semanal = df_neer_semanal.copy()

df_neer_semanal["fecha"] = pd.to_datetime(df_neer_semanal["fecha"], dayfirst=True)
df_neer_semanal = df_neer_semanal.sort_values("fecha").reset_index(drop=True)

df_neer_semanal["fecha_mensual"] = df_neer_semanal["fecha"].dt.to_period("M")

df_neer_4sem = (
    df_neer_semanal
    .groupby("fecha_mensual", group_keys=False)
    .head(4)
    .reset_index(drop=True)
)

df_neer_4sem = df_neer_4sem.drop(columns=["fecha_mensual", "n_dias_semana"])

# ============================================================
# CREACIÓN DE TASAS LOGARÍTMICAS
# ============================================================
# La misma operación sirve para datos diarios y semanales.
# La frecuencia de la tasa depende de la frecuencia del dataset:
#   - diaria si el dataset es diario
#   - semanal si el dataset es semanal
# ============================================================

def crear_tasas_log(df, columnas, sufijo="_tasa_log"):
    
    df = df.copy()
    
    # Asegurar fecha en formato datetime y ordenar
    df["fecha"] = pd.to_datetime(df["fecha"], dayfirst=True)
    df = df.sort_values("fecha").reset_index(drop=True)
    
    # Crear tasa logarítmica para cada columna indicada
    for col in columnas:
        df[col + sufijo] = np.log(df[col]).diff()
    
    return df

df_omie_1_28 = crear_tasas_log(
    df=df_omie_1_28,
    columnas=["precio_marginal_(€/mwh)"]
)

df_br_4sem = crear_tasas_log(
    df=df_br_4sem,
    columnas=["dollars_per_barrel"]
)

df_neer_4sem = crear_tasas_log(
    df=df_neer_4sem,
    columnas=["usd_eur", "neer_aprox"]
)

df_gas_die_4sem = crear_tasas_log(
    df=df_gas_die_4sem,
    columnas=["gasolina_95", "diesel"]
)

# ============================================================
# PREPARACIÓN DE REGRESORES MENSUALES DE ALIMENTOS
# ============================================================
# Objetivo:
#   - df_alimentos ya contiene tasas logarítmicas mensuales.
#   - Está en formato largo:
#       mes_año | producto | tasa_log
#   - Se transforma a formato ancho:
#       fecha | alimentos_cereales | alimentos_aceites | ...
# ============================================================

df_alimentos = df_alimentos.copy()

# Convertir la fecha mensual a datetime
# Formato observado: "03-2026", es decir, mes-año
df_alimentos["fecha"] = pd.to_datetime(
    df_alimentos["mes_año"],
    format="%m-%Y"
)

# Ordenar
df_alimentos = df_alimentos.sort_values(["fecha", "producto"]).reset_index(drop=True)

# Pasar de formato largo a formato ancho
df_alimentos_wide = (
    df_alimentos
    .pivot(
        index="fecha",
        columns="producto",
        values="tasa_log"
    )
    .reset_index()
)

# Quitar el nombre del eje de columnas
df_alimentos_wide.columns.name = None

# Renombrar columnas para que sean cómodas de usar como regresores
df_alimentos_wide = df_alimentos_wide.rename(
    columns={
        col: "alim_" + str(col).lower()
        .replace(" ", "_")
        .replace(",", "")
        .replace(".", "")
        .replace("/", "_")
        .replace("-", "_")
        for col in df_alimentos_wide.columns
        if col != "fecha"
    }
)

# Ordenar definitivamente por fecha
df_alimentos_wide = df_alimentos_wide.sort_values("fecha").reset_index(drop=True)

#Se eliminan "alimentos para bebés" y "pescado fresco o rfefrigerado" porque
#tienen menos registros y con el resto de regresores de alimentos será suficiente. 

# Eliminar regresores que no se quieren incluir
columnas_eliminar_alimentos = [
    "alim_alimentos_para_bebé",
    "alim_pescado__fresco_refrigerado_o_congelado"
]

df_alimentos_wide = df_alimentos_wide.drop(
    columns=[col for col in columnas_eliminar_alimentos if col in df_alimentos_wide.columns]
)

# ===========================================================
# AJUSTES PREVIOS A LA EXPORTACIÓN
# ===========================================================

# Evitar nombres con caracteres no permitidos
df_omie_1_28 = df_omie_1_28.rename(columns = {"precio_marginal_(€/mwh)": "precio_marginal", 
                                              "precio_marginal_(€/mwh)_tasa_log": "precio_marginal_tasa_log"})


# ============================================================
# EXPORTACIÓN DE DATASETS PARA MODELOS MIDAS
# ============================================================
# Objetivo:
#   - Crear dos carpetas:
#       1) desde_2002
#       2) desde_2005
#   - En ambas, las series terminan en diciembre de 2024.
#   - Si un dataframe tiene una sola variable, se exporta directamente.
#   - Si un dataframe tiene varias variables, se crea una carpeta con
#     el nombre del dataframe y dentro se exporta un Excel por variable.
# ============================================================

from pathlib import Path


# Carpeta base donde se guardarán los archivos
ruta_base = Path(r"C:\Users\rober\Desktop\UNIR\TFM\ubicacion_datos") / "datos_midas"

# Fechas de corte
fecha_inicio_2002 = pd.Timestamp("2002-02-01")
fecha_inicio_2005 = pd.Timestamp("2005-02-01")
fecha_fin = pd.Timestamp("2024-12-31")

# Dataframes que se van a exportar
datasets_exportar = {
    "df_br_4sem": df_br_4sem,
    "df_neer_4sem": df_neer_4sem,
    "df_omie_1_28": df_omie_1_28,
    "df_gas_die_4sem": df_gas_die_4sem,
    "df_alimentos_wide": df_alimentos_wide
}

# Configuración de carpetas según fecha inicial
config_fechas = {
    "desde_2002": fecha_inicio_2002,
    "desde_2005": fecha_inicio_2005
}


for nombre_carpeta, fecha_inicio in config_fechas.items():
    
    # Crear carpeta principal: desde_2002 o desde_2005
    ruta_carpeta = ruta_base / nombre_carpeta
    ruta_carpeta.mkdir(parents=True, exist_ok=True)
    
    print("\n============================================================")
    print(f"Exportando series en carpeta: {ruta_carpeta}")
    print(f"Periodo: {fecha_inicio.date()} a {fecha_fin.date()}")
    
    for nombre_df, df in datasets_exportar.items():
        
        df_temp = df.copy()
        
        # Asegurar formato fecha
        df_temp["fecha"] = pd.to_datetime(df_temp["fecha"], dayfirst=True)
        
        # Ordenar por fecha
        df_temp = df_temp.sort_values("fecha").reset_index(drop=True)
        
        # Filtrar por periodo
        df_temp = df_temp[
            (df_temp["fecha"] >= fecha_inicio) &
            (df_temp["fecha"] <= fecha_fin)
        ].copy()
        
        # Identificar columnas de variables
        columnas_variables = [col for col in df_temp.columns if col != "fecha"]
        
        # Si solo hay una variable, exportar directamente un Excel
        if len(columnas_variables) == 1:
            
            variable = columnas_variables[0]
            
            df_export = df_temp[["fecha", variable]].copy()
            
            ruta_salida = ruta_carpeta / f"{nombre_df}.xlsx"
            
            df_export.to_excel(ruta_salida, index=False)
            
            print(f"Exportado: {ruta_salida}")
        
        # Si hay varias variables, crear carpeta con el nombre del dataframe
        else:
            
            ruta_subcarpeta = ruta_carpeta / nombre_df
            ruta_subcarpeta.mkdir(parents=True, exist_ok=True)
            
            for variable in columnas_variables:
                
                df_export = df_temp[["fecha", variable]].copy()
                
                ruta_salida = ruta_subcarpeta / f"{variable}.xlsx"
                
                df_export.to_excel(ruta_salida, index=False)
                
                print(f"Exportado: {ruta_salida}")
