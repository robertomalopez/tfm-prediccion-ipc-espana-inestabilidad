# -*- coding: utf-8 -*-
"""
Created on Tue May  5 17:32:43 2026

@author: rober
"""

import pandas as pd
import numpy as np
from functools import reduce

from brent import df_br
from cambio_de_moneda import df_cam_mon
from omiehist import df_omie
from eu_comision_oil import df_gas_die
from indices_ipc_desagregados import df_alimentos

from indices_ipc_desagregados import df_indice


def media_mensual_1_28(df, fecha_col, value_cols, date_format=None):
    """
    Convierte una tabla diaria o semanal a mensual usando solo los días 1-28
    y calculando la media mensual de las columnas indicadas.
    """
    df = df.copy()

    # Convertir fecha, con formato explícito si se indica
    df[fecha_col] = pd.to_datetime(
        df[fecha_col],
        format=date_format,
        errors="raise"
    )

    # Usar solo días 1-28 de cada mes
    df = df[df[fecha_col].dt.day <= 28]

    # Crear fecha mensual común
    df["fecha_mensual"] = df[fecha_col].dt.to_period("M").dt.to_timestamp()

    # Media mensual
    df_m = (
        df.groupby("fecha_mensual", as_index=False)[value_cols]
        .mean()
    )

    return df_m


#Brent 
df_br_m = media_mensual_1_28(
    df=df_br,
    fecha_col="fecha",
    value_cols=["dollars_per_barrel"],
    date_format="%d-%m-%Y"
)

df_br_m = df_br_m.rename(
    columns={"dollars_per_barrel": "brent_dollars_per_barrel"}
)

#Cambio de moneda
df_cam_mon_m = media_mensual_1_28(
    df=df_cam_mon,
    fecha_col="fecha",
    value_cols=["dlog_usd_eur", "dlog_neer_aprox"],
    date_format="%d-%m-%Y"   # si el año tiene 4 dígitos
)

#omie
df_omie_m = media_mensual_1_28(
    df=df_omie,
    fecha_col="fecha",
    value_cols=["precio_marginal_(€/mwh)"],
    date_format="%d-%m-%Y"
)

df_omie_m = df_omie_m.rename(
    columns={"precio_marginal_(€/mwh)": "omie_precio"}
)


#gasolina_y_diesel
df_gas_die_m = media_mensual_1_28(
    df=df_gas_die,
    fecha_col="fecha",
    value_cols=["gasolina_95", "diesel"],
    date_format="%d-%m-%Y"
)

#alimentos
#ya está en tasa logarítmica y frecuencia mensual, no hay que promediar
#está en formato largo

df_alim = df_alimentos.copy()

df_alim["fecha_mensual"] = pd.to_datetime(df_alim["mes_año"])

df_alim_m = (
    df_alim
    .pivot_table(
        index="fecha_mensual",
        columns="producto",
        values="tasa_log",
        aggfunc="mean"
    )
    .reset_index()
)


#INCISO: se observan únicamente dos alimentos que no tienen registros desde 2002, sino 
#desde 2007, por lo que sencillamente las eliminamos 

df_alim_m = df_alim_m.drop(
    columns=[
        "Alimentos para bebé",
        "Pescado,  fresco, refrigerado o congelado"
    ]
)


import unicodedata

#Se normalizan nombres de columnas: minúsculas, sin tildes, espacios -> "_"
def normalizar_col(col):
    col = col.lower()
    # quitar acentos
    col = ''.join(
        c for c in unicodedata.normalize('NFKD', col)
        if not unicodedata.combining(c)
    )
    # espacios a guiones bajos
    col = col.replace(' ', '_')
    return col

df_alim_m.columns = [normalizar_col(c) for c in df_alim_m.columns]

df_alim_m = df_alim_m.rename(
    columns={c: f"d_log_{c}" for c in df_alim_m.columns if c != "fecha_mensual"}
)

#ipc mensual. Variable objetivo
df_ipc = df_indice.copy()

df_ipc_m = (
    df_ipc[["mes_año", "tasa_log"]]
    .rename(columns={"tasa_log": "y_ipc_general", "mes_año": "fecha_mensual"})
)



#Unión de regresores 
#Se deben unir todos aquellos regresores que todavía no estén en formato de variación
tablas = [df_br_m,
    df_omie_m,
    df_gas_die_m]

df_reg_mensual = reduce(
    lambda left, right: pd.merge(left, right, on="fecha_mensual", how="left"),
    tablas
)

df_reg_mensual = (
    df_reg_mensual
    .sort_values("fecha_mensual")
    .reset_index(drop=True)
)



#Ahora se obtiene su tasa logarítmica
df_reg_log = df_reg_mensual.copy()

df_reg_log["fecha_mensual"] = pd.to_datetime(df_reg_log["fecha_mensual"])
df_reg_log = df_reg_log.sort_values("fecha_mensual")

cols_precios = df_reg_log.columns.drop("fecha_mensual")

df_reg_log[cols_precios] = np.log(df_reg_log[cols_precios]).diff()

df_reg_log = df_reg_log.rename(
    columns={col: f"dlog_{col}" for col in cols_precios}
)

df_reg_log = df_reg_log.reset_index(drop=True)


#Tabla. y + todos los regresores
tablas_log = [df_ipc_m,
    df_reg_log,
    df_cam_mon_m,
    df_alim_m]

df_modelo_mensual = reduce(
    lambda left, right: pd.merge(left, right, on="fecha_mensual", how="left"),
    tablas_log
)

df_modelo_mensual = (
    df_modelo_mensual
    .sort_values("fecha_mensual")
    .reset_index(drop=True)
)

#Acotación de serie
df_modelo_mensual = (
    df_modelo_mensual[
        (df_modelo_mensual["fecha_mensual"] >= "2005-2-01") &
        (df_modelo_mensual["fecha_mensual"] <= "2024-12-01")
    ]
    .sort_values("fecha_mensual")
    .reset_index(drop=True)
)

#Tabla. y + todos los regresores excepto alimentos
tablas_exc_alim = [df_ipc_m,
    df_reg_log,
    df_cam_mon_m]

df_exc_alim = reduce(
    lambda left, right: pd.merge(left, right, on="fecha_mensual", how="left"),
    tablas_exc_alim
)

df_exc_alim = (
    df_exc_alim
    .sort_values("fecha_mensual")
    .reset_index(drop=True)
)

#Acotación de serie
df_exc_alim = (
    df_exc_alim[
        (df_exc_alim["fecha_mensual"] >= "2005-2-01") &
        (df_exc_alim["fecha_mensual"] <= "2024-12-01")
    ]
    .sort_values("fecha_mensual")
    .reset_index(drop=True)
)



#Tabla. y + todos los regresores excepto alimentos y combustibles
tablas_exc_alim_comb = [df_ipc_m, 
                        df_br_m, 
                        df_omie_m,
                        df_cam_mon_m]

df_exc_alim_comb = reduce(
    lambda left, right: pd.merge(left, right, on="fecha_mensual", how="left"),
    tablas_exc_alim_comb
)

df_exc_alim_comb = (
    df_exc_alim_comb
    .sort_values("fecha_mensual")
    .reset_index(drop=True)
)

#Acotación de serie
df_exc_alim_comb = (
    df_exc_alim_comb[
        (df_exc_alim_comb["fecha_mensual"] >= "2002-2-01") &
        (df_exc_alim_comb["fecha_mensual"] <= "2024-12-01")
    ]
    .sort_values("fecha_mensual")
    .reset_index(drop=True)
)

#Acotación de serie ipc_m
df_ipc_m = (
    df_ipc_m[
        (df_ipc_m["fecha_mensual"] >= "2002-2-01") &
        (df_ipc_m["fecha_mensual"] <= "2024-12-01")
    ]
    .sort_values("fecha_mensual")
    .reset_index(drop=True)
)



#Exportación de tablas
df_modelo_mensual.to_excel("df_modelo_mensual.xlsx", index=False)
df_exc_alim.to_excel("df_excepto_alim.xlsx", index=False)
df_exc_alim_comb.to_excel("df_excepto_alim_comb.xlsx", index=False)
df_alim_m.to_excel("df_alim_m.xlsx", index=False)
df_ipc_m.to_excel("df_ipc_m.xlsx", index=False)

#Series de cada regresor por separado
import os

# Lista de dataframes
lista_dfs = [
    df_reg_log,
    df_cam_mon_m,
    df_ipc_m
]

# Carpeta de salida
carpeta_salida = "regresores_excel"
os.makedirs(carpeta_salida, exist_ok=True)

for i, df in enumerate(lista_dfs, start=1):
    df = df.copy()

    # Asegurar que fecha_mensual existe
    if "fecha_mensual" not in df.columns:
        raise ValueError(f"El dataframe {i} no tiene columna 'fecha_mensual'")

    # Todas las columnas excepto la fecha son regresores
    cols_regresores = [c for c in df.columns if c != "fecha_mensual"]

    for col in cols_regresores:
        df_export = df[["fecha_mensual", col]].copy()

        # Nombre de archivo seguro
        nombre_archivo = (
            col.replace("/", "_")
               .replace("\\", "_")
               .replace(":", "_")
               .replace("*", "_")
               .replace("?", "_")
               .replace('"', "_")
               .replace("<", "_")
               .replace(">", "_")
               .replace("|", "_")
        )

        ruta = os.path.join(carpeta_salida, f"{nombre_archivo}.xlsx")

        df_export.to_excel(ruta, index=False)

