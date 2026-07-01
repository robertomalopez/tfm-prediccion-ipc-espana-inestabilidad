# -*- coding: utf-8 -*-
"""
Created on Sun Dec 14 11:02:45 2025

@author: rober
"""

import pandas as pd 
import numpy as np

fp_omie="C:\\Users\\rober\\Desktop\\UNIR\\TFM\\ubicacion_datos\\datos_historicos_OMIE.csv"

# 1. Carga sin parse_dates para inspeccionar columnas
df_omie = pd.read_csv(
    fp_omie,
    sep=";",          # si dudas del separador, prueba sep=None, engine="python"
    decimal=",",
    encoding="utf-8"
)

"""Me quedo con los datos de España"""
df_omie=df_omie[df_omie['País']=='Spain']
df_omie=df_omie.drop(columns=['País'])

"""Creo las medias para los precios que están en un mismo día: precios diarios"""
#Pasamos fecha a formato date_time
df_omie['Fecha'] = pd.to_datetime(df_omie['Fecha'], dayfirst=True)
df_omie = df_omie.groupby(['Fecha'])['Precio marginal (€/MWh)'].mean()


import unicodedata

#Pasar el índice (fecha) a columna y que el índice sea numérico
df_omie = df_omie.reset_index()          # la antigua fecha pasa a columna


#Pasar la fecha de datetime a string con formato d-m-Y
df_omie['Fecha'] = pd.to_datetime(df_omie['Fecha']).dt.strftime('%d-%m-%Y')

#Normalizar nombres de columnas: minúsculas, sin tildes, espacios -> "_"
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

df_omie.columns = [normalizar_col(c) for c in df_omie.columns]