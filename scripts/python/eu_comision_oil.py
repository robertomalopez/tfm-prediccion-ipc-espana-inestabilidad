# -*- coding: utf-8 -*-
"""
Created on Sun Dec 14 12:08:23 2025

@author: rober
"""

import pandas as pd

fp_gas_die = 'C:\\Users\\rober\\Desktop\\UNIR\\TFM\\ubicacion_datos\\Weekly_Oil_Bulletin_Prices_History_maticni_4web.xlsx'
df_gas_die = pd.read_excel(fp_gas_die, header = 0)

"""Filtro a España: ES"""

df_gas_die=df_gas_die[['Consumer prices of petroleum products inclusive of duties and taxes',
                   'ES_price_with_tax_euro95',
                   'ES_price_with_tax_diesel']]

df_gas_die.dtypes


df_gas_die=df_gas_die.rename(columns={'Consumer prices of petroleum products inclusive of duties and taxes': 'fecha',
                                  'ES_price_with_tax_euro95': 'gasolina_95',
                                  'ES_price_with_tax_diesel': 'diesel'})


# 1) Quitar las dos primeras filas (descripción y unidades)
df_gas_die = df_gas_die.iloc[2:].reset_index(drop=True)

# 2) Pasar columnas a numéricas (por si vienen como string, con comas)
cols_num = ['gasolina_95', 'diesel']

for c in cols_num:
    df_gas_die[c] = (
        df_gas_die[c]
        .astype(str)                     # por si hay mezcla de tipos
        .str.replace(',', '.', regex=False)  # si el decimal es coma
    )
    df_gas_die[c] = pd.to_numeric(df_gas_die[c], errors='coerce')

# 3) Dividir entre 1000 para pasar de €/1000L a €/L
df_gas_die['gasolina_95'] = df_gas_die['gasolina_95'] / 1000
df_gas_die['diesel']      = df_gas_die['diesel'] / 1000

# 4) Pasar la fecha a datetime y luego a string 'd-m-Y'

# 1. Convertir a datetime, dejando como NaT lo que no se pueda parsear (p.ej. "Notes:")
df_gas_die['fecha'] = pd.to_datetime(df_gas_die['fecha'], errors='coerce')

# 2. Eliminar las filas que no son fecha (NaT)
df_gas_die = df_gas_die[df_gas_die['fecha'].notna()].reset_index(drop=True)

# 3. Pasar la fecha al formato string d-m-Y
df_gas_die['fecha'] = df_gas_die['fecha'].dt.strftime('%d-%m-%Y')
