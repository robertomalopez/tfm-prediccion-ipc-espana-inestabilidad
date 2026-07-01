# -*- coding: utf-8 -*-
"""
Created on Wed May  6 17:18:42 2026

@author: rober
"""

import re
import numpy as np
import pandas as pd

# ============================================================
# 1. Rutas
# ============================================================

fp_cam_mon = 'C:\\Users\\rober\\Desktop\\UNIR\\TFM\\ubicacion_datos\\cambio_de_moneda.csv'
fp_pesos = 'C:\\Users\\rober\\Desktop\\UNIR\\TFM\\ubicacion_datos\\pesos_importacion.csv'

# ============================================================
# 2. Lectura del dataset diario de tipos de cambio
# ============================================================

df_fx = pd.read_csv(fp_cam_mon, sep=',', encoding='utf-8')

df_fx['Date'] = pd.to_datetime(df_fx['Date'])
df_fx = df_fx.sort_values('Date').reset_index(drop=True)

# Año de cada observación diaria. Servirá para asignar los pesos anuales.
df_fx['year'] = df_fx['Date'].dt.year


# ============================================================
# 3. Serie 1: USD/EUR
# ============================================================
# El dataset contiene USD como dólares por euro.
# Para obtener euros por dólar, se toma la inversa.

df_fx['usd_eur'] = 1 / df_fx['USD']

#Lo pasamos a una variable variacional, la tasa logarítmica, ya que haremos lo
#mismo con el NEER

###############################################################################################

df_fx = df_fx.sort_values("Date")


df_fx["dlog_usd_eur"] = (
    np.log(df_fx["usd_eur"]) 
    - np.log(df_fx["usd_eur"].shift(1))
)


# ============================================================
# 4. Lectura y limpieza del dataset anual de pesos
# ============================================================

df_pesos = pd.read_csv(fp_pesos, sep=',', encoding='utf-8')

# En el CSV del BCE, TIME PERIOD contiene el año.
df_pesos = df_pesos.rename(columns={'TIME PERIOD': 'year'})
df_pesos['year'] = df_pesos['year'].astype(int)

# Nos quedamos solo con columnas de pesos de importación de bienes.
# En este dataset aparecen como "Manufactured products".
cols_bienes = [
    col for col in df_pesos.columns
    if 'Import weight - Manufactured products' in col
]

df_pesos_bienes = df_pesos[['year'] + cols_bienes].copy()


# ============================================================
# 5. Pasar los pesos de formato ancho a formato largo
# ============================================================
# El objetivo es pasar de:
# año | peso Alemania | peso Estados Unidos | ...
#
# a:
# año | pais | codigo_pais | peso

df_pesos_largo = df_pesos_bienes.melt(
    id_vars='year',
    var_name='serie_bce',
    value_name='peso'
)

# Extraemos el nombre del país desde el nombre largo de la columna
df_pesos_largo['pais'] = df_pesos_largo['serie_bce'].str.extract(
    r'counterpart area: (.*?), Not applicable'
)

# Extraemos el código ISO-2 del país desde el identificador BCE:
df_pesos_largo['codigo_pais'] = df_pesos_largo['serie_bce'].str.extract(
    r'WTS\.A\.ES\.([A-Z]{2})\.'
)

df_pesos_largo['peso'] = pd.to_numeric(df_pesos_largo['peso'], errors='coerce')

df_pesos_largo = df_pesos_largo.dropna(
    subset=['year', 'codigo_pais', 'peso']
)


# ============================================================
# 6. Relación país-divisa
# ============================================================
# Esta tabla permite conectar los socios comerciales del BCE
# con las columnas del dataset diario de tipos de cambio.
#
# Solo acabarán utilizándose las divisas que también existan
# como columnas en df_fx.

pais_a_divisa = {
    'AE': 'AED',
    'AR': 'ARS',
    'AU': 'AUD',
    'BR': 'BRL',
    'BG': 'BGN',
    'CA': 'CAD',
    'CH': 'CHF',
    'CL': 'CLP',
    'CN': 'CNY',
    'CO': 'COP',
    'CZ': 'CZK',
    'DK': 'DKK',
    'DZ': 'DZD',
    'GB': 'GBP',
    'HK': 'HKD',
    'HU': 'HUF',
    'ID': 'IDR',
    'IL': 'ILS',
    'IN': 'INR',
    'IS': 'ISK',
    'JP': 'JPY',
    'KR': 'KRW',
    'MA': 'MAD',
    'MX': 'MXN',
    'MY': 'MYR',
    'NO': 'NOK',
    'NZ': 'NZD',
    'PE': 'PEN',
    'PH': 'PHP',
    'PL': 'PLN',
    'RO': 'RON',
    'RU': 'RUB',
    'SA': 'SAR',
    'SE': 'SEK',
    'SG': 'SGD',
    'TH': 'THB',
    'TR': 'TRY',
    'TW': 'TWD',
    'UA': 'UAH',
    'US': 'USD',
    'ZA': 'ZAR',

    # Países de la eurozona.
    # Se asignan a EUR, pero normalmente quedarán fuera porque EUR
    # no debería aparecer como tipo de cambio frente al euro.
    'AT': 'EUR',
    'BE': 'EUR',
    'CY': 'EUR',
    'DE': 'EUR',
    'EE': 'EUR',
    'ES': 'EUR',
    'FI': 'EUR',
    'FR': 'EUR',
    'GR': 'EUR',
    'HR': 'EUR',
    'IE': 'EUR',
    'IT': 'EUR',
    'LT': 'EUR',
    'LU': 'EUR',
    'LV': 'EUR',
    'MT': 'EUR',
    'NL': 'EUR',
    'PT': 'EUR',
    'SI': 'EUR',
    'SK': 'EUR',
}


df_pesos_largo['divisa'] = df_pesos_largo['codigo_pais'].map(pais_a_divisa)


# ============================================================
# 7. Definir y aplicar la cesta final de divisas usando df_fx
# ============================================================

cols_base_fx = ['Date', 'year', 'dlog_usd_eur']

divisas_disponibles_fx = [
    col for col in df_fx.columns
    if col not in cols_base_fx
]

# Divisas sin ningún valor nulo en todo el periodo diario
divisas_fx_completas = [
    divisa for divisa in divisas_disponibles_fx
    if df_fx[divisa].notna().all()
]

divisas_fx_excluidas = sorted(
    set(divisas_disponibles_fx) - set(divisas_fx_completas)
)

print("Divisas excluidas por tener algún valor nulo en df_fx:")
print(divisas_fx_excluidas)

print("Divisas con serie diaria completa:")
print(divisas_fx_completas)

# hay que eliminar de df_fx las columnas de divisas incompletas.
df_fx = df_fx[cols_base_fx + divisas_fx_completas].copy()


# ============================================================
# 8. Filtrar pesos usando solo divisas que siguen en df_fx
# ============================================================

df_pesos_largo = df_pesos_largo[
    df_pesos_largo['divisa'].isin(divisas_fx_completas)
].copy()

# ============================================================
# 9. Agregar pesos por divisa y año
# ============================================================

#Se hace porque diferentes páises pueden tener la misma divisa. 
#Se suman los pesos de aquellos países que tienen la misma divisa. 
df_pesos_divisa = (
    df_pesos_largo
    .groupby(['year', 'divisa'], as_index=False)['peso']
    .sum()
)

#se recalculan los pesos para las divisas que se utilizan finalmente para
#el cálculo del NEER. 
df_pesos_divisa['peso_renorm'] = (
    df_pesos_divisa['peso']
    / df_pesos_divisa.groupby('year')['peso'].transform('sum')
)


# ============================================================
# 10. Pasar pesos anuales a formato ancho
# ============================================================

df_pesos_anuales = (
    df_pesos_divisa
    .pivot(index='year', columns='divisa', values='peso_renorm')
    .reset_index()
)

divisas_neer = [
    col for col in df_pesos_anuales.columns
    if col != 'year'
]

# Renombramos pesos para evitar confundir columnas de tipos de cambio
# con columnas de pesos.
df_pesos_anuales = df_pesos_anuales.rename(
    columns={divisa: f'{divisa}_peso' for divisa in divisas_neer}
)

cols_pesos = [f'{divisa}_peso' for divisa in divisas_neer]


# ============================================================
# 11. Merge final
# ============================================================

df = df_fx.merge(
    df_pesos_anuales,
    on='year',
    how='left'
)

    
# ============================================================
# 12. Cálculo del NEER aproximado
# ============================================================
# En este punto:
# - df ya contiene solo las divisas con serie diaria completa.
# - divisas_neer contiene solo las divisas que se usan en el índice.
# - cols_pesos contiene los pesos anuales renormalizados de esas divisas.
#
# E_i,t está expresado como unidades de moneda extranjera por euro.
# El índice se normaliza a 100 en la primera fecha disponible.

cols_neer = ['Date', 'year', 'dlog_usd_eur', 'usd_eur'] + divisas_neer + cols_pesos

df_neer = df[cols_neer].copy()


# Valores base: primer dato disponible para cada divisa.
# Estos valores sirven para normalizar el índice a 100.
base = df_neer.loc[df_neer.index[0], divisas_neer]

# Cálculo en logaritmos para evitar problemas numéricos:
#
# log(NEER_t) = log(100) + sum_i peso_i,t * log(E_i,t / E_i,0)
#
# donde:
# - E_i,t es el tipo de cambio diario de la divisa i
# - E_i,0 es el tipo de cambio de la divisa i en la fecha base
# - peso_i,t es el peso comercial renormalizado de la divisa i
#   correspondiente al año t

df_neer['log_neer_aprox'] = np.log(100)

for divisa in divisas_neer:
    df_neer['log_neer_aprox'] += (
        df_neer[f'{divisa}_peso']
        * np.log(df_neer[divisa] / base[divisa])
    )

# Índice NEER aproximado en niveles
df_neer['neer_aprox'] = np.exp(df_neer['log_neer_aprox'])

###############################################################################

df_neer_midas = df_neer[
    ['Date', 'usd_eur', 'neer_aprox']
].copy()

df_neer_midas = df_neer_midas.rename(columns={'Date': 'fecha'})

df_neer_midas['fecha']=df_neer_midas['fecha'].dt.strftime('%d-%m-%Y')


###############################################################################
# Variable final para el modelo:
# Variación logarítmica diaria del NEER aproximado.
df_neer['dlog_neer_aprox'] = df_neer['log_neer_aprox'].diff()

# ============================================================
# 13. Dataset final
# ============================================================

df_cam_mon = df_neer[
    ['Date', 'dlog_usd_eur', 'dlog_neer_aprox']
].copy()

df_cam_mon = df_cam_mon.rename(columns={'Date': 'fecha'})

# La fecha se formatea al final para no perder funcionalidad temporal
# durante los cálculos.
df_cam_mon['fecha'] = df_cam_mon['fecha'].dt.strftime('%d-%m-%Y')





