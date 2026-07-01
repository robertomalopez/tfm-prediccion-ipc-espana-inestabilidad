# -*- coding: utf-8 -*-
"""
Created on Thu Dec  4 19:28:50 2025

@author: rober
"""

import pandas as pd 
import numpy as np
import unicodedata

fp_coicop='C:\\Users\\rober\\Desktop\\UNIR\\TFM\\ubicacion_datos\\indices_ipc_desagregados.csv'
df_coicop = pd.read_csv(fp_coicop, sep=';', encoding='utf-8')


# ===== 1) Normalizar nombres de columnas =====
def normalizar_nombre(col):
    col = col.strip().lower()
    # quitar tildes
    col = ''.join(
        c for c in unicodedata.normalize('NFKD', col)
        if not unicodedata.combining(c)
    )
    # espacios -> "_"
    col = col.replace(' ', '_')
    return col

df_coicop.rename(columns=lambda c: normalizar_nombre(c), inplace=True)

#2) Creo columnas mes y año a partir de "periodo" ===== Formato tipo "2021M12"
tmp_fecha = df_coicop['periodo'].str.extract(r'(?P<año>\d{4})M(?P<mes>\d{2})') #vble tmp_fecha con dos columnas
tmp_fecha = tmp_fecha.astype(int) #los valores se transforman a enteros

df_coicop = df_coicop.join(tmp_fecha) #se une al dataset original 
df_coicop = df_coicop.drop(columns=['periodo']) #se descarta la columna "periodo"

df_coicop['mes_año'] = (
    df_coicop['mes'].astype(str).str.zfill(2) + '-' +
    df_coicop['año'].astype(str)
) #creacion de la columna mes_año

#3) Separar "subclases" en "codigo" y "productos" =====
# Valores tipo "01111 Arroz" o "Índice general"
df_coicop[['codigo', 'producto']] = df_coicop['subclases_ecoicop_ver.2'].str.extract(
    r'^\s*(?:(\d+(?:\.\d+)*)\s+)?(.+)$')

df_coicop.drop(columns='subclases_ecoicop_ver.2', inplace=True)

#4)Tipificar variables correctamente 
df_coicop.dtypes #total debe ser float

df_coicop['total'] = df_coicop['total'].astype(str).str.replace(",", ".", regex=False)
df_coicop['total'] = pd.to_numeric(df_coicop['total'], errors='coerce')

mask_prod= df_coicop['producto']!='Índice general'
df_coicop_prod=df_coicop[mask_prod]
mask_ind_gen=df_coicop['producto']=='Índice general'
df_coicop_ind_gen= df_coicop[mask_ind_gen]

#Me quedo con aquellos registros tales que para la categoría 'Índice' de la variable tipo_de_dato
# el valor de total no es nulo

# 2. Filas base solo para decidir productos válidos (Índice)
mask_base_indice = (
    (df_coicop_prod['tipo_de_dato'] == 'Índice') &
    (df_coicop_prod['total'].notna())
)

# 5. Filtro final: Índice + Variación mensual, años, total no nulo, productos válidos
mask_final = (
    df_coicop_prod['tipo_de_dato'].isin(['Índice', 'Variación mensual']) &
    df_coicop_prod['total'].notna()
)

df_coicop_prods_val = df_coicop_prod[mask_final]

#Creamos una columna auxiliar con los dos primeros dígitos
df_coicop_prods_val['coicop_2d'] = df_coicop_prods_val['codigo'].str[:2]

# 3) Lista de categorías que quiero
cats = [f"{i:02d}" for i in range(1, 13)]   # ['01','02',...,'12']

# 4) Diccionario: una clave por código, un dataframe por valor
dfs_coicop = {
    f"df_coicop_{c}": df_coicop_prods_val[df_coicop_prods_val['coicop_2d'] == c].copy()
    for c in cats
}

"""OJO: Se han cogido categorías para las que hay muy pocos años. Filtrar antes de usar"""

# Ejemplos de uso:
df_alimentos = dfs_coicop["df_coicop_01"]
df_alc_tab = dfs_coicop["df_coicop_02"]
df_ropa = dfs_coicop["df_coicop_03"]
df_viv_combust = dfs_coicop["df_coicop_04"]
df_muebles = dfs_coicop["df_coicop_05"]
df_sanidad = dfs_coicop["df_coicop_06"]
df_transporte = dfs_coicop["df_coicop_07"]
df_comunicaciones = dfs_coicop["df_coicop_08"]
df_ocio = dfs_coicop["df_coicop_09"]
df_enseñanaza = dfs_coicop["df_coicop_10"]
df_rest_hot = dfs_coicop["df_coicop_11"]
df_coicop_otros = dfs_coicop["df_coicop_12"]


"""Utilizaré df_alimentos pero solo para aquellos alimentos que tengan registros para al
menos 10 años antes de 2020"""

# 1) Filas antes de 2020
mask_pre2020 = df_alimentos['año'] < 2020

# 2) Contar años distintos por código (solo antes de 2020)
years_per_code = (
    df_alimentos.loc[mask_pre2020]
    .groupby('codigo')['año']
    .nunique()
)

# 3) Códigos que tienen al menos 10 años distintos
codigos_validos = years_per_code[years_per_code >= 10].index

# 4) Filtrar el dataframe completo para quedarte solo con esos alimentos
df_alimentos = df_alimentos[df_alimentos['codigo'].isin(codigos_validos)]

#Lo escribo en forma logarítmica

#Me quedo con los datos del ïndice general
df_alimentos=df_alimentos[df_alimentos['tipo_de_dato']=='Índice']

#Las transformo a tasas logarítmicas 
df_alimentos['tasa_log']= np.log(
    df_alimentos["total"] / df_alimentos["total"].shift(1)
).drop(columns=['tipo_de_dato'])

#Reseteo el índice y lo elimino como columna
df_alimentos = df_alimentos.reset_index().drop(columns=['index'])


"""Lo dejamos en el formato que están el resto de datasets"""
# 1) Creamos la columna mes-año
# 2) Nos quedamos con las columnas relevantes
df_alimentos = df_alimentos[['mes_año','producto', 'tasa_log']]


#Puesta a punto del dataset df_coicop_ind_gen
df_ipc = df_coicop_ind_gen.copy()

df_ipc["mes_año"] = pd.to_datetime(df_ipc["mes_año"], format="%m-%Y")
df_indice = df_ipc.sort_values("mes_año")
df_indice = df_indice[df_indice["tipo_de_dato"] == "Índice"].copy()

df_indice["tasa_log"] = np.log(
    df_indice["total"] / df_indice["total"].shift(1)
)

df_indice = df_indice[["mes_año", "tasa_log"]]

