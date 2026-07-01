# -*- coding: utf-8 -*-
"""
Created on Wed Dec 10 17:49:00 2025

@author: rober
"""

import pandas as pd

fp_br='C:\\Users\\rober\\Desktop\\UNIR\\TFM\\ubicacion_datos\\brent.csv'
df_br= pd.read_csv(fp_br, header=4)


"""Transformación de datos"""
#Fecha 

#Convertimos a datetime
df_br['Week of'] = pd.to_datetime(df_br['Week of'], format='%m/%d/%Y')

#Las mostramos en formato día-mes-año (como texto)
df_br['Week of'] = df_br['Week of'].dt.strftime('%d-%m-%Y')

"""Cambio en el formato de los nombres de las columnas""" 

df_br = df_br.rename(columns={'Week of': 'fecha'})
df_br = df_br.rename(columns={'Weekly Europe Brent Spot Price FOB Dollars per Barrel': 'dollars_per_barrel'})


