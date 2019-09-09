# -*- coding: utf-8 -*-
"""
Created on Fri Jul  5 15:09:26 2019

@author: Oliver
"""

#%% camelot
import os
os.chdir('C:/Users/Oliver/Desktop')
os.listdir()

# Paginas
pg = list(range(10))
pg = [x + 1 for x in pg]
pg = ''.join(str(pg))
pg = pg.replace('[', "")
pg = pg.replace(']', "")


# Aplicar camelot
import camelot
tables = camelot.read_pdf('computacion.pdf', pages = pg)
tables.export('computacion.csv', f='csv', compress=True)


# Traer el zip
import pandas as pd
os.chdir('C:/Users/Oliver/Desktop/computacion')
archivos = os.listdir()

ues = pd.DataFrame()
for f in archivos:
    file = pd.read_csv(f)
    file.columns = ['pais', 'instituion', 'ranking', 'puntaje']
    ues = ues.append(file)

# Guardar
ues.to_excel('ues.xlsx', index = False)



