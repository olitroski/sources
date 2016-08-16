## Script de R para archivos EPI

Revisar si falta algún paquete 

	library(stringr)
	library(dplyr)
	library(openxlsx)
	library(devtools)

Si falta instalar con 

	install.packages("libreria")

## Ejecución

- Abrir R
- Cargar librería `devtools`
- Cargar el source con el siguiente codigo:  

		url <- "https://raw.githubusercontent.com/olitroski/sources/master/epi_files/script_epi.r"
		source(url)

- Se abrirá una ventana para cargar la carpeta en dónde están los archivos epi.txt

