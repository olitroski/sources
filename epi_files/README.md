## Script de R para archivos EPI

Revisar si falta algún paquete con los siguientes comandos

	library(stringr)
	library(dplyr)
	library(openxlsx)
	library(devtools)

Si falta instalar con (ojo los comillas)

	install.packages("libreria")

## Ejecución

- Abrir R
- Cargar librería 

		`library(devtools)`

- Cargar el source con el siguiente codigo:  

		url <- "https://raw.githubusercontent.com/olitroski/sources/master/epi_files/script_epi.r"
		source(url)

- Se abrirá una ventana para cargar la carpeta en dónde están los archivos epi.txt

Y estaría listo cuando aparece el gato...

