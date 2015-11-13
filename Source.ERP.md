## Como utilizar el Source ERP
Intrucciones para usar el source para combinar archivos originales de ERP.

> Instalar por una vez los siguientes paquetes de R, con las siguientes lineas de comando

	install.packages("xlsx")
	install.packages("dplyr")
	install.packages("devtools")


## Instrucciones para usar el Source

> **1.- Abrir R**

	Cualquier versión

> **2.- Cargar el paquete `devtools`**. Escribiendo en la linea de comandos

	library(devtools)

> **3.- Cargar el source desde internet.** Copiando y pegando en la linea de comandos la siguiente linea

	source_url("https://raw.githubusercontent.com/olitroski/sources/master/source_erp.r")

> **4.- Guardar en un objeto la dirección (dir) de la carpeta donde se está trabajando y el nombre del archivo (var) con las variables.** En la dirección de la carpeta el seprarador es `/` y el archivo de variables debe tener una configuración adecuada al archivo erp que se quiere abrir. 

	# Ejemplo
	dir <- "C:/Users/Oliver/Desktop/erp_ejemplo"
	var <- "Variables.txt"

	# Generico
	dir <- "la ruta a la carpeta de trabajo"
	var <- "nombre completo con extensión del archivo de variables

> **5.- Ejecutar el Source**

	erp(dir, var)


## Resumen
Las lineas de comando a escribir serían entonces, siguiendo el ejemplo anterior:

	library(devtools)
	source_url("https://raw.githubusercontent.com/olitroski/sources/master/source_erp.r")
	dir <- "C:/Users/Oliver/Desktop/erp_ejemplo"
	var <- "Variables.txt"
	
	# Este ultimo comando hace la magia 
	erp(dir, var) 
 


 
 