## Como utilizar el Source ERP
Intrucciones para usar el source para combinar archivos originales de ERP.

> Instalar por una vez los siguientes paquetes de R, con las siguientes lineas de comando

	install.packages("xlsx")
	install.packages("dplyr")
	install.packages("devtools")


## Instrucciones para usar el Source

> **1.- Abrir R**

	Cualquier versión

> **2.- Cargar el paquete `devtools` escribiendo**

	library(devtools)

> **3.- Guardar la url del source en un objeto que llamaremos url y cargar el source**

	url <- "https://raw.githubusercontent.com/olitroski/sources/master/source_erp.r"
	source_url(dir)

> **4.- Guardar en un objeto la dirección de la carpeta donde se está trabajando y el nombre del archivo con las variables.** Nota: En la dirección de la carpeta el seprarador es `/` y que el archivo de variables debe tener una configuración adecuada. 

	dir <- "C:/Users/Oliver/Desktop/erp_ejemplo"
	var <- "Variables.txt"

> **5.- Ejecutar el Source**

	
	erp(dir, var) 


 
 