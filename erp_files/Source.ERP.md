## Como utilizar el Source ERP
Intrucciones para usar el source para combinar archivos originales de ERP.

En una carpeta  del PC cualquiera copiar todos los archivos originales de ERP, más el archivo con los nombres de variables. **Debe ser una copia** porque se hacen modificaciones a los originales e inclusive se eliminan.

> Instalar por una vez los siguientes paquetes de R, escribiendo las siguientes lineas de comando en la consola de R.

	install.packages("xlsx")
	install.packages("dplyr")
	install.packages("devtools")


## Instrucciones para usar el Source

> **1.- Abrir R**
Cualquier versón sirve, si no se tiene ninguna es recomendable RStudio que es más amable y se puede descargar haciendo clic acá.

[https://download1.rstudio.org/RStudio-0.99.489.exe](https://download1.rstudio.org/RStudio-0.99.489.exe "https://download1.rstudio.org/RStudio-0.99.489.exe")


> **2.- Cargar el paquete `devtools`**. Escribiendo en la linea de comandos

	library(devtools)

> **3.- Cargar el source desde internet.** Copiando y pegando en la linea de comandos la siguiente linea

Version 1 **Usar la otra mejor**

	source_url("https://raw.githubusercontent.com/olitroski/sources/master/source_erp.r")

Version 2 del source con ventanitas. **Usar esta**

	source_url("https://raw.githubusercontent.com/olitroski/sources/master/source_erp_choose.r")

> **4.- Usar la función**

	erp()

## Resumen
Las lineas de comando a escribir serían entonces, siguiendo el ejemplo anterior. Tambien se puede descargar el script:

	# Instalar los paquetes
		install.packages("xlsx")
		install.packages("dplyr")
		install.packages("devtools")
		
	# Cargar la libreria y el source
		library(devtools)
		source_url("https://raw.githubusercontent.com/olitroski/sources/master/source_erp_choose.r")
	
	# Ejecutar el source
		erp()
 

## Version 1 - Antigua
> **Version antigua** Guardar en un objeto la dirección (dir) de la carpeta donde se está trabajando y el nombre del archivo (var) con las variables. En la dirección de la carpeta el seprarador es `/` y el archivo de variables debe tener una configuración adecuada al archivo erp que se quiere abrir. 

	# Ejemplo
	dir <- "C:/Users/Oliver/Desktop/erp_ejemplo"
	var <- "Variables.txt"

	# Generico
	dir <- "la ruta a la carpeta de trabajo"
	var <- "nombre completo con extensión del archivo de variables

> **Version antigua** Ejecutar el Source

	erp(dir, var)
 
 