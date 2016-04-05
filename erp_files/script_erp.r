## Pasos a seguir para cargar usar el source
## Noviembre 2015 - by Oliver Rojas

# Instalar los paquetes
	install.packages("xlsx")
	install.packages("dplyr")
	install.packages("devtools")
	
# Cargar la libreria y el source
	library(devtools)
	source_url("https://raw.githubusercontent.com/olitroski/sources/master/source_erp_choose.r")

# Ejecutar el source
	erp()