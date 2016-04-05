######################################################################
## Script para funcion de capturar datos y combinar archivos de ERP ##
## by Oiver Rojas - Noviembre 2015 - Lab.Sueño - Inta - Uchile      ##
######################################################################
erp <- function(directorio, nomvariable) {
require(dplyr)
require(xlsx)

## Setea directorio, captura nombre de variables y las borra del directorio
# directorio <- "C:/Users/Oliver/Desktop/erp_ejemplo"
setwd(directorio)


# Leer los nombres de archivos
# nomvariable <- "Variables.txt"
rawnames <- readLines(nomvariable)
for (i in 1:length(rawnames)) {
	n <- read.table(text = rawnames[i], stringsAsFactors = FALSE)
	n <- as.character(n[1,])
	eval(parse(text=paste("n", i, "<- n", sep="")))
	rm(n)
}
unlink(nomvariable)


## Leer cada archivo, dejarlo pituco, borra el original, guarda el nuevo
# Cargar el directorio
archivos <- dir()

# Loop para cada archivo
for (file in archivos)  {
	
	# Cargar el archivo
	rawfile  <- readLines(file)
	
	# Leer cada fila y ponerle nombres	# Leer cada linea
	r1 <- read.table(text = rawfile[1]); names(r1) <- n1
	r2 <- read.table(text = rawfile[2]); names(r2) <- n2
	r3 <- read.table(text = rawfile[3]); names(r3) <- n3
	r4 <- read.table(text = rawfile[4]); names(r4) <- n4
	r5 <- read.table(text = rawfile[5]); names(r5) <- n5
	r6 <- read.table(text = rawfile[6]); names(r6) <- n6
	r7 <- read.table(text = rawfile[7]); names(r7) <- n7
	r8 <- read.table(text = rawfile[8]); names(r8) <- n8
	
	# Combinar cada linea
	nombre <- file
	erp.id <- cbind(r1,r2,r3,r4,r5,r6,r7,r8)
	erp.id <- mutate(erp.id, archivo = nombre)
	
	# Borra el archivo original
	unlink(file)
	
	# Guarda el archivo como .dat	
	nombre <- paste(nombre, ".dat", sep="")
	write.table(erp.id, nombre,row.names = FALSE, quote = FALSE)
}
	
	
## Combinar los archivos individuales en un master, borrar y enviar a un zip
archivos <- dir()

# Combina
datos <- NULL
for (erp in archivos) {
	df <- read.table(erp, header = TRUE)
	datos <- rbind(datos, df)
}


# Mover los archivos a una carpeta
dir.create("archivos_originales")
file.copy(from=archivos, to=file.path("archivos_originales", archivos), copy.mode = TRUE)
unlink(archivos)


# Crear un excel con el archivo final
write.xlsx(datos, "erp.xlsx", row.names=FALSE, sheetName="ERP")
write.table(rawnames, nomvariable, row.names = FALSE, quote = FALSE, col.name = FALSE)
}



