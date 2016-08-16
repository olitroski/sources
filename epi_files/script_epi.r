## ----------------------------------------------------------------------------
## Script para jutnar datos desde los archivos epi de actividorm para las wawas
## Oliver Rojas - 15.08.2016 - INTA, Lab. de Sueño, U. de Chile
## ----------------------------------------------------------------------------
setwd("D:/OneDrive/INTA/Sussanne Reyes/Script EPI/epi")
library(stringr); library(dplyr); library(openxlsx)
rm(list=ls())

## Adquisicion y postura de datos
# Folder Choose - Windows olny
folder=choose.dir()
setwd(folder)

archivos <- dir()
archivos <- archivos[grep("epi", archivos)]

# Funcion para la desviacion estandar o variabilidad --------------------------
sd <- function(vector) {
     if (length(vector) <= 1) {
          return(NA)
     } else {
          mean <- mean(vector, na.rm=TRUE)
          var <- sum((vector - mean)^2)/length(vector)
          stat <- sqrt(var)
          return(stat)
     }          
}


# Funcion para abrir archivos -------------------------------------------------
leer_datos  <- function(file) {
#      file <- archivos[1]
     # Datos globales
     lines <- length(readLines(file)) - 4
     datos <- read.table(file, header=TRUE, sep=";", stringsAsFactors=FALSE, nrows=lines, dec=",")
     names(datos) <- c("id", "periodo", "hora", "estado", "actividad", "dur_min", "prom_act_min", "num_epi", "epi_estado")

     # Detalle del id
     sujeto <- as.data.frame(str_split_fixed(datos$id, " ", 3), stringsAsFactors=FALSE)
     names(sujeto) <- c("idnum", "idnom", "visit")
     sujeto$visit <- sub("_fecha", "", sujeto$visit)
     
     datos <- cbind(sujeto, datos)
     datos <- select(datos, -id)
     
     # Resultado
     filas <- dim(datos)[1]
     cat("   Archivo: ", file, " - Recuento ", filas, " filas\n\n", sep="")
     return(datos)
}


# Funcion para sacar las stats -------------------------------------------------
# A partir de los datos de ID -> Visit -> Datos de los 3 dias
stats <- function(data) {
#      data <- data_dia     #Pasar a comentario depués
     
     # Antecedentes
     idnum <- data$idnum[1]
     idnom <- data$idnom[1]
     visit <- data$visit[1]
     
     # Creacion de base por dia     
     data_stats <- NULL
     dias <- c("Dia 01", "Dia 02", "Dia 03")
     
     for (dia in dias) {
          # Captura del dato
          temp <- filter(data, periodo==dia)
          temp <- arrange(temp, hora)
          temp <- slice(temp, 1:2)
               laten <- temp$dur_min[1]
               sleep <- temp$dur_min[2]
               period <- temp$periodo[1]
          
          # Creacion de una base
          base <- data.frame(idnum=idnum, idnom=idnom, visit=visit, dia=period, lat=laten, slp=sleep, stringsAsFactors=FALSE)
          data_stats <- rbind(data_stats, base)
     }

# Antes de hacer cambios corregir si no tiene los 3 días
suple <- data_stats[1,]
suple[,5:6] <- NA
if (dim(data_stats)[1] == 2) {
     data_stats <- rbind(data_stats, suple)
     data_stats$dia <- c("Dia 01", "Dia 02", "Dia 03")   # La cuchufleta maxima para no hacer el mix perfecto de dias missing
} else if (dim(data_stats)[1] == 1){
     data_stats <- rbind(data_stats, suple, suple)
     data_stats$dia <- c("Dia 01", "Dia 02", "Dia 03")
} else {
     data_stats$dia <- c("Dia 01", "Dia 02", "Dia 03")
}

# Con la data lista, hacer calculos y un reshape
data_stats$lat_mean <- mean(data_stats$lat, na.rm=TRUE)
data_stats$lat_sd <- sd(data_stats$lat)
data_stats$slp_mean <- mean(data_stats$slp, na.rm=TRUE)
data_stats$slp_sd <- sd(data_stats$slp)

data_stats <- reshape(data_stats, timevar="dia", idvar=c("idnum", "idnom", "visit", 
              "lat_mean", "lat_sd", "slp_mean", "slp_sd"), direction="wide")

# Por fin CTM
return(data_stats)
}



## Antecedentes del analisis
# Combinar archivos de la carpeta
datos <- NULL
for (file in archivos) {
     cat("Cargando: ", file, "\n")
     datos <- rbind(datos, leer_datos(file))
}


# Parte 1 - Filtrar por Sujeto
base_datos <- NULL
sujetos <- unique(datos$idnum)
for (id in sujetos) {
# id <- sujetos[1]  # Ejemplo pasar a comentario
data_id <- filter(datos, idnum==id)


     # Parte 2 - Filtar por perido/visita
     visitas <- unique(data_id$visit)
     for (vis in visitas) {
          # visit <- visitas[1]   # Ejemplo pasar a comentario
          data_visit <- filter(data_id, visit==vis)


          # Parte 3 - Filtrar por días a considerar
          dias <- c("Dia 01", "Dia 02", "Dia 03")
          data_dia <- filter(data_visit, periodo %in% dias)
          temp_dia <- stats(data_dia)
          base_datos <- rbind(base_datos, temp_dia)
     }
}
base_datos <- arrange(base_datos, idnum, visit)


## Pasar a Excel
write.xlsx(base_datos, file="BaseDatos.xlsx", sheetName="epi")