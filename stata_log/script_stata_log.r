# ------------------------------------------------------------------------------------- #
# ---- Script para capturar resultados de regresión lineal de un log de Stata --------- #
# ---- supone <reg, contrast, margins> en versión loop y normal, log en txt ----------- #
# ---- resultado a Excel. By Oliver Rojas, 12.06.2018, Lab.Sueño, INTA, Uchile -------- #
# ------------------------------------------------------------------------------------- #
library(dplyr); library(stringr)
setwd("D:/")


## Capturar las lineas de inicio de cada regresión --------------------------------------
# Lee el log
lineas <- readLines("mylog_loop.txt")

# Indice con todas las ocurrencias
indice <- c(grep("Source", lineas), grep("-----", lineas), grep("Model", lineas),
            grep("Residual", lineas), grep("Total", lineas))

# Si hay cuatro seguidas indica regresion
indice <- data.frame(indice = sort(indice), diff = 0)
for (i in 1:dim(indice)[1]-1){
     indice$diff[i] <- indice$indice[i+1] - indice$indice[i]
}
indice <- filter(indice, diff == 1)
   
# Identificar la primera linea de la regrsión
for (i in 2:dim(indice)[1]){
     indice$diff[i] <- indice$indice[i] - indice$indice[i-1]
}
indice$diff[1] <- 50   # para el primero
indice <- filter(indice, diff > 1) %>% select(ini = indice) 
indice$fin <- c(indice$ini[-1], length(lineas))-1
indice$reg.name <- paste("reg.", row.names(indice), sep="")


## Funcion de captura de la regresion
# ini <- indice[3,1];  fin <- indice[3,2]; linefile <- lineas
read.reg <- function(ini = NULL, fin = NULL, linefile = NULL){
     # Lineas del bloque a trabajar
     reglines <- linefile[ini:fin]

     # Buscar el final de la regresion usando la linea final y capturar
     count <- 0
     i <- 1
     while (count < 5) {
          if (grepl("----", reglines[i]) == TRUE){count <- count + 1}
          i <- i + 1
     }

     reglines <- reglines[1:(i-1)]
     
     # Lineas 1 a 6 datos iniciales
     head <- reglines[1:6]
     head <- str_sub(head, start = -25, end = -1)
     head <- data.frame(str_split_fixed(head, "=", 2), stringsAsFactors=FALSE)
     head <- mutate(head, X2 = as.numeric(X2))
     
     
     # Modelo
     # Si termina en numero es un coeficiente, si tiene letras pero no numero sería el comando
     model <- reglines[9:length(reglines)]
     model <- model[-grep("---", model)]
     model <- sub("\\|", "", model)
          
     # determinar que es cada linea
     coef.num <- data.frame(n = grep("[0-9]$", model), coef = "numero")
     coef.let <- data.frame(n = grep("[a-z]", model))
     coef.cmd <- merge(coef.let, coef.num, by="n", all.x=TRUE)
     coef.cmd <- slice(coef.cmd, -1) %>% filter(is.na(coef)==TRUE)
     coef.cmd <- coef.cmd$n 
     coef.num <- coef.num$n
     coef.let <- coef.let$n
     
 
 
     

     
}

























