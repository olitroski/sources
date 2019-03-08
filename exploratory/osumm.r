#' @title Olitos's Summary, estadisticas descriptivas 
#'
#' @decription Esta funcion calcula estadisticas descriptivas de una variable numerica y tiene la opcion de hacer los calculos por grupo. De momento solo 1 grupo. Calcula la Media, mediana, desviacion estandar, iqr, Q1, Q2, N, Miss, N valido, minimo, maximo. Tambien un Shapiro. Siempre sacando los NA, que se reportan en los missing. Se pasa al porta papeles.
#'
#' @param var Variable o Vector de variables en String
#' @param grp Variable de agrupacion, puede ser un string, factor o numerico, si no esta no se usa
#' @param data Data frame 
#' 
#' @return Devuelve un data frame que ademas se pasa al portapepeles
#'
#' @examples
#' osumm("mpg", data = mtcars)
#' osumm("mpg", "am", data = mtcars)
#' osumm(c("mpg", "disp", "wt"), data = mtcars)
#' osumm(c("mpg", "disp", "wt", "gear"), "am", data = mtcars)
#'
osumm <- function(var = NULL, grp = NULL, data = NULL){
     require(dplyr)   #

     # Seleccionar data y chequea que sea numerica
     variables <- select(data, var)
     
     for (j in 1:dim(variables)[2]){
          if (is.numeric(variables[,j]) == FALSE){
               stop(paste("Variable", names(variables)[j], "no es numerica"))
          }
     }
     
     calculos <- function(data){
          # Acopio de estadisticas
          stat <- NULL
          for (j in 1:dim(data)[2]){
               media = mean(data[,j], na.rm = TRUE)
               mediana = median(data[,j], na.rm = TRUE)
          
               desv.est = sd(data[,j], na.rm = TRUE)
               p25 <- quantile(data[,j], probs = 0.25, na.rm = TRUE)
               p75 <- quantile(data[,j], probs = 0.75, na.rm = TRUE)
               iqr <- p75 - p25
          
               N = dim(data)[1]
               miss = sum(is.na(data[,j]))
               valid = dim(data)[1] - sum(is.na(data[,j]))
          
               valmin <- min(data[,j], na.rm = TRUE)
               valmax <- max(data[,j], na.rm = TRUE)
               
               shapi <- shapiro.test(data[,j])
               shapi <- shapi$p.value
          
               # Compila
               result <- data.frame(variable = names(data)[j], media, mediana, desv.est, iqr, p25, p75, 
                    min = valmin, max = valmax, N, miss, valid, shapiro = shapi)
               stat <- rbind(stat, result)
          }
          
          # Embellecimiento
          row.names(stat) <- NULL
          for (j in 2:13){stat[,j] <- round(stat[,j], 3)}
          return(stat)
     }
     

     # Hacer los calculos para sin grupo
     if (class(grp) == "NULL"){
          stat2 <- calculos(variables)
          write.table(stat2, "clipboard-128", sep="\t", row.names=FALSE)
          return(stat2)
     
     # Si no es null significa que algo hay
     } else {
          # Antecedentes
          vargrp <- data[, c(grp, var)]
          grupos <- unique(data[, grp])

          # Calculos sobre los grupos
          stat3 <- NULL
          for (gp in grupos){
         
               temp <- vargrp[vargrp[[1]] == gp, ]
               grpname <- names(temp)[1]
               temp[[1]] <- NULL
          
               # Arreglo del dim
               nvariables <- dim(temp)[2]
                
               stat.calc <- calculos(temp)
               stat.names <- cbind(variable = stat.calc[,1], data.frame(grupo = rep(gp, nvariables)))    
               stat.calc[[1]] <- NULL
               stat.calc <- cbind(stat.names, stat.calc)
               names(stat.calc)[2] <- grpname
               
               stat3 <- rbind(stat3, stat.calc)
               rm(stat.calc)
          }
          
          # Retorno
          write.table(stat3, "clipboard-128", sep="\t", row.names=FALSE)
          return(stat3)          
     }
}
