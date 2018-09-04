#' @title Olitos's Summary, estadísticas descriptivas 
#'
#' @decription Esta función calcula estadísticas descriptivas de una variable numérica y tiene la opción de hacer los cálculos por grupo. De momento solo 1 grupo. Calcula la Media, mediana, desviación estándar, iqr, Q1, Q2, N, Miss, N valido, minimo, maximo. También un Shapiro. Siempre sacando los NA, que se reportan en los missing. Se pasa al porta papeles.
#'
#' @param var Variable o Vector de variables en String
#' @param grp Variable de agrupación, puede ser un string, factor o numérico, si no está no se usa
#' @param data Data frame 
#' 
#' @return Devuelve un data frame que además se pasa al portapepeles
#'
#' @examples
#' osumm("mpg", data = mtcars)
#' osumm("mpg", "am", data = mtcars)
#' osumm(c("mpg", "disp", "wt"), data = mtcars)
#' osumm(c("mpg", "disp", "wt"), "am", data = mtcars)
#'
osumm <- function(var = NULL, grp = NULL, data = NULL){
     require(dplyr)

     # Seleccionar data y chequea que sea numérica
     variables <- select(data, var)
     
     for (j in 1:dim(variables)[2]){
          if (is.numeric(data[,j]) == FALSE){
               stop(paste("Variable", names(variables)[j], "no es numerica"))
          }
     }
     
     calculos <- function(data){
          # Acopio de estadisticas
          stat <- NULL
          for (j in 1:dim(variables)[2]){
               media = mean(variables[,j], na.rm = TRUE)
               mediana = median(variables[,j], na.rm = TRUE)
          
               desv.est = sd(variables[,j], na.rm = TRUE)
               p25 <- quantile(variables[,j], probs = 0.25, na.rm = TRUE)
               p75 <- quantile(variables[,j], probs = 0.75, na.rm = TRUE)
               iqr <- p75 - p25
          
               N = dim(variables)[1]
               miss = sum(is.na(variables[,j]))
               valid = dim(data)[1] - sum(is.na(variables[,j]))
          
               valmin <- min(variables[,j], na.rm = TRUE)
               valmax <- max(variables[,j], na.rm = TRUE)
               
               shapi <- shapiro.test(variables[,j])
               shapi <- shapi$p.value
          
               # Compila
               result <- data.frame(variable = names(variables)[j], media, mediana, desv.est, iqr, p25, p75, 
                    min = valmin, max = valmax, N, miss, valid, shapiro = shapi)
               stat <- rbind(stat, result)
          }
          
          # Embellecimiento
          row.names(stat) <- NULL
          for (j in 2:13){stat[,j] <- round(stat[,j], 3)}
          return(stat)
     }
     

     # Hacer los cálculos para sin grupo
     if (class(grp) == "NULL"){
          stat <- calculos(variables)
          write.table(stat, "clipboard-128", sep="\t", row.names=FALSE)
          return(stat)
     
     # Si no es null significa que algo hay
     } else {
          # Antecedentes
          vargrp <- data[, c(grp, var)]
          grupos <- unique(data[, grp])
     
          # Calculos sobre los grupos
          stat <- NULL
          for (gp in grupos){
               temp <- vargrp[vargrp[[1]] == gp, ]
               grpname <- names(temp)[1]
               temp[[1]] <- NULL
          
               stat.calc <- calculos(temp)
               stat.names <- cbind(variable = stat.calc[,1], data.frame(grupo = rep(gp, 3)))
               stat.calc[[1]] <- NULL
               stat.calc <- cbind(stat.names, stat.calc)
               names(stat.calc)[2] <- grpname
               
               stat <- rbind(stat, stat.calc)
          }
          
          # Retorno
          write.table(stat, "clipboard-128", sep="\t", row.names=FALSE)
          return(stat)          
          
     }

     
}



