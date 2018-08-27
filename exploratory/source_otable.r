# Función para calcular una tabla de contingencia o de 1 variable, se pasa a una lista
# con los elementos, freq - row - col - cell - x2, todo en un data frame, se hace un
# clipboard de alguna de ellas, hay que indicar no más.
# 1. Freq
# 2. row
# 3. col
# 4. cell
# 5. Chi2

# Data de prueba
# data <- mtcars
# rvar <- "gear"
# cvar <- "am"


otable <- function(rvar = NULL, cvar = NULL, data = NULL){
     require(dplyr)

     # Si es que sólo hay row var (1 variable)
     if (class(cvar) == "NULL"){
          # Tablas          
          df <- paste("table(data$", rvar, ")", sep = "")
          df <- eval(parse(text=df))
          
          freq <- data.frame(df)
          names(freq) <- c(rvar, "freq")
          margin <- data.frame("total", margin.table(df))
          names(margin) <- names(freq)
          freq <- rbind(freq, margin)
          
          pct <- data.frame(prop.table(df))
          names(pct) <- c(rvar, "pct")
          margin <- data.frame("total", margin.table(prop.table(df)))
          names(margin) <- names(pct)
          pct <- rbind(pct, margin)
          pct <- select(pct, pct)
          
          df <- cbind(freq, pct)
          df <- mutate(df, pct = round(pct, 3))
          
          # Resultado
          write.table(df, "clipboard-128", sep="\t", row.names=FALSE)
          return(df)
          
          
     # tablas de 2x2
     } else {
          # Primero las tablas
          df <- paste("table(data$", rvar, ", data$", cvar, ")", sep = "")
          df <- eval(parse(text=df))
          
          # Tabla de frecuencias
          freq <- data.frame(df)
          names(freq) <- c(rvar, "time", cvar)
          freq <- reshape(freq, timevar = "time", idvar = rvar, direction = "wide")
                    
          cols <- colSums(freq[,(2:dim(freq)[2])])   ## Ta weno pero no aguanta largos nombres
          cols <- data.frame(t(cols))
          cols <- cbind(data.frame("total"), cols)
          names(cols) <- names(freq)
          freq <- rbind(freq, cols)
          
          rows <- rowSums(freq[,(2:dim(freq)[2])])
          rows <- data.frame(total = rows)
          freq <- cbind(freq, rows)
          
          
          # Porcentaje por columnas
          pctcol <- prop.table(df, margin = 2)
          pctcol <- data.frame(pctcol)
          names(pctcol) <- c(rvar, "time", cvar)
          pctcol <- reshape(pctcol, timevar = "time", idvar = rvar, direction = "wide")
          
          rows <- paste("table(data$", rvar, ")", sep = "")
          rows <- eval(parse(text=rows))
          rows <- prop.table(rows)
          rows <- data.frame(rows)
          rows <- select(rows, total = Freq)
          pctcol <- cbind(pctcol, rows)
                              
          cols <- colSums(pctcol[,(2:dim(pctcol)[2])])          
          cols <- data.frame(t(cols))
          cols <- cbind(data.frame("total"), cols)
          names(cols) <- names(pctcol)
          pctcol <- rbind(pctcol, cols)
          
          col1 <- select(pctcol, 1)
          pctcol <- select(pctcol, -1)
          pctcol <- sapply(pctcol, function(x) round(x, 3))   
          pctcol <- cbind(col1, data.frame(pctcol))
          
          
          # Porcentaje por filas
          pctrow <- prop.table(df, margin = 1)
          pctrow <- data.frame(pctrow)
          names(pctrow) <- c(rvar, "time", cvar)
          pctrow <- reshape(pctrow, timevar = "time", idvar = rvar, direction= "wide")
          
          cols <- paste("table(data$", cvar, ")", sep = "")
          cols <- eval(parse(text=cols))
          cols <- prop.table(cols)
          cols <- data.frame(cols)
          cols <- select(cols, Freq)
          cols <- cbind(data.frame("total"), data.frame(t(cols)))
          names(cols) <- names(pctrow)
          pctrow <- rbind(pctrow, cols)
                    
          rows <- rowSums(pctrow[,(2:dim(pctrow)[2])])
          rows <- data.frame(total = rows)
          pctrow <- cbind(pctrow, rows)
          
          col1 <- select(pctrow, 1)
          pctrow <- select(pctrow, -1)
          pctrow <- sapply(pctrow, function(x) round(x, 3))
          pctrow <- cbind(col1, data.frame(pctrow))
          pctrow <- mutate(pctrow, total = sprintf("%.3f", total))
          row.names(pctrow) <- NULL
          
          
          # Porcentaje por celdas
          pctcell <- prop.table(df)
          pctcell <- data.frame(pctcell)
          names(pctcell) <- c(rvar, "time", cvar)          
          pctcell <- reshape(pctcell, timevar = "time", idvar = rvar, direction = "wide")
          
          rows <- paste("table(data$", rvar, ")", sep = "")
          rows <- eval(parse(text=rows))
          rows <- prop.table(rows)
          rows <- data.frame(rows)
          rows <- select(rows, total = Freq)
          pctcell <- cbind(pctcell, rows)

          cols <- paste("table(data$", cvar, ")", sep = "")
          cols <- eval(parse(text=cols))
          cols <- prop.table(cols)
          cols <- data.frame(cols)
          cols <- select(cols, Freq)
          cols <- cbind(data.frame("total"), data.frame(t(cols)))
          cols <- mutate(cols, total = 1)
          names(cols) <- names(pctcell)
          pctcell <- rbind(pctcell, cols)          
          
          col1 <- select(pctcell, 1)
          pctcell <- select(pctcell, -1)
          pctcell <- sapply(pctcell, function(x) round(x, 3))
          pctcell <- cbind(col1, data.frame(pctcell))
          row.names(pctcell) <- NULL
          
          
          # Test de independencia
#           F <- fisher.test(df)
#           F <- F$p.value
          
          chi <- summary(df)
          chi <- data.frame(table = paste(rvar, "*", cvar),  
               ChiSq = sprintf("%.3f", chi$p.value))
#              ChiSq = sprintf("%.3f", chi$p.value), Exact = sprintf("%.3f", F))
         
          
          # Resultado
          lista <- list(freq=freq, row=pctrow, col=pctcol, cell=pctcell, pvalue = chi)
          write.table(lista, "clipboard-128", sep="\t", row.names=FALSE)
          return(lista)
     }

}

## Data de prueba
# rvar <- "gear"
# cvar <- "am"     
# otable(rvar = "cyl", data = mtcars)
# otable(rvar = "cyl", cvar = "am", data = mtcars)
# tab <- otable(rvar = "cyl", cvar = "am", data = mtcars)
# tab$freq


