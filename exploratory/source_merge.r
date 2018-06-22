# ---- Script para chequear un merge ----
# Cuando se hace un merge el R pone las variables del using al final, si el "id" esta 
# en la columna 1 se puede anotar desde:hasta va el master y el using así se puede hacer
# una variable para reportar el resultado.

# Requiere:
#    merge(all = TRUE)
#    id está en la columna 1
#    Los rangos son numericos 2:7, asi tal cual "numero:numero"
#    keep = TRUE arroja una lista con las 3 categorías de match.
#
#    Como es lata que a cada rato pregunte por las columnas agrego "cols", en donde se 
#    ingresan como texto los rangos de columnas del master ya que con eso basta si se 
#    cumplen las reglas



merge.table <- function(df = NULL, cols = "no", keep = FALSE){
     # df <- eyetrack.stats
     require(dplyr)
     
     ## Checa si se declararon las columnas
     if (cols == "no"){   
          # Captura datos
          print(data.frame(variables = names(df)))
          
          master <- readline("Rango columnas del master (sin la key): ")
          using <- readline("Rango columnas del using (sin la key): ")
          merged <- paste("2:", dim(df)[2], sep="")
          
          # Informar
          varnames <- names(df)
          
          master.ini <- eval(parse(text=master))[1]
          master.fin <- max(eval(parse(text=master)))
          cat("\nMaster", master, "- Inicio:", varnames[master.ini], 
               "\n               Final:", varnames[master.fin], "\n")
          
          using.ini <- eval(parse(text=using))[1]
          using.fin <- max(eval(parse(text=using)))
          cat("\nUsing", using, "- Inicio:", varnames[using.ini], 
               "\n               Final:", varnames[using.fin], "\n\n")
          
     } else {
          # Captura de columnas
          master <- cols
          using <- paste(max(eval(parse(text=master)))+1, ":", dim(df)[2], sep = "")
          merged <- paste("2:", dim(df)[2], sep="")
 
           # Informar
          varnames <- names(df)
          
          master.ini <- eval(parse(text=master))[1]
          master.fin <- max(eval(parse(text=master)))
          cat("\nMaster", master, "- Inicio:", varnames[master.ini], 
               "\n               Final:", varnames[master.fin], "\n")
          
          using.ini <- eval(parse(text=using))[1]
          using.fin <- max(eval(parse(text=using)))
          cat("\nUsing", using, "- Inicio:", varnames[using.ini], 
               "\n               Final:", varnames[using.fin], "\n\n")
     }
     

     # Agregar las variables
     datos <- df
     datos$na_master <- rowSums(is.na(datos[, eval(parse(text=master))]))
     datos$na_using  <- rowSums(is.na(datos[, eval(parse(text=using))]))
     datos$na_count  <- rowSums(is.na(datos[, eval(parse(text=merged))]))
     
     
     # Calcular merge
     # Logica. Si el master es completo con NA, no le cruzó nada del using, y así para todo
     # chequeando que no hayan files que tengan todo en blanco.
     datos <- mutate(datos, merge = ifelse(na_master == length(eval(parse(text=master))), 1, 
          ifelse(na_using == length(eval(parse(text=using))), 2, 
          ifelse(na_count == length(eval(parse(text=merged))), 3, 4))))
          
     datos$merge2 <- factor(datos$merge, levels = c(1,2,3,4), 
          labels = c("Only in using", "Only in master", "-No data, check-", "Matched observations"))
          
     
     # Reportería en pantalla
     report <- data.frame(table(datos$merge2))
     names(report) <- c("StatusMerge", "Count")
     report <- rbind(report, data.frame(StatusMerge = "--- Total ---", Count = sum(report$Count)))
     print(report, row.names = FALSE)
          

     ## Return del data frame
     datos <- select(datos, -na_master, -na_using, -na_count, -merge) %>% rename(merge = merge2)
     
     u <- filter(datos, merge == "Only in using")
     m <- filter(datos, merge == "Only in master")
     match <- filter(datos, merge == "Matched observations")

     if (keep == TRUE){
          return(list(using = u, master = m, match = match, report = report))
     }
}