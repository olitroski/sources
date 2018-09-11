#' @title Olitos's Proportion test for variables, estadí­sticas descriptivas 
#'
#' @decription Esta función hace los calculos para el Z test de proporciones, solo eso, pero de forma expedita y sencilla. Para usar cuando se tiene un data.frame, con porcentajes (en wide). Usa la función "oprop"
#'
#' @param pct1 Vector o Variable con porcentajes 
#' @param pct2 Vector o Variable con porcentajes 
#' @param n1 Vector o Variable con los N asociados
#' @param n2 Vector o Variable con los N asociados
#' @data Data frame
#' @append Logical, determina si el resultado se anexa o no a la base original
#' 
#' @return Devuelve un data frame con el Z y el p-valor
#'
#' @examples
#' data <- otable("am", data = mtcars)
#' prop <- data$pct[1:2]
#' nprop <- data$freq[1:2]
#' oprop(pct = prop, n = nprop)
#'
#' p <- 18/24
#' q <- 10/25
#' np <- 24
#' nq <- 25
#' oprop(pct = c(p, q), n = c(np, nq))

#' pctdata <- data.frame(pct1 = c(0.39, 0.25, 0.30), 
#'                       pct2 = c(0.18, 0.20, 0.37), 
#'                         n1 = c(  45,   37,   98), 
#'                         n2 = c(  41,   37,   84))
#'
#' pctdata
#' oprop.df("pct1", "pct2", "n1", "n2", data = pctdata)
#' oprop.df("pct1", "pct2", "n1", "n2", data = pctdata, append = TRUE)
#'

oprop.df <- function(pct1 = NULL, pct2 = NULL, n1 = NULL, n2 = NULL, data = NULL, append = FALSE){
     # Seleccionar la data
     temp <- select(data, pct1, pct2, n1, n2)
     
     # Asignar a objetos como en la otra función
     p <- data$pct1
     q <- data$pct2
     np <- data$n1
     nq <- data$n2
     
     # Data for the formula
     dif <- p - q
     pbar <- (p*np + q*nq)/(np + nq)
     qbar <- 1 - pbar
     
     # Formula
     Z <- dif / sqrt( (pbar*qbar)/np  +  (pbar*qbar)/nq )

     upp <- round(1 - pnorm(Z), 3)
     low <- round(pnorm(Z), 3)
     two <- round((1 - pnorm(Z))*2, 3)

     one <- data.frame(upp, low)
     one <- mutate(one, one = ifelse(low < upp, low, upp))
     one <- one$one

     # Resultado
     temp <- data.frame(Z = round(Z, 3), two.t = two, one.t = one)
     
     if (append == FALSE){
          write.table(temp, "clipboard-128", sep="\t", row.names=FALSE)
          return(temp)
     
     } else {
          temp <- cbind(data, temp)
          write.table(temp, "clipboard-128", sep="\t", row.names=FALSE)
          return(temp)
     }

}