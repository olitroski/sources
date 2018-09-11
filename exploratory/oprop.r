#' @title Olitos's Proportion test, estadí­sticas descriptivas 
#'
#' @decription Esta función hace los calculos para el Z test de proporciones, solo eso, pero de forma expedita y sencilla. Es una calculdora, es decir se necesitan los parámetros no los datos.
#'
#' @param pct Vector con dos porcentajes
#' @param n Vector con los N asociados al porcentaje
#' 
#' @return Devuelve un data frame que además se pasa al portapepeles con los resultados
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


oprop <- function(pct = NULL, n = NULL){
     # Initial data
     p <- pct[1]
     q <- pct[2]
     np <- n[1]
     nq <- n[2]
     
     # Data for the formula
     dif <- p - q
     pbar <- (p*np + q*nq)/(np + nq)
     qbar <- 1 - pbar
     
     # Formula
     Z <- dif / sqrt( (pbar*qbar)/np  +  (pbar*qbar)/nq )
     upp <- round(1 - pnorm(Z), 3)
     low <- round(pnorm(Z), 3)
     
     two <- round((1 - pnorm(Z))*2, 3)
     one <- if (upp < low) {upp} else {low}
     
     # Compilar resultados
     tabla <- data.frame(p = sprintf("%.2f", round(p, 3)), q = sprintf("%.2f", round(q, 3)), 
          diff = round(dif, 3), Np = np, Nq = nq, Z = round(Z, 3), two.t = two, one.t = one)
     
     # Output
     write.table(tabla, "clipboard-128", sep="\t", row.names=FALSE)
     return(tabla)
}

