#' @title Olitos's clipboard
#'
#' @decription Esta función simplemente copia un objeto al clipboard y lo pasa a la consola, si se puede poner en la consola se copia.
#'
#' @param x El objeto en cuestión
#' 
#' @return Devuelve un print en la consola y al portapapeles
#'
#' @examples
#' data(mtcars)
#' clip(mtcars)
#'

oclip <- function(x){
     write.table(x, "clipboard-128", sep="\t", row.names=FALSE)
     return(x)
}
