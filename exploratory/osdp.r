#' @title Olito's Standard Deviation poblacional
#' 
#' @description SD poblacional, no divide por N-1, hace na.rm=TRUE
#' 
#' @param un vector numerico
#' 
#' @return un atomico
#' 
#' @examples
#' 

osdp <- function(vec) {
	sd <- sqrt(sum((vec-mean(vec, na.rm=TRUE))^2, na.rm=TRUE)/length(vec))
	return(sd)
}