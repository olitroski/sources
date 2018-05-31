# Funcion para la desviacion estandar poblacional
sdp <- function(vec) {
	sqrt(sum((vec-mean(vec, na.rm=TRUE))^2, na.rm=TRUE)/length(vec))
}