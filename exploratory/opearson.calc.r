#' @title Olitos's Pearson calculator
#' 
#' @description Calcula correlación de pearson a partir de 2 vectores, agrega un mensaje si se quiere
#' 
#' @param x Vector 1
#' @param y Vector 
#' @param msg FALSE by default, por si se quiere algún mensaje sobre missing data
#' 
#' @return Data.frame con los resultados
#' 
#' @examples
#' data(mtcars)
#' x <- mtcars$mpg
#' y <- mtcars$wt
#' opearson(x, y, msg = TRUE)


# Source que calcula correlacion de pearson con ajuste de missing por pairwise
# arroja un data frame con todo y avisa de missing
# Solo acepta dos vectores
opearson <- function(x, y, msg=FALSE){
	# para avisar si hay NAs
	nax <- sum(is.na(x))
	nay <- sum(is.na(y))
	
	# Sacar los NA que existan
	df <- data.frame(x, y)
	pairs <- dim(df)[1]
	df$nas <- is.na(df$x) | is.na(df$y)
	df <- df[df$nas == FALSE, ]
	x <- df$x; y <- df$y
	
	# Mensaje si hay missing
	if (nax > 0 | nay > 0) {
		if (msg==TRUE){
			cat("Missing data found: X=", nax, "  ", "Y=", nay, 
			"  Original=", pairs, "  Complete=", length(x), "\n", sep="")
		}
	}
	
	# Calculos de la correlacion y limite de digitos (por un drama raro que paso)
	r <- (sum(x*y) - sum(x)*sum(y)/length(x))  /  
		sqrt((sum(x^2)-sum(x)^2/length(x)) * (sum(y^2)-sum(y)^2/length(y)))
	r <- round(r, 10)
	
	# Si todo un vector tiene el mismo valore el r = NaN, por lo cual hay que hacer un excepcion
	if (is.na(r)==TRUE){
		if (msg==TRUE){
			cat("One vector is a constant, can't calculate r\n")
		}
		corr <- data.frame(r = NA, n = length(x), t = NA, oneT = NA, twoT = NA)
			
	# Si hay menos de 3 pares no ejecutar y avisar y arrojar un df con NA
	} else if (length(x) < 3){
		if (msg==TRUE){
			cat("Num. of pairs < 3, P-Value = NA\n")
		}
		corr <- data.frame(r = r, n = length(x), t = NA, oneT = NA, twoT = NA)
	
	# Calculos si hay datos suficientes
	} else {
		# Probabilidades
		t <- r/sqrt((1-(r^2))/(length(x)-2))
		if (t < 0){t <- t*-1}
		df <- length(x)-2
		
		cola1 <- 1-pt(t, df)
		cola2 <- (1-pt(t, df))*2

		# Resultado
		corr <- data.frame(r = r, n = length(x), t = t, oneT = cola1, twoT = cola2)
	}
		
	return(corr)
}
