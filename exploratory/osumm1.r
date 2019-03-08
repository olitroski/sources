#' @title Olitos's summary para variables completas en 2 variantes
#' 
#' @description eso, solo estadsiticas muy simples para variable completa
#' 
#' @param set de variables en vector char
#' 
#' @return un df
#' 
#' @examples
#' 

## Source para descriptivos generales simples de publicacion
# data(mtcars)
# data <- mtcars
# set <- names(mtcars)
# var <- "mpg"

osumm1 <- function(set = NULL, data = NULL, version = 1){
	dfresult <- NULL
	R <- 1     # Redondeo
	require(dplyr)
		
	# loopeo sobre cada variable
	for (var in set){
		datos <- data.frame(variable = data[[var]])
		datos <- dplyr::summarize(datos, mean = mean(variable, na.rm=TRUE), sd = sd(variable, na.rm=TRUE))
		datos <- mutate(datos, n = sum(!is.na(data[[var]])))
		datos <- cbind(data.frame(variable = var), datos)
		dfresult <- rbind(dfresult, datos)
	}
	
	
	# Version 1 raw
	if (version == 1){
		write.table(dfresult, "clipboard-128", sep="\t", row.names=FALSE)
		return(dfresult)
		
	# Version 2 combinado
	} else if (version == 2){
		namevar <- paste("MeanSd.N=",  max(dfresult[4]), sep = "")
		stats <- data.frame(paste(round(dfresult[[2]], R), rawToChar(as.raw(177)), round(dfresult[[3]], R)))
		names(stats) <- namevar
		stats <- cbind(dfresult[1], stats)
		write.table(stats, "clipboard-128", sep="\t", row.names=FALSE)
		return(stats)
	}
}


# summ(set, mtcars, version = 2)
