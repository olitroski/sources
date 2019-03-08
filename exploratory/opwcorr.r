#' @title pwcorr Correlations between (onevar & set) and (set & set)
#'
#' @description Calculate correlations by transforming into a matrix and the use of hmisc package the rest is data management. Calculates pearson and spearman. Wrapper del Stata.
#' 
#' @param set1 Char vector de variables (o una sola)
#' @param set2 Lo mismo que el 1
#' @param data Un data frame
#' @param type Tipo de correlaci�n que se quiere ="pearson" o "spearman"
#' @param save Guarda el resultado en un excel en el wd
#' @param set 1 = es una variable contra 1 o varias, 2 = pares de variables en dos char vectors
#' 
#' @examples
#' # Function Test Data
#' data <- mtcars
#' var_unique <- "mpg"
#' var_set <- c("mpg", "cyl", "qsec")
#' var_multiple <- c("drat", "hp", "wt")
#' opwcorr("mpg", c("drat", "hp", "wt"), data = mtcars, set = 1)
#' opwcorr(c("mpg", "cyl", "qsec"), c("drat", "hp", "wt"), data = mtcars, set = 2)
#'


opwcorr <- function(set1 = NULL, set2 = NULL, data = NULL, type = "pearson", save = "file", set = NULL){ 
     # Packages
     require(dplyr); require(Hmisc)
		
	## Selecciona si 1var vs Multiple o 2 sets
	if (set == 1){
	     # Calculos 1 set:  1var - Many
          sp <- rcorr(as.matrix(data[c(set1, set2)]), type = type)	
          sp <- data.frame(variable = row.names(sp$r), r=sp$r[,1], pvalue=sp$P[,1], n=sp$n[,1])
          sp <- slice(sp, -1)
          sp <- mutate(sp, r = round(r, 3), pvalue = round(pvalue, 3))
          sp <- mutate(sp, sig = ifelse(pvalue <= 0.05, "***", ""))
          sp$variable <- paste(set1, "-", sp[[1]])
	
	} else if (set == 2){
          # Calculos 2 sets, one to one var
          if (length(set1) != length(set2)){
               cat("Error: Sets are different in length \n")
               break()
          }
          
          # Crear sets y compilar correlaciones
          sets <- data.frame(set1, set2, stringsAsFactors = FALSE)
          sp.set <- NULL
          for (i in 1:dim(sets)[1]){
               sp <- rcorr(as.matrix(data[c(sets[i,1], sets[i,2])]), type = type)
               sp <- data.frame(variables = row.names(sp$r), r=sp$r[,1], pvalue=sp$P[,1], n=sp$n[,1])              
               sp <- slice(sp, -1)               
               sp$variables <- paste(sets[i,1], "-", sets[i,2])
               sp.set <- rbind(sp.set, sp)
          }
          sp <- sp.set
          sp <- mutate(sp, sig = ifelse(pvalue <= 0.05, "***", ""))
	}
	# Redondeo
	sp <- mutate(sp, r = round(r, 3), pvalue = round(pvalue, 3))
	
	
	## Remosado de nombres y variables
	# Nombre del tipo de correlacion
	if (type == "pearson"){
		stat <- paste("r.", type, sep="")
	} else if (type == "spearman"){
		stat <- paste("rho.", type, sep="")
	}
	
     # P-valor con N
	pvalue <- paste("pvalue.N=", max(sp$n), sep="")
	
	# nombres y variables
	names(sp) <- c("variables", stat, pvalue, "n", "sig")
	sp <- select(sp, -n)
	
	
	## Guardado (siempre al clipboard)
	if (save == "file"){
		write.table(sp, "clipboard-128", sep="\t", row.names=FALSE)
		return(sp)
	} else {
          filename <- paste(save, ".xlsx", sep="")
          excel <- createWorkbook()
          addWorksheet(excel, type)
          writeData(excel, type, sp)
          freezePane(excel, type, firstRow = TRUE)
          setColWidths(excel, type, cols=c(1:7), widths="auto")
          saveWorkbook(excel, filename, overwrite=TRUE)
          write.table(sp, "clipboard-128", sep="\t", row.names=FALSE)
		return(sp)
	}
}


# Test funcion
     # setwd("C:/Users/Oliver/Desktop")
     # data(mtcars)
     # set1 <- "mpg"                          
     # set2 <- c("drat", "hp", "wt")
     # 
     # pwcorr(set1, set2, data = mtcars, type = "spearman")
     # pwcorr(set1, set2, data = mtcars, type = "pearson")
     # pwcorr(set1, set2, data = mtcars, type = "spearman", save = "sp_unique")
     # pwcorr(set1, set2, data = mtcars, type = "pearson", save = "pe_unique")
     # 
     # set1 <- c("mpg", "cyl", "qsec")
     # 
     # pwcorr(set1, set2, data = mtcars, type = "spearman", set=2)
     # pwcorr(set1, set2, data = mtcars, type = "pearson", set=2)
     # pwcorr(set1, set2, data = mtcars, type = "spearman", set=2, save = "sp_unique2")
     # pwcorr(set1, set2, data = mtcars, type = "pearson", set=2, save = "pe_unique2")