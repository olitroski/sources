#' @title Olito's ttest for dependent variables
#' 
#' @description refrito del stata
#' 
#' @param muchos 
#' 
#' @return un df creo
#' 
#' @examples
#' 

# ------------------------------------------------------------------------------------ #
# ---- Source de un ttest pareado para tener resultado directo a data.frame ---------- #
# ------------------------------------------------------------------------------------ #
# rm(list=ls())
# data(mtcars)
# data <- mtcars
# set1 <- c("disp", "wt", "qsec", "carb")
# set2 <- c("hp", "drat", "mpg", "gear")
# i <- 1
# save <- "test"
# version <- 1
# 
## Funcion ttest pareado
ottest.pair <- function(set1 = NULL, set2 = NULL, data = NULL, version = 1, save = "file"){
	# Paquetes
	require(dplyr); require(openxlsx)
	
	# Checar que los pares funcionan
	if (length(set1) != length(set2)){
		break()
	}
	
	# Crear el set de datos
	pairs <- data.frame(set1, set2, stringsAsFactors=FALSE)
	pairs$num <- row.names(pairs)
	finaldf <- NULL
	
	
	# Loopeo entre los pares
	for (i in 1:dim(pairs)[1]){
		
		# Subset de datos
		datos <- data[, c(pairs[i,"set1"], pairs[i,"set2"])]	
		datos[is.na(datos[[1]])==FALSE,]
		datos[is.na(datos[[2]])==FALSE,]
		vname1 <- names(datos)[1]
		vname2 <- names(datos)[2]
		
		# ttest 
		t <- t.test(datos[[1]], datos[[2]], alternative = "less", paired = TRUE)
		pval.less <- t$p.value
		t <- t.test(datos[[1]], datos[[2]], alternative = "greater", paired = TRUE)
		pval.grea <- t$p.value
		
		if (pval.less < pval.grea) {
			pvalue <- pval.less
		} else {
			pvalue <- pval.grea
		}
		
		t <- data.frame(tvalue=t$statistic, pvalue = round(pvalue, 3))
		t <- mutate(t, sig = ifelse(pvalue <=0.05, "***", ""))
		
		
		# Estadisticos + delta
		datos$delta <- datos[[2]] - datos[[1]]
		stat <- data.frame(set1.mean = mean(datos[[1]]), set2.mean = mean(datos[[2]]), 
			delta.mean = mean(datos[[3]]), delta.sd = sd(datos[[3]]), npairs = dim(datos)[1])
		
		
		# Encabezado variables y arrejunte
		vname <- data.frame(contrast = paste(vname1, "->", vname2))
		tresult <- cbind(vname, stat, t)
		finaldf <- rbind(finaldf, tresult)
	}
		
		
	# Tipos de resultado   <<<<<<<<<<<<<<<<<< hay que arreglar esto >>>>>>>>>>>>>>>>>>>>>
     if (version == 1){       # Version base
          if (save == "file"){
			write.table(finaldf, "clipboard-128", sep="\t", row.names=FALSE)
               return(finaldf)
          } else {
               filename <- paste(save, ".xlsx", sep="")
               excel <- createWorkbook()
               addWorksheet(excel, "ttest")
               writeData(excel, "ttest", finaldf)
               freezePane(excel, "ttest", firstRow = TRUE)
               setColWidths(excel, "ttest", cols=c(1:9), widths="auto")
               saveWorkbook(excel, filename, overwrite=TRUE)
               write.table(finaldf, "clipboard-128", sep="\t", row.names=FALSE)
               return(finaldf)
          }
		
		
	} else {
		break()
	}
          
}


# 
# set1 <- c("disp", "wt", "qsec", "carb")
# set2 <- c("hp", "drat", "mpg", "gear")
# data(mtcars)
# ttest.pair(set1, set2, data = mtcars, save = "ttest")
# 





































