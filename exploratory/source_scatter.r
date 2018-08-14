# Función para hacer un grafico en ggplot2 un scatter con correlación y lm
# sin facets, sólo el gráfico 2 variables y una tercera para colorear, pero
# que no es parte lm

# Test pre
# data <- mtcars
# yvar <- "mpg"
# xvar <- "wt"
# color <- "cyl"
# outl <- TRUE

scatter <- function(xvar = NULL, yvar = NULL, data = NULL, color = NULL, outl = FALSE){
     source("https://raw.githubusercontent.com/olitroski/sources/master/exploratory/source_pwcorr.r")
     require(ggplot2)
     require(dplyr)
     
     # Para la ayuda
     if (class(xvar) == "NULL"){
          cat("xvar = NULL, yvar = NULL, data = NULL, color = NULL, outl = FALSE \n")
          stop("No olvidar -> Variables en character")
     }
     

     # Selección de datos
     if (class(color) == "NULL"){
          gdata <- select(data, varx = xvar, vary = yvar)
     } else {
          gdata <- select(data, varx = xvar, vary = yvar, varc = color)
     }
     
     
     # Remoción de outliers SD sobre 3
     if (outl == TRUE){
          gdata <- mutate(gdata, zx = scale(varx)[,1], zy = scale(vary)[,1])
          gdata <- filter(gdata, zx < 3, zy < 3) 
          gdata <- select(gdata, -zx, -zy)
     } 
     
     
     # Pairwise data, solo datos validos.
     if (class(color) == "NULL"){
          gdata <- mutate(gdata, na = is.na(gdata$varx) + is.na(gdata$vary))
          gdata <- filter(gdata, na == 0) %>% select(-na)
          
     } else if (class(color) != "NULL"){
          gdata <- mutate(gdata, na = is.na(gdata$varx) + is.na(gdata$vary) + is.na(gdata$varc))
          gdata <- filter(gdata, na == 0) %>% select(-na)

     } else {
          stop("Algo terrible ha sucedido en la remoción de NA")
          
     }
     
     
     # Correlacion y regresión
     N <- dim(gdata)[1]
     r <- pwcorr(set1="varx", set2="vary", data = gdata, set = 1)
     r <- paste("Pearson: n=", N, " r=", r[1,2], " p=", sprintf("%.3f",r[1,3]), sep="")
          
     fit <- lm(varx ~ vary, data = gdata)
     fit <- summary(fit)
     fit <- as.data.frame(fit$coefficients); names(fit)[4] <- "pvalor"
     fit <- mutate(fit, Estimate = round(Estimate, 3), pvalor = round(pvalor, 3))
     fit <- paste("Regress: ", yvar, " = ", fit[2,1], " + ", fit[1,1], "*", xvar, " (", sprintf("%.3f",fit[1,4]), ")", sep="")
     
     
     # Plot
     if (class(color) == "NULL"){
          g <- ggplot(data = gdata, aes(x = varx, y = vary))
          g <- g + geom_smooth(method='lm') + geom_point()
          g <- g + xlab(xvar) + ylab(yvar) + ggtitle(paste(r, "\n", fit, sep=""))
     
     } else {
          g <- ggplot(data = gdata, aes(x = varx, y = vary))
          g <- g + geom_smooth(method='lm') + geom_point(aes(colour = factor(varc)))
          g <- g + xlab(xvar) + ylab(yvar) + ggtitle(paste(r, "\n", fit, sep="")) + labs(colour = color)
     }
     
     
     # Exportado
     return(g)
}

# xvar = NULL, yvar = NULL, data = NULL, color = NULL, outl = FALSE
# data <- mtcars
# scatter(xvar = "mpg", yvar = "wt", data = mtcars, color = "cyl")
