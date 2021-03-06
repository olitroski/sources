# setwd("C:/Users/Oliver/Desktop")
# rm(list=ls())

percentil <- function(datos, varpct, varcal, varfiltro, filtro)  {
## Funcion para hacer los calculos de percentiles necesita los datos en formato
## stata version 12 (saveold) e indicar los siguientes componentes
# Datos: Archivo stata
# varpct: Variable que se pasar� a percentiles
# varcal: Variable con la que se contrastara la variable en percentil
# varfiltro: es la variable de filtro, si no existe se debe crear y que solo contenga valor = 1
# filtro: es el valor de la variable filtro, si no existe la var anterior indicar el valor = 1
# Ejemplo de uso: percentil("auto.dta", "mpg", "price", "rep78", "3")

## Paquetes requeridos
require(foreign); require(dplyr); require(boot); require(xlsx)
# Datos de prueba
# datos <- read.dta("auto.dta")
# varpct <- "mpg"
# varcal <- "price"
# varfiltro <- "rep78"
# filtro <- 3


## Contraste de percentiles y pruebas ttest
file <- varpct # Nombre archivos a guardar
archivo <- paste(file, ".xlsx", sep="")
if (file.exists(archivo)) {file.remove(archivo)}


# Procesar base de datos
datos <- read.dta(datos)
data <- filter_(datos, paste(varfiltro, " == ", filtro))      
data <- select_(data, varpct=varpct, varcal=varcal) %>% arrange(varpct)  
data <- mutate(data, rank = rank(varpct, ties.method = "average"), rank = rank/dim(data)[1])      

# Guardar resultados
write.xlsx(data, archivo, sheetName="datos", row.names=FALSE, append=TRUE)


# Crear cuantiles
cut <- quantile(data$varpct, c(seq(0.1,0.9,0.1)))
pval <- NULL; m0 <- NULL; m1 <- NULL


# Crear vector de pvalores del ttest
for (i in cut) {
	grupo <- mutate(data, grupo=ifelse(varpct > i, 1, 0))
	ttest <- t.test(varcal ~ grupo, data = grupo)
	
	ttestp <- ttest$p.value
	pval <- c(pval, ttestp)
	pval <- round(pval,4)
	
	means <- ttest$estimate
	m0 <- c(m0, means[1])
	m1 <- c(m1, means[2])
}

# Guarda resultados
t <- data.frame(pct=c(1:9)*10,cortePCT=cut, mean0=m0, mean1=m1, pvalue=pval)
write.xlsx(t, archivo, sheetName="ttest", row.names=FALSE, append=TRUE)



## Test de aleatorizacion
contrast <- data.frame(i = seq(0.1, 0.8, by=0.1), j=seq(0.2, 0.9, by=0.1))
deltaboot <- data.frame(test=character(), p1=numeric(), p2=numeric(), 
                        delta=numeric(), boot=numeric(), error=numeric(), pcero=numeric())

# Funcion para el boot
sampQ <- function(var, indice) {
	diff <- quantile(var[indice], contrast[i,2]) - quantile(var[indice], contrast[i,1])
	return(diff)
}

# Selecciona la variable para hacer el bootstrap
var <- data$varcal
for (i in 1:8) {
	# Bootstrap
	boot <- boot(data=var, statistic=sampQ, R=10000)

	# Guardar percentiles ORIGINALES
	p1 <- quantile(var, contrast[i,1])
	p2 <- quantile(var, contrast[i,2])
	delta <- p2-p1
	
	# Guardat datos BOOTSTRAP y hace trim al 10% superior (por el sesgo)
	dataB <- as.data.frame(boot$t)
	dataB <- rename(dataB, boot = V1) %>% arrange(boot)  
	dataB <- slice(dataB, 1:9500) #%>% filter(boot>0)
	
	mu <- mean(dataB$boot, trim=0.1)
	sd <- sd(dataB$boot)
	pvalue <- pnorm(0, mean = mu, sd = sd)
	
	# Resultado en data frame
	centil <- paste("delta p", contrast[i,1]*100, " - p", contrast[i,2]*100, sep="")
	temp <- data.frame(test=centil, p1=p1, p2=p2, delta=delta, boot=mu, error=sd, pcero=pvalue)
	deltaboot <- rbind(deltaboot, temp)
}

# Guarda el archivo
write.xlsx(deltaboot, archivo, sheetName="bootstrap", row.names=FALSE, append=TRUE)
} # Fin de funcion









