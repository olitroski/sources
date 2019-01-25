# Para hacer el source, mientras no haga el package
# se corre completo y genera el source que uso para cargar desde github
setwd("D:/OneDrive/GitHub/sources/exploratory")

url <- "https://raw.githubusercontent.com/olitroski/sources/master/exploratory/"

archivos <- dir()
archivos <- archivos[grep(".[rR]", archivos)]
archivos <- archivos[!(grepl("make_source.r", archivos))]
archivos <- archivos[!(grepl("sources.r", archivos))]
archivos <- paste("source('", url, archivos, "')", sep = "")

paquetes <- c("stringr", "dplyr", "openxlsx", "foreign", "ggplot2", "lubridate")
paquetes <- paste("library(", paquetes, ")", sep = "")

cargar <- c(archivos, paquetes)

writeLines(cargar, "sources.r")

