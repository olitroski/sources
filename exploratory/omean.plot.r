# data <- endo
# numvar <- "BMIz"
# cat1 <- "sexo"
# cat2 <- "nutri2"

    
# grafico de medias
omean.plot <- function(numvar = NULL, cat1 = NULL, cat2 = NULL, data = NULL, stat = "sd"){
    library(ggplot2); library(dplyr)
    
    # Checar numvar
    test <- deparse(substitute(numvar))
    if (test == "NULL"){
        error("Debe ingresar una variable")
    } else if (test %in% names(data) == FALSE){
        print(test); error("Variable no está en base de datos")
    } else if (class(data[[test]]) != "numeric"){
        print(test); error("la variable debe ser numerica")
    }
    
    # Checar que exista cat1 al menos
    test <- deparse(substitute(cat1))
    if (test == "NULL"){
        error("Debe ingresar Categoria 1")
    } else if (test %in% names(data) == FALSE){
        print(test); error("Categoria 1 no esta en la base de datos")
    }
    
    # De haber cat2 checar que exista en la base de datos y que exista cat1
    test <- deparse(substitute(cat2))    
    if (test != "NULL"){
        if (test %in% names(data) == FALSE){
            print(test); error("Categoria 2 no esta en la base de datos")
        }
    }
    
    # Capturar bien las variables para hacer todo GDATA
    var <- deparse(substitute(numvar))    
    c1 <- deparse(substitute(cat1))    
    c2 <- deparse(substitute(cat2))    
    
    ## Gráfico cuando solo hay CAT 1
    if (c2 == "NULL"){
        # Crear el gdata
        gdata <- select(data, var, c1)
        names(gdata) <- c("meanvar", "cat1var")
        gdata <- group_by(gdata, cat1var)
        gdata <- summarize(gdata, media = mean(meanvar, na.rm = TRUE), 
                         sd = sd(meanvar, na.rm = TRUE),
                         sem = sd/sqrt(n()),
                         n = n())
        gdata <- data.frame(gdata)
        
        # Crear los limite dependiendo si es sd, sem o ci (ahora solo sd)
        if (stat == "sd"){
            gdata <- mutate(gdata, li = media - sd, ls = media + sd)
        } else if (stat == "sem") {
            gdata <- mutate(gdata, li = media - sem, ls = media + sem)
        }

        # Craer el caption y los limites de Y
        nlab <- paste(gdata$cat1var,"=", gdata$n, sep = "")
        nlab <- paste(nlab, collapse = " ")
        
        # Crear el gráfico
        g <- ggplot(data = gdata, aes(x=cat1var, y=media))
        g <- g + geom_bar(position=position_dodge(0.9), stat="identity", color = "black", fill = "grey") 
        g <- g + geom_errorbar(position=position_dodge(0.9), aes(ymin=media, ymax=ls), width=0.25)
        g <- g + labs(x = c1, y = var, caption = nlab, title = "Gráfico de medias")
        g <- g + theme(
            panel.border = element_rect(colour = "black", fill = NA, size = 0.25),
            panel.background = element_rect(fill = "grey95"),
            panel.grid.major.x = element_line(colour = "grey95"),
            panel.grid.major.y = element_line(colour = "grey"),
            panel.grid.minor = element_line(colour = "grey95"))
        return(g)
        
    
    ## Ahora si hay 2 categorías
    } else if (c2 != "NULL"){
        # Crear el gdata
        gdata <- select(data, var, c1, c2)
        names(gdata) <- c("meanvar", "cat1var", "cat2var")
        gdata <- group_by(gdata, cat1var, cat2var)
        gdata <- summarize(gdata, media = mean(meanvar, na.rm = TRUE), 
                         sd = sd(meanvar, na.rm = TRUE),
                         sem = sd/sqrt(n()),
                         n = n())
        gdata <- data.frame(gdata)
        
        # Crear los limite dependiendo si es sd, sem o ci (ahora solo sd)
        if (stat == "sd"){
            gdata <- mutate(gdata, li = media - sd, ls = media + sd)
        } else if (stat == "sem") {
            gdata <- mutate(gdata, li = media - sem, ls = media + sem)
        }
        
        # Craer el caption y los limites de Y
        nlab <- paste(gdata$cat1var,":", gdata$cat2var,"=", gdata$n, sep = "")
        nlab <- paste(nlab, collapse = " ")
        
        # Crear el gráfico
        g <- ggplot(data = gdata, aes(x=cat1var, y=media, fill = cat2var))
        g <- g + geom_errorbar(position=position_dodge(0.9), aes(ymin=li, ymax=ls), width=0.25)
        g <- g + geom_bar(position=position_dodge(0.9), stat="identity", colour = "black") 
        g <- g + labs(x = c1, y = var, caption = nlab, title = "Gráfico de medias")
        g <- g + scale_fill_discrete(name = c2)
        g <- g + theme(
            panel.border = element_rect(colour = "black", fill = NA, size = 0.25),
            panel.background = element_rect(fill = "grey95"),
            panel.grid.major.x = element_line(colour = "grey95"),
            panel.grid.major.y = element_line(colour = "grey"),
            panel.grid.minor = element_line(colour = "grey95"))
        return(g)
        
    }
 
}
