#' @title Olito's plot for otable output, grafico de barras
#' 
#' @description Esta función hace un grafico de barras a partir de un resultado de
#' calcular una tabla con "otable". También imprime la tabla con la cual se hizo
#' el gráfico, aunque no está en el return.
#' 
#' @param tabdata Tabla o Lista resultado de construir una tabla con "otable"
#' @param stat El tipo de dato con el que contruye la tabla puede ser: freq, cell, row, col
#' 
#' @return Devuelve un objeto grafico de ggplot
#' 
#' @examples
#' # Get some data
#' data(mtcars)
#' 
#' # For a one variable table
#' onevar <- otable(rvar = "gear", data = mtcars)
#' otable.plot(onevar, stat = "freq")
#' otable.plot(onevar, stat = "cell")
#' 
#' # For a contingency table
#' twovar <- otable(rvar = "cyl", cvar = "gear", data = mtcars)
#' otable.plot(twovar, stat = "freq")
#' otable.plot(twovar, stat = "cell")
#' otable.plot(twovar, stat = "col")
#' otable.plot(twovar, stat = "row")
#'
otable.plot <- function(tabdata = NULL, stat = "freq"){
    require(ggplot2)

    # Si es un data.frame puede venir de un otable 1 var
    if (class(tabdata) == "data.frame"){
        # Que el df venga de un otable
        if (sum(names(tabdata)[2:3] %in% c("freq", "pct")) == 2){
            # que el stat esté correcto
            if (stat %in% c("freq", "cell") == TRUE){
                print("data Ok")
            } else {stop("Elije \"freq\" o \"cell\"")}
        } else {stop("tabdata debe provenir de un otable")}
    
    # Si otable 2 variables, puede ser una lista
    } else if (class(tabdata) == "list"){
        # Que la lista venga de un otable
        if (sum(names(tabdata) %in% c("freq","row","col","cell","pvalue")) == 5){
            # que el stat esté correcto
            if (stat %in% c("freq", "cell", "col", "row") == TRUE){
                print("data Ok")
            } else {stop("Elije un stat valido")}
        } else {stop("tabdata debe provenir de un otable, lista incompleta o equivocada")}
    
    # Si es otra cosa avisar
    } else {
        stop("tabdata debe ser un data.frame o una lista")
    }

    
    # Tonce pa elegir 1 o 2 var lo haremos por clase
    ## Gráfico una variables
    if (class(tabdata) == "data.frame"){
        data <- tabdata
        print(data)
        
        # previos
        varname <- names(data)[1]
        names(data) <- c("x", "freq", "cell")
        data <- slice(data, -dim(data)[1])
        total <- sum(data$freq)
        minf <- min(data$freq)
        minp <- min(data$cell)
        
        # Si freq o cell
        if (stat == "freq"){
            g <- ggplot(data, aes(x = x, y = freq)) + geom_col(colour = "black", fill = "grey") +
                 xlab(paste(varname, ": N =", total)) + ylab("Conteo")
        } else {
            g <- ggplot(data, aes(x = x, y = cell)) + geom_col(colour = "black", fill = "grey") +
                 xlab(paste(varname, ": N =", total)) + ylab("Porcentaje")
        }
    
    ## Grafico 2 variables
    } else if (class(tabdata) == "list"){
        # Previos, solo cambia el tipo de tabla tons con función arreglo el df
        total <- tabdata$freq[dim(tabdata$freq)[1],dim(tabdata$freq)[2]]
        data <- tabdata[[stat]]
        print(data)
        
        # Arreglo de lo que se haya elegido
        data$total <- NULL
        data <- data[-dim(data)[1],]
        data <- reshape(data, varying = 2:dim(data)[2], direction = "long")
        data$id <- NULL
        variables <- c(names(data)[1], names(data)[3])
        names(data) <- c("X", "grp", "value")
        data <- mutate(data, grp = factor(grp), X = factor(X))
        
        # El plot
        g <- ggplot(data, aes(x = X, y = value, fill = grp)) +
             xlab(paste(variables[1], ": N =", total)) + 
             geom_bar(stat = "identity", position=position_dodge(), colour="black") +
             scale_fill_discrete(name = variables[2])
             
        # Ylab
        if (stat == "freq"){
            y <- "Conteo"
        } else if (stat == "cell"){
            y <- "Porcentaje del total"
        } else if (stat == "row"){
            y <- paste("Pct. fila: 100% en", variables[1])
        } else if (stat == "col"){
            y <- paste("Pct. columna: 100% en", variables[2])
        }
        
        g <- g + ylab(y)
        
    } else {
        stop("Alguna cagada quedó, revisar  :(")
    }
   
    # Retorno
    return(g)
}
