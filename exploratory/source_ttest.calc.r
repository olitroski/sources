# Función para calcular un ttest a partir de datos estadísticos descriptivos
# toma como argumentos las medias, desviaciones y tamaños muestrales
# está bueno para hacer source.
# ------------------------------------------------------------------------------
## t-Student test con estadisticas 
# m1, m2: the sample means
# s1, s2: the sample standard deviations
# n1, n2: the same sizes
# m0: the null value for the difference in means to be tested for. Default is 0. 
# equal.variance: whether or not to assume equal variance. Default is FALSE. 
ttest <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE) {
     # Welch-satterthwaite df
     if (equal.variance==FALSE){
          se <- sqrt( (s1^2/n1) + (s2^2/n2) )
          df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1) )

     # Pooled standard deviation, scaled by the sample sizes          
     } else {
          se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
          df <- n1+n2-2
     }      

     # Estaditica y resultados
    t <- (m1-m2-m0)/se 
    data <- data.frame(m1, m2, m1-m2, se, t, 2*pt(-abs(t),df))
    names(data) <- c("Mean1", "Mean2", "Difference", "StdError", "t", "pvalue")
    return(data) 
}