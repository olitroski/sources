# Script captura de log Stata
La idea es pasar un do-file en Stata en dos posibles modalidades, directo desde el do o en un loop programado. De momento sólo para detectar regresión lineal. Asume la siguiente secuencia.

- Regresión lineal (`regress`)
	- Contrastes
	- Margins
	- Marginsplot


## Detección regresión
En toda regresión lineal el encabezado es el siguiente

    Source
    -----
    Model
    Residual
    -----
    Total

Una vez detectado habría que capturar varias cosas

- Numero de observaciones
- P-valor del modelo nulo
- R cuadrado ajustado
- El Root MSE que viene siendo el `SS (residual) / n-2`

En la parte de la regresión capturar la tabla

- Reconstruir el modelo porque en la versión de programación no está el comando
- Coeficientes, error estándard, t, pvalor, intervalo.

**Nombre de la función: "read.regress"**
