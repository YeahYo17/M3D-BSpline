#######################################################################
###
### CALCULA LOS PUNTOS DE CONTROL DE LA BSPLINE
### @params: datos (data.frame)
### @params: nudos (vector)
###
### Devolvemos los puntos de control de la bspline (data.frame)
###
#######################################################################
calculaPuntosControl <- function(datos, nudos) {
  	nDatos <- dim(datos)[1]
  	### Paso Forward
  	ABG <- dame_AlfaBetaGamma(nDatos, nudos)
  	print("Alfa")
  	print(ABG$alfa)
  	print("Beta")
  	print(ABG$beta)
  	print("Gamma")
  	print(ABG$gamma)
  	
  	LD  <- dame_LambdaDelta(datos, ABG$alfa, ABG$beta, ABG$gamma)
  	print("Lambda")
  	print(LD$lambda)
  	print("Delta")
  	print(LD$delta)
  
  	### Paso backward
  	X <- dame_X(LD$delta, LD$lambda)
  	print("X")
  	print(X)

    ### CODIGO A REALIZAR:
    ### Calcular los puntos de control de la bspline
    puntosControl <- NULL
    
    puntosControl <- data.frame(x=double(), y=double())
  	rownames(puntosControl) <- NULL
  	colnames(puntosControl) <- c("x", "y")
  	puntosControl <- data.frame(puntosControl)
  	
  	n <- length(ABG$alfa)+2
  	 
  	puntosControl[1,1] <- datos$x[1] 
  	puntosControl[1,2] <- datos$y[1] 
  	puntosControl[n,1] <- datos$x[n-2] 
  	puntosControl[n,2] <- datos$y[n-2] 
  	 
  	for(i in 2:(n-1)){
  	  puntosControl[i,1] <- X[i-1,1]
  	  puntosControl[i,2] <- X[i-1,2]
  	}
  
  	return(puntosControl)
}