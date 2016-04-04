##########################################################################################
###
### CALCULA LOS VALORES MAXIMOS Y MINIMOS DE LAS COORDENADAS DE LOS PUNTOS DE CONTROL
### @params: puntosControl (data.frame)
###
### Devolvemos xy (data.frame)
###
##########################################################################################
calculaLimites <- function(puntosControl) {
    ### Valores por defecto
    xy <- data.frame(x=c(-10,10), y=c(-10,10))

    ### CODIGO A REALIZAR:
    ### Si hay puntos de control,
    ###   Calcular minX, maxX, minY, maxY que corresponden a los valores minimos y maximos
    ###   de las coordenadas de los puntos de control
    ###             xy <- data.frame(x=c(minX,maxX), y=c(minY,maxY))
    		
    minX <- 1000000
    maxX <- -1000000
    minY <- 1000000
    maxY <- -1000000
    
    for(i in 1:nrow(puntosControl)){
      if(minX > puntosControl[i,1]){
        minX <- puntosControl[i,1]
      }
      if(maxX < puntosControl[i,1]){
        maxX <- puntosControl[i,1]
      }
      if(minY > puntosControl[i,2]){
        minY <- puntosControl[i,2]
      }
      if(maxY < puntosControl[i,2]){
        maxY <- puntosControl[i,2]
      }
    }
    
    xy[1,1] <- minX
    xy[2,1] <- maxX
    xy[1,2] <- minY
    xy[2,2] <- maxY
    
    
  	return(xy)
}