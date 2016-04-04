#######################################################################
c###
### CALCULA LOS NUDOS UNIFORMES DE UNA BSPLINE
### @params: nDatos (numero de datos) ### [m] (n) (numero de nudos)
### @params: p (grado de la bspline)
###
### Devolvemos un vector de nudos
###
#######################################################################
calculaNudosUniforme <- function(nDatos, p) {

    ### CODIGO A REALIZAR:
    ### Calcular los nudos uniformes de una bspline
  
    ### (n+5)=n nudos, (n+1) ptos. ctrl. p=3
    # nudos <- NULL
    ### n=m-p-1
    ### Caso normal 0..m [1..(m+1)]: (m+1) nudos,
    ### Caso especial 1..m: m nudos.
    ### num. nudos 'm' = 1..'m'
    #numN <- m-p-1 ### n=n+1; n+1=(m+1)-p-1; n=m+1 
 	  #nudos[1:(p+1)] <- rep(0,p+1)
    #for(k in 2:(numN-3)){
    #  nudos[k+3] <- k/(numN-2)
    #}
    #nudos[(m-p):m] <- rep(1,p+1)
  	#return(nudos)
    
    nudos <- NULL
  
    nudos <- c(rep(0.0, p), seq(0, 1, 1/(nDatos-p+2)), rep(1.0, p))
    
    return(nudos)
}
