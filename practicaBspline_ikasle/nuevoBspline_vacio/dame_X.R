#######################################################################
###
### CALCULA X (PASO BACKWARD)
### @params: delta (data.frame)
### @params: lambda (vector)
###
### Devolvemos X (data.frame)
###
#######################################################################
dame_X <- function (delta, lambda) {
  
    #[1] "X"
    #        x          y
    #1 -7.51537589   7.459935
    #2 -4.65576348  13.103857
    #3 -2.51568576   3.264162
    #4 -0.05541292 -10.510001
    #5  2.70108381   9.393165
    #6  4.52482638 -12.144402
    #7  7.55356947   2.486376  
  
    ### CODIGO A REALIZAR:
    ### Calcular X
    X <- NULL
    X <- data.frame(x=double(), y=double())
    
    n <- length(lambda)
    
    X[n,1] <- delta[n,1]
    X[n,2] <- delta[n,2]
    
    for(i in (n-1):1){
      auxDeltaX <- delta[i,1]
      XyLambdaX <- lambda[i]*X[i+1,1]
      X[i,1] <- auxDeltaX - XyLambdaX
      
      auxDeltay <- delta[i,2]
      XyLambdaY <- lambda[i]*X[i+1,2]
      X[i,2] <- auxDeltay - XyLambdaY
    }
    
    ### ----------------
    ### -- Resultado ---
    ### ----------------
    
    #[1] "X"
    #   x     y
    #1 -7.52  7.46
    #2 -5.01 10.05
    #3 -2.46  2.61
    #4 -0.01 -4.90
    #5  2.55  2.49
    #6  4.98 -4.90
    #7  7.55  2.49
    
    return(X)
}