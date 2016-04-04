#######################################################################
###
### CALCULA ALFA, BETA, GAMMA (PASO FORDWARD 1)
### @params: nVar [n] (integer)
### @params: nudos (vector)
###
### Devolvemos los vectores alfa, beta, gamma
###
#######################################################################
dame_AlfaBetaGamma <- function (nVar, nudos) {
  	alfa  <- array(0, dim = nVar)
  	beta  <- array(0, dim = nVar)
  	gamma <- array(0, dim = nVar)
  
  	### alfa[1] == alfa[n] == 0
  	### gamma[1] == gamma[n] == 0
    ### beta[1] == beta[n] == 1
  	
    #alfa[1] <- 0
    #for(k in 2:(nVar-1)){
    ##for(k=2 to nVar-1){
    #  alfa[k]
    #  beta[k]
    #  gamma[k]
    #}
    
    #[1] "Alfa"     N(k,3,nudos[k+3])
    #[1] 0.0000000 0.2500000 0.1666667 0.1666667 0.1666667 0.1666667 0.0000000
    #[1] "Beta"     N(k+1,3,nudos[k+3])
    #[1] 1.0000000 0.5833333 0.6666667 0.6666667 0.6666667 0.5833333 1.0000000
    #[1] "Gamma"    N(k+2,3,nudos[k+3])
    #[1] 0.0000000 0.1666667 0.1666667 0.1666667 0.1666667 0.2500000 0.0000000
    
    ### CODIGO A REALIZAR:
    ### Calcular los vectores alfa, beta, gamma
  
    alfa[1] <- alfa[nVar] <- 0
    beta[1] <- beta[nVar] <- 1
    gamma[1] <- gamma[nVar] <- 0
    
    for(k in 2:(nVar-1)){
      alfa[k] <- N(k,3,nudos[k+3],nudos)
      beta[k] <- N(k+1,3,nudos[k+3],nudos)
      gamma[k] <- N(k+2,3,nudos[k+3],nudos)
    }

  	res       <- NULL
  	res$alfa  <- alfa
  	res$beta  <- beta
  	res$gamma <- gamma
  	
  	return(res)

}