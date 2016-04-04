#######################################################################
###
### CALCULA LAMBDA-DELTA (PASO FORDWARD 2)
### @params: datos (data.frame)
### @params: alfa, beta, gamma
###
### Devolvemos lambda (vector) y beta (data.frame)
###
#######################################################################
dame_LambdaDelta <- function (datos, alfa, beta, gamma) {
  	n <- length(alfa)
  	lambda <- array(0, dim = c(n-1,1))
  	delta  <- data.frame(x=double(), y=double())
  	
  	#[1] "Lambda"
  	#[1] 0.0000000 0.2857143 0.2692308 0.2680412 0.2679558 0.4641026 0.0000000
  	
    ### CODIGO A REALIZAR:
    ### Calcular lambda y delta 
  	
  	lambda[1] <- gamma[1]/beta[1]
  	
  	for(i in 2:n){
  	  lambda[i] <- gamma[i]/(beta[i]-alfa[i]*lambda[i-1])
  	}
  	
  	#[1] "Delta"
  	#       x           y
  	#1 -7.5153759   7.4599349
  	#2 -5.3745308  14.0364746
  	#3 -2.5306046   0.4345468
  	#4  0.6685889  -7.9922452
  	#5  3.9135373   6.1390024
  	#6  8.0304573 -10.9904679
  	#7  7.5535695   2.4863764
  	
  	### -----------------------------------------
  	### -- Calculo Delta (Con 'd', Sin Lambda) --
  	### -----------------------------------------
  	
    #d <- data.frame(x=double(), y=double())
    
    #d[1,1] <- beta[1]*datos$x[1] + gamma[1]*datos$x[2]
    #d[1,2] <- beta[1]*datos$y[1] + gamma[1]*datos$y[2]
    
    #for (i in 2:n) {
    #  palfax <- alfa[i]*datos$x[i-1]
    #  pbetax <- beta[i]*datos$x[i]
    #  if(i != n){
    #    pgammax <- gamma[i]*datos$x[i+1]
    #  }else{
    #    pgammax <- 0
    #  }  	
    #  d[i,1] <- palfax + pbetax + pgammax
      
    #  palfay <- alfa[i]*datos$y[i-1]
    #  pbetay <- beta[i]*datos$y[i]
    #  if(i != n){
    #    pgammay <- gamma[i]*datos$y[i+1]
    #  }else{
    #    pgammay <- 0
    #  }
    #  d[i,2] <- palfay + pbetay + pgammay
    #}
    
    #delta[1,1] <- d[1,1]/beta[1]
    #delta[1,2] <- d[1,2]/beta[1]
    
  	#for(i in 2:n){
  	#  delta[i,1] <-  (d[i,1]-alfa[i]*delta[i-1,1])/(beta[i]-alfa[i]*lambda[i-1])
  	#  delta[i,2] <-  (d[i,2]-alfa[i]*delta[i-1,2])/(beta[i]-alfa[i]*lambda[i-1])
  	#}
  	
  	### -------------------------------------------
  	### -- Calculo Delta (Sin 'd', con 'Lambda') --
  	### -------------------------------------------
    
    for(i in 1:n){
      plambdax <- lambda[i]*datos$x[i+1]
      if(i == n){
        plambdax <- 0
      }
      delta[i,1] <-  1*datos$x[i] + plambdax
      
      plambday <- lambda[i]*datos$y[i+1]
      if(i == n){
        plambday <- 0
      }
      delta[i,2] <-  1*datos$y[i] + plambday
    }
    
  	### ---------------
    ### -- RESULTADO --
  	### ---------------
  	
    #$delta
    #       x         y
    #1 -7.5200000  7.460000
    #2 -5.7128571 10.795714
    #3 -2.4626923  1.290769
    #4  0.6735052 -4.232577
    #5  3.8844199  1.177017
    #6  8.4839744 -3.744385
    #7  7.5500000  2.490000
    
  	res        <- NULL
  	res$lambda <- lambda
  	res$delta  <- delta
  
  	return(res)
}