#######################################################################
###
### FUNCION COX-DE-BOOR
### @params: i (indice)
### @params: p (grado de la curva)
### @params: u (parametro)
### @params: nudos (vector)
###
### Devolvemos el valor de la funcion base N(i,p,u)
###
#######################################################################
N <- function (i, p, u, nudos) {
  	### CODIGO A REALIZAR
    ### Calcular el valor de la funcion base N(i,p,u)
    nip <- 0
    izq <- 0
    dcha <- 0
    
    # La funcion base N(i,p,u) es no nula en el intervalo de nudos [ui, ui+p+1), 
    # N(i,p,u) es no nula en (p+1) intervalos: [ui, ui+1), [ui+1, ui+2), . . . , [ui+p, ui+p+1)
    # En el intervalo [ui, ui+1), como mucho, p + 1 funciones base de grado p son no nulas:
    # N(i-p,p,u), N(i-p+1,p,u), N(i-p+2,p,u), ... , N(i-1,p,u) y N(i,p,u)

    if (p == 0) {
      ui <- nudos[i]
      ui1 <- nudos[i+1]
      if( ui <= u & u < ui1){
        nip <- 1
      }
      
    }else{
      Nip1 <- N(i,p-1,u,nudos)
      Ni1p1 <- N(i+1,p-1,u,nudos)
      if(Nip1 != 0){
        izq <- (u-nudos[i])/(nudos[i+p]-nudos[i]) * Nip1 
      }
      if(Ni1p1 != 0){
        dcha <- (nudos[i+p+1]-u)/(nudos[i+p+1]-nudos[i+1]) * Ni1p1
      }
      nip <- izq + dcha
      
    }
    
  	return(nip)
}
