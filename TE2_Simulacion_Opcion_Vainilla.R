"###############################Tarea exámen parte 2 de Procesos estaocástico 1##########################

Programa que simula el precio de una Opcion Vainilla sujeta a un 
activo subyavente St

@Author : Aarón López Pedraza
@Date   : 28-06-2020
@Version: 1.0
@update1: 01-12-2020
@Version: 1.1

@Update1{
Docuementé un poco mejor el código
        }

"

# numero normales ---------------------------------------------------------
set.seed(21)#plantamos la semilla para que la simulación se reproducible

N <- 1000 #¿Qué tan grande N? Tú decide.

Zi <- rnorm(N,mean =0 , sd =1 ) #creamos los aleatorios normales estandar

Zi
hist(Zi)#un histograma de los numeros

# valores -----------------------------------------------------------------

St <- 42; volatilidad <- .2; r <- .1; delta_T <- .5; K <- 40

# Si(T) ---------------------------------------------------------------------

Si_T <-  St * exp((r-(0.5*(volatilidad^2))*delta_T) + (volatilidad*Zi)*sqrt(delta_T))

class(Si_T)
# Ct ----------------------------------------------------------------------

# CT = max(Si_T-K,0), el precio


n_esperanza <- rep(0,N)#para cada entrada de este vector vamos aplicar l funcion CT
esperanza <- 0

#funcion que nos calcula el promedio de de la sucesion de simulacones Zi tras aplicar CT 
Esperanza <- function(vektor,N){
  
  for (i in 1:N) {
    
   n_esperanza[i] <-  max(vektor-40,0)
   
  }
  n_esperanza <- sum(n_esperanza)#acumulamos
  esperanza <- (1/N)*n_esperanza#promediamos
  return(esperanza)
}


# Aplicamos la funcion -------------------------------------------------

esperanza <- Esperanza(Si_T,N)

# Precio de la opcion al tiempo t ------------------------------------------------------------------

Ct <- exp((-1*r)*(delta_T))*esperanza

#Por lo tanto el precio del activo al tiempo t es
Ct

