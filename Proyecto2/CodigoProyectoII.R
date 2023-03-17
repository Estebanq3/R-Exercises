install.packages('expm')
install.packages('MASS')
install.packages('EmiStatR')
install.packages('markovchain')
install.packages('devtools')
devtools::install_github('spedygiorgio/markovchain')


library(expm)
library(MASS)
library(markovchain)



#----------------------------------------------------------------
#Cálculo de la Regularidad
P=t(matrix(c(c(0.86,0.12,0.02,0,0),c(0,0,0.90,0.10,0),c(0.07,0.14,0.69,0.07,0.03),c(0,0.17,0.13,0.70,0),c(0.50 ,0 ,0 ,0 ,0.50)),nrow=5))
(P%^%3) #3 pasos son necesarios para alcanzar la regularidad
#----------------------------------------------------------------
#Cálculo de la distribución estacionaria

statesNames <- c("dormir", "comer", "estudiar","clases","jugar")
markovB <- new("markovchain", states = statesNames, transitionMatrix =
                 matrix(c(0.86,0.12,0.02,0,0,
                          0,0,0.90,0.10,0,
                          0.07,0.14,0.69,0.07,0.03,
                          0,0.17,0.13,0.70,0,
                          0.50 ,0 ,0 ,0 ,0.50
                          ), nrow = 5,
                        byrow = TRUE, dimnames=list(statesNames,statesNames)),
               name = "A markovchain Object"
)

#0.87,0,0.07,0,0.50,
#0.12,0,0.14,0.17,0,
#0.02,0.90,0.69,0.13,0,
#0,0.10,0.07,0.70,0,
#0,0,0.03,0,0.50
steadyStates(markovB)
#----------------------------------------------------------------
#Cálculo de la distribución estacionaria Alternativa, modo manual completamente
# Get the eigenvectors of P, note: R returns right eigenvectors
r=eigen(P)
rvec=r$vectors
# left eigenvectors are the inverse of the right eigenvectors
lvec=ginv(r$vectors)
# The eigenvalues
lam<-r$values
# Two ways of checking the spectral decomposition:
# Standard definition
# rvec%*%diag(lam)%*%ginv(rvec)

#Calculo del valor de pi1,pi2,pi3,pi4,pi5
pi_eig<-lvec[1,]/sum(lvec[1,])
pi_eig
#Asegurando que las probabilidades sumadas sean 1
sum(pi_eig)

#----------------------------------------------------------------
#Simulacion de Markov Alternativa 1

P <- t(matrix(c(0.86,0.12,0.02,0,0,
                0,0,0.90,0.10,0,
                0.07,0.14,0.69,0.07,0.03,
                0,0.17,0.13,0.70,0,
                0.50 ,0 ,0 ,0 ,0.50), nrow=5, ncol=5))

run.mc.sim <- function( P, num.iters = 168 ) {
  
  # number of possible states
  num.states <- nrow(P)
  
  # stores the states X_t through time
  states     <- numeric(num.iters)
  
  # initialize variable for first state 
  states[1]    <- 1
  
  for(t in 2:num.iters) {
    
    # probability vector to simulate next state X_{t+1}
    p  <- P[states[t-1], ]
    
    # draw from multinomial and determine state
    states[t] <-  which(rmultinom(1, 1, p) == 1)
  }
  return(states)
}


num.chains     <- 1000
num.iterations <- 168
chain.states <- matrix(NA, ncol=num.chains, nrow=num.iterations)
for(c in seq_len(num.chains)){
  chain.states[,c] <- run.mc.sim(P)
}

matplot(chain.states, type='l', lty=1, col=1:5, ylim=c(0,6), ylab='state', xlab='time')
abline(h=1, lty=3)
abline(h=5, lty=3)


#----------------------------------------------------------------

#Pregunta 6
#Calcular la fraccion de tiempo que se estuvo en cada uno de los estados en las mil simulaciones
#Cantidad de horas que se estuvo en cada estado entre 168000 horas que son el total de horas simuladas
a <- table(chain.states) #Mediante este comando contamos la cantidad de veces que aparece cada estado en la tabla
a[1]/168000 #Cantidad de horas que se esta en el estado 1 entre las 168000 horas
a[2]/168000 #"horas estado 2"
a[3]/168000 #"horas estado 3"
a[4]/168000 #"horas estado 4"
a[5]/168000 #"horas estado 5"

#Al comparar los resultados obtenido con las probabilidades de la distribucion estacionaria del punto 3 podemos observar como
#se obtienen exactamente los mismos valores

#----------------------------------------------------------------

#Pregunta7
#Promedios porcentaje cuadro 1
vec <- numeric(168000)
vecSemanal <- numeric(1000)

contador <- 1
contadorSemanal <- 1
SemanaEstados <-c(1,1,1,1,1,1,1,1,2,3,4,4,2,3,3,4,4,4,4,4,2,3,3,3,
                  1,1,1,1,1,1,2,4,4,4,3,3,2,3,3,4,4,4,4,4,2,3,3,3,
                  1,1,1,1,1,1,1,1,3,3,3,3,2,3,3,3,3,3,3,3,2,3,3,3,
                  1,1,1,1,1,1,1,1,2,3,4,4,2,3,3,3,4,4,4,3,2,3,3,3,
                  1,1,1,1,1,1,2,4,4,4,3,3,2,3,3,3,3,3,3,3,2,3,3,3,
                  1,1,1,1,1,1,1,1,2,3,3,3,2,3,3,3,3,3,3,3,2,3,5,5,
                  1,1,1,1,1,1,1,1,2,3,3,3,2,3,3,3,3,3,3,3,2,3,5,5)
SemanaEstados
chain.states[9,13]

for (i in 1:1000){
  
  for (j in 1:168) {
    if(SemanaEstados[j] == chain.states[j,i]){
        vec[contador] = 1
        contadorSemanal <- contadorSemanal+1
    }
    contador<- contador+1
  }
  vecSemanal[i] = contadorSemanal/168 
  contadorSemanal = 0
}

#Promedio de igualdad
mean(vecSemanal)
#Promedio de igualdad calculado de forma alternativa 2
sum(vec)/168000









#--------------------------------------------------------------
#Pregunta 7 Antigua, no es lo que pedia el profe
#Dormir
(probDormir= 52/168)
coincidenciaDormir = 1- abs(probDormir-a[1]/168000)
#Comer
(probComer= 20/168)
coincidenciaComer = 1- abs(probComer-a[2]/168000)
#Estudiar
(probEstudiar= 69/168)
coincidenciaEstudiar = 1- abs(probEstudiar-a[3]/168000)
#Clases
(probClases= 23/168)
coincidenciaClases = 1- abs(probClases-a[4]/168000)
#Jugar
(probJugar = 4/168)
coincidenciaJugar = 1- abs(probJugar-a[5]/168000)

#Promedio del porcentaje de coincidencia
vectorCoincidencias <- c(coincidenciaDormir, coincidenciaComer, coincidenciaEstudiar, coincidenciaClases, coincidenciaJugar)
mean(vectorCoincidencias)


#----------------------------------------------------------------
#Simulacion de Markov Alternativa 2
M <- 168

N <- 168; X <- rep(0,N); U <- runif(N);
P <- matrix(c(0.86,0,0.07,0,0.50,  0.12,0,0.14,0.17,0,  0.02,0.90,0.69,0.13,0,  0,0.10,0.07,0.70,0,  0,0,0.03,0,0.50  ),5); P ; P_k <- c(1,0,0,0,0); P_k # P_0
for (k in 1:N) {
  X[k] = if (U[k] < P_k[1]) 1 else 2
  P_k <- P[X[k], ] }

P_k
X

#-----------------------------------------------------------------------
# Alternativa 3
N <- 1000 # length of sample path
X <- rep(0,N) # initialization
Q <- c(1,0,0,0,0) # initial distribution
prob <- c(0.86,0,0.07,0,0.50,  0.12,0,0.14,0.17,0,  0.02,0.90,0.69,0.13,0,  0,0.10,0.07,0.70,0,  0,0,0.03,0,0.50  ) #vector of probabilities
P <- matrix(prob,5,5) #transition prob. matrix
prob
U <- runif(N) #N Uniform variables
for (t in 1:N) { #simulate X sequentially
X[t] <- (U[t]<=Q[1])+ # X(t)=1 with prob. Q(1)
2*(U[t] > Q[1]) #X(t)=2 with prob. Q(2)
Q <- P[ X[t], ] #the X(t)-th row of P
}; X