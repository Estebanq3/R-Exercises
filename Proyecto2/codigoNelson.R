install.packages('expm')
install.packages('MASS')
install.packages('EmiStatR')
install.packages('markovchain')
install.packages('devtools')

library(expm)
library(MASS)
library(markovchain)

P=t(matrix(c(c(0.87,0.12,0.02,0,0),c(0,0,0.90,0.10,0),c(0.07,0.14,0.69,0.07,0.03),c(0,0.17,0.13,0.70,0),c(0.50 ,0 ,0 ,0 ,0.50)),nrow=5))

P%^%2
P%^%3 # 3 pasos son necesarios para alcanzar la regularidad

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

steadyStates(markovB)



#Simulacion de Markov
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
    
    ## draw from multinomial and determine state
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

matplot(chain.states, type='l', lty=1, col=1:5, ylim=c(0,6), ylab='actividad', xlab='tiempo (horas)')
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

#----------------------------------------------------------------

#Pregunta8

#Simulacion con valores estacionarios
run.steady.sim <- function(indexes, sorted, num.iters = 168 ) {
  
  # number of possible states
  num.states <- length(indexes)
  
  # stores the states X_t through time
  states     <- numeric(num.iters)
  
  # initialize variable for first state 
  states[1]    <- 1
  
  for(t in 2:num.iters) {
    
    # probability vector to simulate next state X_{t+1}
    p <- runif(1)
    
    ## draw from multinomial and determine state
    states[t] <-  indexes[which(sorted > p)[1]]
  }
  return(states)
}


states <- steadyStates(markovB)
states.index <- order(states)
states.sorted <- cumsum(states[order(states)])

num.chains     <- 1
num.iterations <- 168
chain.states <- matrix(NA, ncol=num.chains, nrow=num.iterations)
for(c in seq_len(num.chains)){
  chain.states[,c] <- run.steady.sim(states.index, states.sorted)
}

matplot(chain.states, type='l', lty=1, col=1:5, ylim=c(0,6), ylab='actividad', xlab='tiempo (horas)')
abline(h=1, lty=3)
abline(h=5, lty=3)


# Porcentaje de coincidencia teórico

SemanaEstados <-c(1,1,1,1,1,1,1,1,2,3,4,4,2,3,3,4,4,4,4,4,2,3,3,3,
                  1,1,1,1,1,1,2,4,4,4,3,3,2,3,3,4,4,4,4,4,2,3,3,3,
                  1,1,1,1,1,1,1,1,3,3,3,3,2,3,3,3,3,3,3,3,2,3,3,3,
                  1,1,1,1,1,1,1,1,2,3,4,4,2,3,3,3,4,4,4,3,2,3,3,3,
                  1,1,1,1,1,1,2,4,4,4,3,3,2,3,3,3,3,3,3,3,2,3,3,3,
                  1,1,1,1,1,1,1,1,2,3,3,3,2,3,3,3,3,3,3,3,2,3,5,5,
                  1,1,1,1,1,1,1,1,2,3,3,3,2,3,3,3,3,3,3,3,2,3,5,5)

states <- steadyStates(markovB)
states <- states[SemanaEstados]
states[1] = 1 # estado inicial
mean(states)