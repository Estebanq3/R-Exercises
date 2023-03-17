#Distribución de Bernoulli en R
p = 0.68
(U <- runif(1)) #Generando 1 numero aleatorio
(X<-1*(U < p)) #La x toma los valores de true(1) o false(0) dependiendo del valor de p y U
#Si la U es menor que la P es un acierto U < p si la U es mayor o igual a p es un fallo U >= p


#Distribucion Binomial
p = 0.4
(U <- runif(15)) # se forma vector U
(X <- sum(U < p)) #aqui cada casilla del vector U se ira comparando con p, la funcion sum ya multiplica por 1 por defecto, por eso no se multiplica por 1
#Se arroja como resultado en X la cantidad de aciertos que existieron en la simulacion


#Distribucion Geometrica
p = 0.01
X <- 1
U <- runif(1)
while(U > p){
  X <- X+1
  U <- runif(1)
}
X
#La X dice el numero de veces que se repitio el experimento hasta obtener un acierto


#Distribucion Binomial Negativa
p = 0.01
X <- 10 #tienen que haber 10 aciertos para que se detenga la simulacion
Y <- 0
Z <- 0 #fallos contabilizados en el camino a obtener determinado numero de aciertos
while(Y < X){
  U <- runif(1)
  (ifelse(U < p, Y <- Y+1, Z <- Z+1))
}


#Distribucion de Poisson
lambda <- 3
U <- runif(1)
k <- 0
F <- exp(-lambda)
while (U >= F) {
  F <- F + exp(-lambda) * lambda^k / factorial(k);
  k <- k + 1;
};
(X <- k)
#k cantidad de vuelos que han salido


#Generando z
qnorm(0.975)


