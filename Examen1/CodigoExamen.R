#1 e)
X <- rep(0,10000)
Y <- runif(10000)*2-1
#for(i in 1:10000){
  if(-1 <= Y[i] || Y[i] <= 0){
    X[i]<- -1+sqrt(2)*sqrt(Y[i]) 
  }else{
    X[i] <- -1+sqrt(2)*sqrt(1-(Y[i])) 
  }
#}
mean(X)

#Segunda pregunta del examen
#Integral 1 de la forma
#U <- a + (b-a) * runif(N)
#V <- c * runif(N)
#I <- (b-a) * c * mean(V < g(U))


#Una forma de integrar sería esta, con esta forma dura mucho mi computadora procesando el millon, pero con un N menor si me da
f <- function(x) log(x)
area_estimates <- vector(length = 10000)
for(i in 1:10000){
  query_points <- runif(n = i, min = 1/exp(1), max = exp(1))
  area_estimates[i] <- (exp(1)-(1/exp(1))) * mean(f(query_points))
}
(mean(area_estimates))

#Segunda forma posible
N <- 1000000
U <- (1/exp(1)) + (exp(1)-(1/exp(1))) * runif(N)
V <- 1 * runif(N)
(I <- (exp(1)-(1/exp(1))) * 1 * mean(V < log(U)))





