#1 e)
Y = runif(100000000)
X = (2*(Y)-1)/4
mean(X)


#Integral 1 de la forma
#U <- a + (b-a) * runif(N)
#V <- c * runif(N)
#I <- (b-a) * c * mean(V < g(U))
#Otra forma

#Otra forma
N <- 1000000
U <- (1/exp(1)) + (exp(1)-(1/exp(1))) * runif(N)
V <- 1 * runif(N)
(I <- (exp(1)-(1/exp(1))) * 1 * mean(V < log(U)))

f <- function(x) log(x)
area_estimates <- vector(length = 1000000)
for(i in 1:1000000){
  query_points <- runif(n = i, min = 1/exp(1), max = exp(1))
  area_estimates[i] <- (exp(1)-(1/exp(1))) * mean(f(query_points))
}
(mean(area_estimates))