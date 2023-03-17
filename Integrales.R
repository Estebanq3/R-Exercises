#1 Integral
f <- function(x) abs(sin(1/x))
area_estimates <- vector(length = 10000)
for(i in 1:10000){
  query_points <- runif(n = i, min = 0, max = 1)
  area_estimates[i] <- 1 * mean(f(query_points))
}
(mean(area_estimates))



#Integral 1 de la forma en la que la presenta el profe
#U <- a + (b-a) * runif(N)
#V <- c * runif(N)
#I <- (b-a) * c * mean(V < g(U))

N <- 1e4
U <- 0 + (1-0) * runif(N)
V <- 1 * runif(N)
(I <- (1-0) * 1 * mean(V < abs(sin(1/U))))



#Forma sin exactitud de calcular la integral
#query_points <- runif(n = i, min = 0, max = 1)
#(1* mean(f(query_points)))


#-------------------------------------------------
#2 integral
f <- function(x) abs(sin(1/x))
area_estimates <- vector(length = 10000)
for(i in 1:10000){
  query_points <- runif(n = i, min = 0, max = 5)
  area_estimates[i] <- 5 * mean(f(query_points))
}
(mean(area_estimates))

#Otra forma
N <- 1e4
U <- 0 + (5-0) * runif(N)
V <- 1 * runif(N)
(I <- (5-0) * 1 * mean(V < abs(sin(1/U))))

#-------------------------------------------------
#3 integral
f <- function(x) sin(1/x)
area_estimates <- vector(length = 10000)
for(i in 1:10000){
  query_points <- runif(n = i, min = 0, max = 1)
  area_estimates[i] <- 1 * mean(f(query_points))
}
(mean(area_estimates))


#Otra forma
N <- 1e4
U <- 0 + (1-0) * runif(N)
V <- 1 * runif(N)
(I <- (1-0) * 1 * mean(V < sin(1/U)))


#-------------------------------------------------
#4 integral
f <- function(x) exp(-x^(2))
area_estimates <- vector(length = 10000)
for(i in 1:10000){
  query_points <- runif(n = i, min = -2, max = 2)
  area_estimates[i] <- 4 * mean(f(query_points))
}
(mean(area_estimates))


#Integral 1 de la forma en la que la presenta el profe
#U <- a + (b-a) * runif(N)
#V <- c * runif(N)
#I <- (b-a) * c * mean(V < g(U))
#Otra forma
N <- 1e4
U <- -2 + (2--2) * runif(N)
V <- 1 * runif(N)
(I <- (2--2) * 1 * mean(V < sin(1/U)))


#-------------------------------------------------
#integral 5
N = 1e4
E = rnorm(N)
g = exp(-E^(2))
f = dnorm(E)
mean(g/f)


#-------------------------------------------------
#6 integral

N = 1e4
E = rexp(N)
g = exp(-sqrt(E))
f = dexp(E)
mean(g/f)


runif(1)
(-log(0.69)/(20))



f <- function(x) log(x)
area_estimates <- vector(length = 1000000)
for(i in 1:1000000){
  query_points <- runif(n = i, min = 1/exp(1), max = exp(1))
  area_estimates[i] <- (exp(1)-(1/exp(1))) * mean(f(query_points))
}
(mean(area_estimates))

