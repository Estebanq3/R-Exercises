library(fitdistrplus)
library(plotrix)
library(dplyr)

#I Parte del Proyecto
clients_data <- read.csv("DatosEstudiantes.csv")

hist(clients_data$Tiempo,
     main = "Histograma de duraciones",
     col = "blue",
     breaks = 300,
     border = "black")
lines(density(clients_data$Tiempo))

shapiro.test(clients_data$Tiempo)

boxplot(clients_data$Tiempo,range = 1.5)

Q <- quantile(clients_data$Tiempo, probs=c(.25,.75), na.rm = FALSE)
iqr <- IQR(clients_data$Tiempo)
clients_filtered <- subset(clients_data, clients_data$Tiempo > (Q[1] - 1.5*iqr) & clients_data$Tiempo < (Q[2] + 1.5*iqr))

boxplot(clients_filtered$Tiempo,range = 1.5)

shapiro.test(clients_filtered$Tiempo)
times_log <- log(clients_filtered$Tiempo)

shapiro.test(times_log)


# GET PROCESSING TIME
processing_data <- read.csv("Processing_time.csv")

s1_songs.lm <- lm(MBP.2019..s.~Seconds, data=processing_data)
s2_songs.lm <- lm(MBP.2015.2.9..s.~Seconds, data=processing_data)
s3_songs.lm <- lm(MBP.2015.2.7..s.~Seconds, data=processing_data)

s_intercept <- rep(0,3)
s_slope     <- rep(0,3)

# server 1: intercept p-value > 0.05; slope p-value < 0.05
s_intercept[1] <- 0
s_slope[1]     <- s1_songs.lm$coefficients[2]

# server 2: intercept p-value > 0.05; slope p-value < 0.05
s_intercept[2] <- 0
s_slope[2]     <- s2_songs.lm$coefficients[2]

# server 3: intercept p-value > 0.05; slope p-value < 0.05
s_intercept[3] <- 0
s_slope[3]     <- s3_songs.lm$coefficients[2]


# Try one more server
s_intercept <- c(s_intercept, 0)
s_slope <- c(s_slope, s1_songs.lm$coefficients[2])

# x = duration, a = intercept, b = slope
get_processing_time <- function(x,a,b){return(as.numeric(a+x*b));}

get_price <- function(x){
  price <- as.integer(x/(60*6)) * 0.99 + 0.99
  return(price)
}

# PROYECTO 1
songs_data <- read.csv("songs.csv", header = TRUE)

Q <- quantile(songs_data$duration, probs=c(.25,.75), na.rm = FALSE)
iqr <- IQR(songs_data$duration)
songs_filtered <- subset(songs_data, songs_data$duration > (Q[1] - 1.5*iqr) & songs_data$duration < (Q[2] + 1.5*iqr))

songs.dist <- fitdist(songs_filtered$duration, "gamma", method="mme")

# SIMULATION M/M/3/r/r

lambda <- mean(data$Tasa.de.arribo...mes.) / (30*24*60)
mean_wait <- mean(times_log)
sd_wait   <- sd(times_log)

N <- 20
k <- 3
#k <- 10
#k <- 12
#r <- 2160000 * 0.01 # 1%
r <- 2160000 * 0.10  # 10%

profit      <- rep(0,N)
long_songs  <- rep(0,N)
long_sales  <- rep(0,N)
total_songs <- rep(0,N)
total_requests <- rep(0,N)

Twork_s1   <- rep(NA,N)
Twork_s2   <- rep(NA,N)
Twork_s3   <- rep(NA,N)

Njobs_s1   <- rep(NA,N)
Njobs_s2   <- rep(NA,N)
Njobs_s3   <- rep(NA,N)

Wmean      <- rep(0,N)        # average waiting time
Smax       <- rep(0,N)        # the longest waiting time
Smean      <- rep(0,N)        # average service time
Rmax       <- rep(0,N)        # the longest service time
Rmean      <- rep(0,N)        # average response time
Wmax       <- rep(0,N)        # the longest response time
Nwithdr    <- rep(0,N)        # number of withdrawn jobs
Nav        <- rep(0,N)        # number of jobs that did not wait

for(i in 1:N)
  {
  j    <- 0
  Time <- 0
  A    <- rep(0,k)

  arrival <- c()
  start   <- c()
  finish  <- c()
  server  <- c()
  
  while(Time < 60*24*7)
  {
    j <- j + 1
    rate <- r*lambda
    Time <- Time + rexp(1, rate)
  
    arrival <- c(arrival, Time)
    Nfree   <- sum(A < Time)
  
    u <- 1
    if(Nfree == 0) # all servers busy
    {
      for(v in 2:k) # checks the first to be available
      {
        if(A[v] < A[u])
        {
          u <- v
        }
      }
    
      wait_time <- rlnorm(1, mean_wait, sd_wait) # random wait time
    
      if(A[u] - Time > wait_time) # if job j waits more than estimated
      {
        start <- c(start, Time+wait_time)
        finish <- c(finish, Time+wait_time)
        u <- 0  # no server
      }
      else
      {
        start <- c(start, A[u])
      }
    }
    else # servers available
    {
      u <- ceiling(runif(1)*k)
      while(A[u] > Time)
      {
        u <- ceiling(runif(1)*k)
      }
    
      start <- c(start, Time)
    }
  
    server <- c(server,u)
  
    if(u > 0) # if job j doesn't withdraw
    {
      duration <- rgamma(1, as.numeric(songs.dist$estimate[1]), as.numeric(songs.dist$estimate[2])) # random song duration
      total_songs[i] <- total_songs[i] + 1
      
      if(duration > 360)
      {
        long_songs[i] <- long_songs[i] + 1
        long_sales[i] <- long_sales[i] + get_price(duration)
      }
    
      profit[i] <- profit[i] + get_price(duration)
    
      S <- get_processing_time(duration, s_intercept[u], s_slope[u]) / 60
      finish <- c(finish, start[j]+S)
      A[u] <- start[j] + S;
    }
  }

  job = 1:j
  #print(data.frame(job, arrival, start, finish, server))

  total_requests[i] <- length(job)
  result.data <- data.frame(start, arrival, finish, server)
  
  Twork <- rep(NA,k)
  Njobs <- rep(NA,k)
  
  for (u in 1:k) {                                # Twork(u) is the total working
    Twork[u] <- sum((server == u)*(finish-start)) # time for server u.
    Njobs[u] <- sum(server == u)                  # number of jobs served by u
  }

  Twork_s1[i] <- Twork[1]
  Twork_s2[i] <- Twork[2]
  Twork_s3[i] <- Twork[3]

  Njobs_s1[i] <- Njobs[1]
  Njobs_s2[i] <- Njobs[2]
  Njobs_s3[i] <- Njobs[3]

  Wmean[i] <- mean(start-arrival)                    # average waiting time
  Wmax[i] <- max(start-arrival)                      # the longest waiting time
  
  Smean[i] <- as.numeric(result.data %>% filter(server > 0) %>% summarize(Avg = mean(finish-start)))   # average service time
  
  Smax[i] <- max(finish-start)                       # the longest service time
  Rmean[i] <- as.numeric(result.data %>% filter(server > 0) %>% summarize(Avg = mean(finish-arrival))) # average response time
  
  Rmax[i] <- max(finish-arrival)                     # the longest response time
  
  Nwithdr[i] <- sum(server == 0)                     # number of withdrawn jobs
  Nav[i] <- sum(start-arrival < 0.00001)             # number of jobs that did not wait
}


mean(profit)/7 * 365                             #valor esperado de ventas (extrapolado)
sd(profit)/sqrt(length(profit))                  #error estandar de ventas
mean(profit)/7 * 365 * 0.70                      #valor esperado de ganacias (extrapolado)
mean(long_songs)/mean(total_songs)               #fraccion de caciones de mas de 6 min
mean(long_sales)/mean(profit)                    #fraccion de ventas por canciones de mas de 6 min
mean(total_songs)/7 * 365                        #valor esperado de canciones procesadas (extrapolado)

mean(Njobs_s1) / mean(total_songs)               #fraccion procesada por el servidor 1
mean(Njobs_s2) / mean(total_songs)               #fraccion procesada por el servidor 2
mean(Njobs_s3) / mean(total_songs)               #fraccion procesada por el servidor 3
mean(Nwithdr) / mean(total_requests)             #fraccion de solicitudes retiradas

mean(Wmean)      # tiempo promedio de espera
mean(Wmax)       # tiempo promedio maximo de espera

mean(Smean)      # tiempo promedio de servicio
mean(Smax)       # tiempo promedio maximo de servicio

mean(Rmean)      # tiempo promedio de respuesta
mean(Rmax)       # tiempo promedio maximo de respuesta

mean(Wmean)/mean(Rmean)
