#install.packages('nortest')
#install.packages('car')
#install.packages('moments')
#install.packages('fitdistrplus')
#install.packages('plotrix')

library(readr)
library(nortest)
library(car)
library(moments)
library(fitdistrplus)
library(plotrix)

#I PARTE DEL PROYECTO

#Leyendo los datos y guardándolos en outputData
outputData <- read_csv("C:/Users/Esteban Quesada/Desktop/ProyectoMetodos/outputData.csv")
#(duration <- outputData$duration)

#Hacer gráfico de las duraciones de las canciones
hist(duration,
     main = "Histograma de duraciones",
     xlab="Duración", 
     ylab="Frecuencia",
     col = "blue",
     breaks = 300,
     border = "black")
lines(density(duration))

#Es normal, el gráfico? R/Con solo mirar un histograma de probabilidad, puede saber si es normal observando su forma. Si la gráfica tiene aproximadamente
#una forma de campana y es simétrica con respecto a la media, generalmente se puede asumir la normalidad. En este caso, por lo tanto parece ser una distribución
#normal, más solo es una suposición basada en el texto anterior.

#Prueba de Normalidad
ad.test(duration)
shapiro.test(duration)
#Hipotesis:
#H0: La muestra proviene de una distribución normal
#H1: La muestra no proviene de una distribución normal

#El nivel de significancia que se trabajará es de 0.05. Alfa = 0.05
#Criterio de Decisión
#Si P < Alfa Se rechaza H0
#Si p >= Alfa No se rechaza H0
#Segun los resultados de ambas pruebas de normalidad realizadas, una prueba con el uso de la biblioteca nortest, y la segunda prueba
#realizada con la prueba de normalidad por defecto que se puede ejecutar en R, se obtiene un p-value muy pequeño, en ambos casos
#se obtiene un p-value de 2.2e-16, es decir p-value < 0.05, por lo tanto se rechaza la hipótesis H0. De esta forma podemos afirmar
#que la muestra NO proviene de una distribucion normal.


qqPlot(duration)
#Analizando el Q-Q plot tenemos que tener claro que diremos se puede asumir una distribución normal si  los 
#puntos caen aproximadamente a lo largo de esta línea de referencia. Sin embargo, como se observa en el Q-Q plot de duraciones de las canciones
#es claro que no podremos asumir tal normalidad debido a que existen muchos puntos que no caen aproximadamente a los largo de la linea
#de referencia, podemos notar entre los norm quantiles 1 y 3 una gran cantidad de puntos que desobedecen el seguir la línea de referencia
#y más bien caen considerablemente lejos de la misma.


boxplot(duration,range = 1.5)
#Asimismo realizando un diagrama de cajas notamos como existen bastantes puntos los cuales consideramos valores extremos atípicos
#Por lo tanto podemos suponer que si existen valores atípicos que deben ser eliminados, sin embargo, esto en realidad es tan solo una
#suposición, debemos realizar una prueba que verdaderamente afirme que existen valores extremos atípicos como por ejemplo: 1,5IQR 

#1.5 IQR
# El criterio IQR significa que todas las observaciones por encima de q0.75 +1.5*IQR o por debajo de q0.25-1.5*IQR(donde q0.25 y q0.75
#corresponden al primer y tercer cuartil respectivamente, y IQR es la diferencia entre el tercer y primer cuartil) son consideradas 
#como valores atípicos potenciales por R. En otras palabras, todas las observaciones fuera del siguiente intervalo se considerarán como 
#valores atípicos potenciales:
# I = [q0.25-1.5*IQR; q0.75+1.5*IQR]


Q <- quantile(duration, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(duration)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
qqPlot(iqr)
#Eliminamos los valores extremos atipicos del conjunto de datos mediante el uso de la funcion subset
eliminated<- subset(outputData, duration > (Q[1] - 1.5*iqr) & duration < (Q[2]+1.5*iqr))

#Una vez elminados los valores extremos atipicos realizamos la prueba de normalidad de nuevo, obteniendo un
#valor de p-value mucho mayor, sin embargo todavia continua sigue siendo p-value < 0.05, es decir, no es una
#distribucion normal.
shapiro.test(eliminated$duration)

#Para calcular la asimetria utilizamos la funcion:
skewness(eliminated$duration)
#Al ser una asimetria positiva entonces vamos modelar los datos mediante una distribucion gamma 
(fit.gamma <- fitdist(eliminated$duration, distr = "gamma", method = "mle"))
fit.gamma
#Mediante summary calculamos los parametros de la distribucion gamma
summary(fit.gamma)
#Posteriormente modelamos la distribucion
plot(fit.gamma)

#Comprobando si es una distribución gamma
descdist(eliminated$duration, boot=1000)




#2 parte
#Duración del procesamiento
proccesTime <- read_csv("C:/Users/Esteban Quesada/Desktop/ProyectoMetodos/data2.csv")

#Diagrama de dispersión
plot(x = proccesTime$Seconds, 
     y = proccesTime$`Processing (s)`, 
     xlab="Duracion de cancion en segundos", 
     ylab="Segundos de Procesamiento")
abline(lsfit(proccesTime$Seconds,proccesTime$`Processing (s)`))
#Analizando el diagrama de dispersion de izquierda a derecha podemos notar ciertos detalles, que no siempre se cumplen pero en algunos
#puntos sí se puede considerar como cierto, lo cual es que entre más dure la canción en segundos más tiempo se tarda procesando la misma
#Hay que decir que se pueden observar ciertas canciones que duran menos que otras y aún así tardan más tiempo procesando que
#canciones que duran más, por lo que son excepciones al primer punto mencionado. Sin embargo como se puede notar, la cancion
#que dura más tiempo es la que tarda mas tiempo en procesarse.
#pairs(proccesTime$`Processing (s)` ~ proccesTime$Seconds)

lmProcessTime = lm(`Processing (s)`~Seconds, data = proccesTime) #Create the linear regression
summary(lmProcessTime) #Review the results

#Calculo de la correlación
cor(proccesTime$`Processing (s)`,proccesTime$Seconds)
#Calculo de coeficiente de determinacion R2
(R2 = cor(proccesTime$`Processing (s)`,proccesTime$Seconds)^2)
#La duracion de la cancion es un buen predictor de la duracion del procesamiento de la cancion
#esto debido a que suponemos que entre mas dure la cancion mas tiempo tomara procesarla, una vez en primera
#instancia al realizar el diagrama de dispersion notamos como la suposicion parecia 
#ser verdadera, donde en el grafico la relacion parecia ser positiva, sin embargo para comprobarlo entonces 
#calculamos el coeficiente de determinacion R2, obteniendo como resultado 0.9041124, esto quiere decir que es un modelo
#cuyas estimaciones se ajustan muy bien a la variable real. Aunque tecnicamente no sea del todo correcto
#se puede decir que el modelo explica en un 90.4% a la variable real. 
#A la hora de interpretar el coeficiente de determinacion R2 de forma mas tecnica, se puede decir que cuando
#este arrojaba como resultado -1, se indicaba una fuerte correlacion negativa, que significaba que cada vez que la x
#incrementaba la y decrecia. POr otra parte, si era 0, significaba la no existencia de una asociacion entre dos
#variables "x" y "y". Pero por ultimo y la que mas nos interesa es el caso donde 1 indica un fuerte correlacion positiva
#lo cual significa que y incrementa con x, es decir, en el caso actual, entre mas duren las canciones
#mas sera el tiempo que se tarde procesandolas.

#Para estimar la pendiente y la intercepcion volvemos a revisar esta formula que nos lo da
summary(lmProcessTime) #Review the results
#Segun los resultados la intercepcion es igual a -2.77785, mientras que la pendiente es igual a 0.09772



#Una forma de probar que la M no se vuelve nula, es con el hecho de que si "y" es igual para todo "x" entonces M definitivamente es nulo, es decir M = 0
#por lo tanto, podemos generar ciertos x y comprobar si la y es distinta para cada uno de ellos:
M<- 0.09772 #pendiente
B<- -2.77785 #intercepcion

#con x igual a 302.4766
(y = M*302.4766 + B )

#con x igual a 430.0008
(y = M*430.0008 + B )


#Luego para demostrar que B no es igual a 0 recordemos que si y(x = 0) no es cero, entonces b no es 0, debido a que b = y(0)
(y0 <- M*0 + B)
#Con y(x = 0) obtenemos un valor que no es cero, se obtiene -2.77785, por lo tanto al b ser igual a y(0) se dice que B no es nulo





#MONTECARLO
#Variables del modelo lineal de la parte 2
M<- 0.09772 #pendiente
B<- -2.77785 #intercepcion
(N <- 0.25*(qnorm(0.95)/0.05)^2)

#Locales
ventasN <- rep(0,N)

#Calcular los precios de las canciones:
tiempoDeProcesamientoTotal <- 0
cancionesExceden6 <- 0
totalCanciones <- 0
dineroExcedente6 <- 0
dineroTotalGenerado <- 0

i = 1
while(i < N){
  contadorprimero = i
  ventasPorCadaN = 0
  tiempoDeProcesamientoTotal = 0
  while(tiempoDeProcesamientoTotal < 31536000){
    totalCanciones <- totalCanciones + 1
    (duracionCancion <- rgamma(1, shape = 5.86283427, rate = 0.02288505))
    (tiempoDeProcesamiento = M*duracionCancion+B  )
    tiempoDeProcesamientoTotal <- tiempoDeProcesamientoTotal + tiempoDeProcesamiento
    precioCancion <- 0
    if(duracionCancion < 360){
      precioCancion <- 0.99
      ventasPorCadaN = ventasPorCadaN + 0.99
    }else{
      cancionesExceden6 <- cancionesExceden6 + 1
      duracionParcial <- duracionCancion
      precioTotalAcumulado <- 0
      while(duracionParcial > 0){
        duracionParcial <- (duracionParcial-360)
        precioTotalAcumulado <- (precioTotalAcumulado + 0.99)
      }
      precioCancion<- precioTotalAcumulado
      ventasPorCadaN = ventasPorCadaN + precioCancion
      dineroExcedente6 <- dineroExcedente6 + precioCancion
    }
    
    dineroTotalGenerado <- dineroTotalGenerado + precioCancion      
  }
  ventasN[i] = ventasPorCadaN
  i <- i+1
  
}
(ventasN)

#Valor esperado de las ventas:
mean(ventasN)
#Error estandar de las ventas:
std.error(ventasN)
(sd(ventasN)/sqrt(N)) 

#fraccion de ventas provenientes de ventas de canciones que exceden los 6 minutos
(fraccionVentasEsperada <- dineroExcedente6/dineroTotalGenerado)
#Fraccion esperada de canciones que exceden los 6 minutos
(fraccionCancionesEsperada <- cancionesExceden6/totalCanciones)

#Fracción esperada de ganancias despues de cobro de comision del 30%
ganancias <- rep(0,N)
for(i in 1:N){
  if(ganancias[i] > 1000000){
    ganancias[i] <- ventasN[i]- (ventasN[i]*0.30)         
  }
  if(ganancias[i] < 1000000){
    ganancias[i] <- ventasN[i]- (ventasN[i]*0.15)         
  }
  
}
mean(ganancias)