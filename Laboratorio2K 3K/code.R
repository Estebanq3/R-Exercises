library(ACSWR)
library(readr)
data(bottling)
bottling

par(mfrow=c(2,2))
interaction.plot(bottling$Carbonation,bottling$Pressure,bottling$Deviation)
interaction.plot(bottling$Pressure,bottling$Speed,bottling$Deviation)
interaction.plot(bottling$Carbonation,bottling$Speed,bottling$Deviation) 
par(mfrow=c(1,1))

summary(bottling.aov <- aov(Deviation~.,bottling))
summary(bottling.aov <- aov(Deviation~.^2,bottling))
summary(bottling.aov <- aov(Deviation~.^3,bottling))
# Equivalente a realizar las interacciones manualmnete :)
summary(aov(Deviation~ Carbonation + Pressure + Speed+ (Carbonation*Pressure)+
              (Carbonation*Speed)+(Pressure*Speed)+(Carbonation*Speed*Pressure),data=bottling))

yield = read.csv("FactorialDesign.csv",
                 header = T,
                 encoding = "UTF-8")
yield <- read_csv("D:/UCR/Actual VI Semestre/Desempeño y Experimentación/Laboratorio2K 3K/FactorialDesign.csv")
View(yield)

# Debemos quitar el núnero de corrida para no crear interacciones espurias
yield <- yield[-1]
plot(yield)

par(mfrow=c(2,2))
interaction.plot(yield$Temp,yield$Conc,yield$Yield)
interaction.plot(yield$Temp,yield$Catalyst,yield$Yield)
interaction.plot(yield$Conc,yield$Catalyst,yield$Yield) 
par(mfrow=c(1,1))

# Modelamos los efectos principales
lm1 = lm(Yield ~ ., data = yield)
summary(lm1)

# Aquí se realizan o agregan las interacciones de 2 factores
lm2 = lm(Yield ~ .^2, data = yield)
summary(lm2)

#compare the two models
anova(lm1, lm2)

# En este paso realizamos las interacciones del modelo completo
lm3 = lm(Yield ~ .^3, data = yield)
summary(lm3)

# Ojo, los resultados del summary(lm3) son algo distintos verdad. Esto es porque en este experimento no tenemos réplicas, sino que se realizaron corridas con distinta Temp. Y no podemos analizar la significancia de de los coeficientes del modelo complete. Recuerden el problema del 3k - se pueden volver intratable

# Con base en modelos y resultados anteriores, sin embargo, podemos modelar usando las interacciones mas significativas
lm4 = lm(Yield ~ Conc + Temp*Catalyst, data = yield)
summary(lm4) 
