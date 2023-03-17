#II Parte del laboratorio

library(readr)
hops <- read_table2("D:/UCR/Actual VI Semestre/Desempeño y Experimentación/LaboratorioRegresión/hops.txt")
View(hops)


summary(hops)

hops

head(hops)

tail(hops)

par(mfrow = c(1, 2))
plot(yield ~ temperature, data = hops)
plot(yield ~ sunshine, data = hops)


plot(yield ~ temperature, data = hops, type = "n")
points(yield ~ temperature, data = subset(hops, variety == "fuggle"), pch = "F")
points(yield ~ temperature, data = subset(hops, variety == "hallertau"), pch = "H")
points(yield ~ temperature, data = subset(hops, variety == "saaz"), pch = "S")
legend("topright", legend = c("fuggle", "hallertau", "saaz"), pch = c("F", "H", "S"))


fm1 <- lm(yield ~ temperature + sunshine + fuggle.id + hallertau.id, data = hops)
summary(fm1)


fm2 <- lm(yield ~ temperature + sunshine, data = hops)


anova(fm2, fm1)


par(mfrow = c(1, 2))
plot(fm1)


par(mfrow = c(1, 2))

plot(resid(fm1) ~ hops$temperature)
abline(h = 0, lty = "dotted")

plot(resid(fm1) ~ hops$sunshine)
abline(h = 0, lty = "dotted")



