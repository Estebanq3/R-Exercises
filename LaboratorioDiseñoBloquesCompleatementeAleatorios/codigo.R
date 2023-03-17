install.packages("ACSWR")
library(ACSWR)

#Fase 1
data(hardness)

str(hardness)

head(hardness)

interaction.plot(hardness$Test_Coupon, hardness$Tip_Type, hardness$Hardness)

# Fase 2
boxplot(hardness$Hardness ~ hardness$Tip_Type)

boxplot(hardness$Hardness ~ hardness$Test_Coupon)


# Fase 3
anova(aov(hardness$Hardness ~ factor(hardness$Tip_Type) + factor(hardness$Test_Coupon)))

fit <- aov(hardness$Hardness ~ factor(hardness$Tip_Type) + factor(hardness$Test_Coupon))
TukeyHSD(fit,
         which= "factor(hardness$Tip_Type)",
         ordered=TRUE)

TukeyHSD(fit,
         which= "factor(hardness$Test_Coupon)",
         ordered=TRUE)

# Fase 4
summary(lm(hardness$Hardness ~ factor(hardness$Tip_Type) + factor(hardness$Test_Coupon)))

par(mfrow=c(2,2))
plot(lm(hardness$Hardness ~ factor(hardness$Tip_Type) + factor(hardness$Test_Coupon)))

#Prueba normalidad
shapiro.test(hardness$Hardness)



#2 Parte
library(ACSWR)
data("intensity")

#Fase 1
str(intensity)

interaction.plot(intensity$Ground,
                 intensity$Filter,
                 intensity$Intensity)
#Fase 2

# vemos la composición de los datos de intensity
head(intensity)

boxplot(intensity$Intensity ~ intensity$Ground)

boxplot(intensity$Intensity ~ intensity$Filter)


#Fase 3
anova(aov(intensity$Intensity ~ factor(intensity$Ground) + factor(intensity$Filter)))
fit <- aov(intensity$Intensity ~ factor(intensity$Ground) + factor(intensity$Filter))
TukeyHSD(fit,
         which= "factor(intensity$Ground)",
         ordered=TRUE)

TukeyHSD(fit,
         which= "factor(intensity$Filter)",
         ordered=TRUE)

summary(aov(intensity$Intensity ~ factor(intensity$Ground) + factor(intensity$Filter)))

#Fase 4

summary(lm(intensity$Intensity ~ factor(intensity$Ground) + factor(intensity$Filter)))

par(mfrow=c(2,2))
plot(lm(intensity$Intensity ~ factor(intensity$Ground) + factor(intensity$Filter)))

#Prueba normalidad

shapiro.test(intensity$Intensity)


