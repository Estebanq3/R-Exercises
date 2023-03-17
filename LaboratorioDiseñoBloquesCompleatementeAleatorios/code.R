#Las dos variables elegidas que se consideran poseen el mayor impacto en las capacidades de
#detección en el sistema de radar son: la cantidad de ruido de fondo (Ground) y
#tipo de filtro (Filter) en la pantalla del radar.

install.packages("ACSWR")

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

