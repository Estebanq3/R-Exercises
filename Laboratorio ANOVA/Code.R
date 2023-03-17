# install.packages("tidyverse")
install.packages('gplots')
library(tidyverse)
library("gplots")

# cargamos datos
my_data <- PlantGrowth

# Mostrar una muestra aleatoria
set.seed (420)
dplyr::sample_n(my_data, 10)

# Mostrar los niveles
levels(my_data$group)

my_data$group <- ordered(my_data$group,levels = c("ctrl", "trt1", "trt2"))
levels(my_data$group)


group_by(my_data, group)%>%
summarise(count = n(),mean = mean(weight, na.rm = TRUE),sd = sd(weight, na.rm = TRUE))

# Box plot
boxplot(weight ~ group, data = my_data,
        xlab = "Treatment", ylab = "Weight",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))
# plotmeans
library("gplots")
plotmeans(weight ~ group, data = my_data, frame = FALSE,
          xlab = "Treatment", ylab = "Weight",
          main="Mean Plot with 95% CI")


# Compute the analysis of variance
res.aov <- aov(weight ~ group, data = my_data)
# Summary of the analysis
summary(res.aov)

TukeyHSD (res.aov)


plot(res.aov, 1)




#ANOVA de dos vías
# install.packages("tidyverse") # ya todos lo deberían tener
library(tidyverse)
# Almacenar los datos en la variable my_data
my_data <- ToothGrowth

# Mostrar una muestra aleatoria
set.seed(667)
dplyr::sample_n (my_data, 10)


# Verificar la estructura
str(my_data)


# Convertir dosis como factor y recodificar los niveles
# como "D0.5", "D1", "D2"
my_data$dose <- factor (my_data$dose,
                        levels = c(0.5, 1, 2),
                        labels = c("D0.5", "D1", "D2"))

# Tablas de Frecuencias
table(my_data$supp, my_data$dose)

# Box plot with two factor variables
boxplot(len ~ supp * dose, data=my_data, frame = FALSE, 
        col = c("#00AFBB", "#E7B800"), ylab="Tooth Length")


# Two-way interaction plot
interaction.plot(x.factor = my_data$dose, trace.factor = my_data$supp, 
                 response = my_data$len, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Dose", ylab="Tooth Length",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))

res.aov2 <- aov(len ~ supp + dose, data = my_data)
summary(res.aov2)



# ANOVA de 2 vías con efecto de interacción
# Estas dos llamadas son equivalentes

# metodo 1
res.aov3 <- aov(len ~ supp * dose, data = my_data)
# metodo 2
res.aov3 <- aov(len ~ supp + dose + supp:dose, data = my_data)
# sumamry de resultados de ANOVA
summary(res.aov3)

group_by(my_data, supp, dose) %>%
  summarise(
    count = n(),
    mean = mean(len, na.rm = TRUE),
    sd = sd(len, na.rm = TRUE)
  )


TukeyHSD(res.aov3, which = "dose")

# Homogeneidad de variaciones
plot(res.aov3, 1)


