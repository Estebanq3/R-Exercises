library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(RColorBrewer)
library(reshape2)
library(mice)

# -----------------------------------------------------------------------
# Data Import

original_df = read.csv(file.choose(), header=T, encoding = "UTF-8")
attach(original_df)

md.pattern(original_df)


# -----------------------------------------------------------------------
# Extract Variables

df = select(original_df, gameId, gameDuraton, blueWins, redWins,
            blueFirstDragon, redFirstDragon, blueFirstBaron, redFirstBaron, blueFirstTower, redFirstTower,
            blueFirstBlood, redFirstBlood, blueFirstInhibitor, redFirstInhibitor, blueTotalGold,redTotalGold)

summary(df)

md.pattern(df)

# -----------------------------------------------------------------------
# Rename 

df = rename(df, gameDuration = gameDuraton)

# -----------------------------------------------------------------------
# Reshape Data
# Wide to Tall

df =  pivot_longer(df, cols = starts_with(c("blue","red")),
                   names_to = c("Team",".value"),
                   names_pattern = "(blue|red)(.*)"
)


# -----------------------------------------------------------------------
# Data labeling

wins.labs <- c("Derrota","Victoria")
names(wins.labs) <- c(0, 1)

team.labs <- c("Azul","Rojo")
names(team.labs) <- c("blue","red")


# -----------------------------------------------------------------------

#Primer drag髇
blueFirstWinners = filter(df,df$Team == "blue", df$FirstDragon == 1)
blueFirstLoosers = filter(df,df$Team == "blue", df$FirstDragon == 0)


#Dragones


sum(blueFirstLoosers$TotalGold)
p_dragones

ggplot(blueFirstWinners, aes_string(x = blueFirstWinners$TotalGold))


#Barones
p_barones <- ggplot(df, aes(x = BaronKills, fill = as.factor(Wins))) +
  scale_fill_manual(values=c("dodgerblue3","firebrick1"), labels = wins.labs, name = "Resultado") +
  geom_bar(position="dodge") +
  labs(x = "Eliminaciones de baron", y = "Cantidad equipos", title = "b) Comparaci贸n eliminaciones de barones por resultado de partida") +
  theme_gray()

#Inhibidores
p_inhibidores <- ggplot(df, aes(x = InhibitorKills, fill = as.factor(Wins))) +
  scale_fill_manual(values=c("dodgerblue3","firebrick1"), labels = wins.labs, name = "Resultado") +
  geom_bar(position="dodge") +
  labs(x = "Eliminaciones de inhibidores", y = "Cantidad equipos", title = "c) Comparaci贸n eliminaciones de inhibidores por resultado de partida") +
  theme_gray()

#Torres
p_torres <- ggplot(df, aes(x = TowerKills, fill = as.factor(Wins))) +
  scale_fill_manual(values=c("dodgerblue3","firebrick1"), labels = wins.labs, name = "Resultado") +
  geom_bar(position="dodge") +
  labs(x = "Torres destruidas", y = "Cantidad equipos", title = "d) Comparaci贸n torres destruidas por resultado de partida") +
  theme_gray()

#Eliminaciones
p_eliminaciones <- ggplot(df, aes(x = Kills, fill = as.factor(Wins))) +
  scale_fill_manual(values=c("dodgerblue3","firebrick1"), labels = wins.labs, name = "Resultado") +
  geom_bar(position="dodge") +
  labs(x = "Eliminaciones", y = "Cantidad equipos", title = "e) Comparaci贸n eliminaciones por resultado de partida") +
  theme_gray()

grid.arrange(p_dragones, p_barones, p_inhibidores, p_torres, p_eliminaciones, ncol=2)