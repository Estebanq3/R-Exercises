install.packages(c("dplyr","ggplot2","gridExtra","tidyr","reshape","RColorBrewer","ggrepel","reshape2","Rcpp"))
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(reshape2)
library(RColorBrewer)
library(ggrepel)
library(Rcpp)

install.packages('Rcpp')


update.packages()


df= (read.csv(file.choose(), header=T, encoding = "UTF-8"))
attach(df)

summary(df)

df = tbl_df(df)
colnames(df)[25] <- "classification"
df$capture_rate <- as.numeric(df$capture_rate)
head(df)


df = select(df, name, classification, hp, weight_kg, 
            height_m, speed, attack, defense,
            sp_attack, sp_defense, type1, type2, 
            abilities, generation,is_legendary, 
            capture_rate) 
head(df)


density_hp <- ggplot(data=df, aes(hp)) + geom_density(col="white",fill="pink", alpha=0.8) + ggtitle("Density Plot of HP")
density_speed <- ggplot(data=df, aes(speed)) + geom_density(col="white", fill="darkorchid", alpha=0.8) + 
ggtitle("Diagrama densidad de caracteristicas de velocidad")
density_attack <- ggplot(data=df, aes(attack)) + geom_density(col="white", fill="orange", alpha=0.7) + ggtitle("Diagrama densidad de caracteristicas ofensivas")
density_defense <- ggplot(data=df, aes(defense)) + geom_density(col="white", fill="firebrick", alpha=0.7) + ggtitle("Diagrama densidad de caracteristicas defensivas")
density_height <- ggplot(data=df, aes(height_m)) + geom_density(col="white", fill="slateblue1", alpha=0.8) + ggtitle("Diagrama de densidad basado n altura (m) ")
density_weight <- ggplot(data=df, aes(weight_kg)) + geom_density(col="white", fill="mediumturquoise", alpha=0.8) + ggtitle("Diagrama densidad Peso (kg)")
grid.arrange(density_hp, density_speed, density_attack, density_defense, density_height, density_weight, ncol=2)


type_1_poke <- ggplot(data=df, aes(type1)) + geom_bar(aes(fill=..count..), alpha=0.8) + theme(axis.text.x = element_text(angle = 90, hjust = 0)) + ggtitle("Distribucion basados en Type-1") + coord_flip()
type_2_poke <- ggplot(data=df, aes(type2)) + geom_bar(aes(fill=..count..), alpha=0.8) + theme(axis.text.x = element_text(angle = 90, hjust = 0)) + ggtitle("Distribucion basados en Type-2") + coord_flip()
grid.arrange(type_1_poke, type_2_poke, ncol=2)


df %>%
filter(is_legendary==1) %>%
ggplot(aes(type1)) + geom_bar(aes(fill= ..count..)) + theme(axis.text.x = element_text(angle=90, hjust=0)) + ggtitle("Numero de Pokemon Legendarios usando Type-1")


ggplot(data=df, aes(attack, defense)) + geom_point(aes(color=is_legendary), alpha=0.8) + scale_color_gradient(low="darkblue", high="red") + ggtitle("Contraste Caracteristicas de Defensa vs Ataque") + 
  geom_label_repel(data=subset(df,attack > 150 | defense >150 | attack < 25), aes(label=name), 
                   box.padding = 0.35, point.padding = 0.5,
                   segment.color = 'grey50')


speed_attack_legendary <- ggplot(na.omit(df), aes(attack, speed)) + geom_point(aes(color=is_legendary)) + geom_label_repel(data=subset(df, (attack > 170 | attack < 50 & speed >150 | speed < 50) & is_legendary == 1 | speed > 145), aes(label=name), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') + geom_smooth(method = "lm")

weight_attack_legendary <- ggplot(na.omit(df), aes(attack, weight_kg)) + geom_point(aes(color=is_legendary)) + geom_label_repel(data=subset(df, (attack > 170 | attack < 50 | weight_kg > 650) & (is_legendary == 1)), aes(label=name), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') + geom_smooth(method = "lm")

height_attack_legendary <- ggplot(na.omit(df), aes(attack, height_m)) + geom_point(aes(color=is_legendary)) + geom_label_repel(data=subset(df, ((attack > 170 | attack < 50 | height_m > 7.5) & is_legendary == 1) | height_m > 5 & is_legendary == 0), aes(label=name), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') + geom_smooth(method = "lm")

hp_attack_legendary <- ggplot(na.omit(df), aes(attack, hp)) + geom_point(aes(color=is_legendary)) + geom_label_repel(data=subset(df, ((attack > 170 | hp > 190 | attack < 50) & is_legendary == 1) | hp >160), aes(label=name), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50')  + geom_smooth(method = "lm")

grid.arrange(speed_attack_legendary, weight_attack_legendary, height_attack_legendary, hp_attack_legendary, ncol = 2)


speed_defense_legendary <- ggplot(na.omit(df), aes(defense, speed)) + geom_point(aes(color=is_legendary)) + geom_label_repel(data=subset(df, (defense > 130 | defense < 50| speed > 140 | speed < 50) & is_legendary == 1), aes(label=name), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') + geom_smooth(method = "lm")

weight_defense_legendary <- ggplot(na.omit(df), aes(defense, weight_kg)) + geom_point(aes(color=is_legendary)) + geom_label_repel(data=subset(df, (defense > 160 | defense < 50 | weight_kg > 600) & is_legendary == 1), aes(label=name), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') + geom_smooth(method = "lm")

height_defense_legendary <- ggplot(na.omit(df), aes(defense, height_m)) + geom_point(aes(color=is_legendary)) + geom_label_repel(data=subset(df, (defense > 150 | defense < 50 | height_m > 6) & is_legendary == 1 | height_m >5 & is_legendary ==0), aes(label=name), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') + geom_smooth(method = "lm")

hp_defense_legendary <- ggplot(na.omit(df), aes(defense, hp)) + geom_point(aes(color=is_legendary)) + geom_label_repel(data=subset(df, (defense > 150 | defense < 50 | hp > 150) & is_legendary == 1 | hp > 200), aes(label=name), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') + geom_smooth(method = "lm")

grid.arrange(speed_defense_legendary, weight_defense_legendary, height_defense_legendary, hp_defense_legendary, ncol = 2)


box_plot_attr <- select(df, type1, is_legendary, hp, defense, attack, sp_attack, sp_defense, speed)

box_plot_attr_leg <- filter(box_plot_attr, is_legendary == 1)

box_plot_attr_nor <- filter(box_plot_attr, is_legendary == 0)
box_plot_attr_leg_long <- gather(box_plot_attr_leg, attribute, value, -c(type1, is_legendary))

box_plot_attr_nor_long <- gather(box_plot_attr_nor, attribute, value, -c(type1, is_legendary))

bp_leg <- ggplot(data = box_plot_attr_leg_long, aes(attribute, value)) + geom_boxplot(fill="green4") + ggtitle("Pokemon Legendario")

bp_nor <- ggplot(data = box_plot_attr_nor_long, aes(attribute, value)) + geom_boxplot(fill = "yellow2") + ggtitle("Pokemon Normal")

grid.arrange(bp_leg, bp_nor,ncol=2)


hmap_attr <- select(df, type1, is_legendary, hp, defense, attack, sp_attack, sp_defense, speed)
hmap_attr_leg <- filter(hmap_attr, is_legendary == 1)
hmap_attr_nor <- filter(hmap_attr, is_legendary == 0)
hmap_attr_leg <- group_by(hmap_attr_leg, type1)
hmap_attr_nor <- group_by(hmap_attr_nor, type1)
hmap_attr_leg <- summarise(hmap_attr_leg, hp=median(hp), attack=median(attack), defense=median(defense), sp_attack=median(sp_attack), sp_defense=median(sp_defense), speed=median(speed))

hmap_attr_leg_m <- melt(hmap_attr_leg)
hmap_attr_nor <- summarise(hmap_attr_nor, hp=median(hp), attack=median(attack), defense=median(defense), sp_attack=median(sp_attack), sp_defense=median(sp_defense), speed=median(speed))
hmap_attr_nor_m <- melt(hmap_attr_nor)
hm.palette <- colorRampPalette(rev(brewer.pal(5, 'RdYlBu')), space='Lab')

ggplot(data=hmap_attr_leg_m, aes(type1, variable)) + geom_tile(aes(fill=value)) + ggtitle("Legendary Pokemon: Type1 - Attribute") + scale_fill_gradientn(colours = hm.palette(100)) + theme(axis.text.x = element_text(angle=90, hjust=0)) + coord_equal()


hm.palette <- colorRampPalette(rev(brewer.pal(5, 'RdYlBu')), space='Lab')

ggplot(data=hmap_attr_nor_m, aes(type1, variable)) + geom_tile(aes(fill=value)) + ggtitle("Non Legendarios Atributo Tipo 1") + scale_fill_gradientn(colours = hm.palette(100)) + theme(axis.text.x = element_text(angle=90, hjust=0)) + coord_equal()


hmap_attr <- select(df, type1, is_legendary, hp, defense, attack, sp_attack, sp_defense, speed)
hmap_attr_leg <- filter(hmap_attr, is_legendary == 1)
hmap_attr_nor <- filter(hmap_attr, is_legendary == 0)
hmap_attr_leg <- group_by(hmap_attr_leg, type1)
hmap_attr_nor <- group_by(hmap_attr_nor, type1)
hmap_attr_leg <- summarise(hmap_attr_leg, hp=median(hp), attack=median(attack), defense=median(defense), sp_attack=median(sp_attack), sp_defense=median(sp_defense), speed=median(speed))
hmap_attr_nor <- summarise(hmap_attr_nor, hp=median(hp), attack=median(attack), defense=median(defense), sp_attack=median(sp_attack), sp_defense=median(sp_defense), speed=median(speed))
row.names(hmap_attr_leg) <- hmap_attr_leg$type1
hmap_attr_leg$type1 <- NULL
hmap_attr_leg$is_legendary <- NULL
row.names(hmap_attr_nor) <- hmap_attr_nor$type1
hmap_attr_nor$type1 <- NULL
hmap_attr_nor$is_legendary <- NULL
hmap_attr_leg_cor <- cor(hmap_attr_leg)
hmap_attr_leg_cor_m <- melt(hmap_attr_leg_cor)
hm.palette <- colorRampPalette(rev(brewer.pal(5, 'GnBu')), space='Lab')

ggplot(data=hmap_attr_leg_cor_m, aes(Var1, Var2)) + geom_tile(aes(fill=value)) + ggtitle("Correlación de Atributos - Legendarios") + scale_fill_gradientn(colours = hm.palette(100)) + coord_equal()


hmap_attr_nor_cor <- cor(hmap_attr_nor)
hmap_attr_nor_cor_m <- melt(hmap_attr_nor_cor)

hm.palette <- colorRampPalette(rev(brewer.pal(5, 'GnBu')), space='Lab')

ggplot(data=hmap_attr_nor_cor_m, aes(Var1, Var2)) + geom_tile(aes(fill=value)) + ggtitle("Correlación de Atributos - Noraml") + scale_fill_gradientn(colours = hm.palette(100)) + coord_equal()

#HAY QUE VOLVER A LEER LA TABLA
df= (read.csv(file.choose(), header=T, encoding = "UTF-8"))
attach(df)

df_fight_against <- select(df, type1, against_bug:against_water)
head(df_fight_against)


df_fight_against_g <- group_by(df_fight_against, type1)

df_fight_against_summ <- summarise(df_fight_against_g, 
                                   against_bug = median(against_bug), 
                                   against_dark = median(against_dark), 
                                   against_dragon = median(against_dragon),
                                   against_electric = median(against_electric),
                                   against_fairy = median(against_fairy),
                                   against_fight = median(against_fight),
                                   against_fire = median(against_fire),
                                   against_flying = median(against_flying),
                                   against_ghost = median(against_ghost),
                                   against_grass = median(against_grass),
                                   against_ground = median(against_ground),
                                   against_ice = median(against_ice), 
                                   against_normal = median(against_normal),
                                   against_poison = median(against_poison),
                                   against_psychic = median(against_psychic),
                                   against_rock = median(against_rock),
                                   against_steel = median(against_steel),
                                   against_water = median(against_water))

# Ploteamos el heatmap

df_fight_against_long <- melt(df_fight_against_summ)

hm.palette <- colorRampPalette(rev(brewer.pal(9, 'RdYlBu')), space='Lab')

ggplot(data=df_fight_against_long, aes(type1, variable)) + geom_tile(aes(fill=value)) + scale_fill_gradientn(colours = hm.palette(100)) + coord_equal() + theme(axis.text.x=element_text(angle=90, hjust=0)) + ggtitle("Efectividad de Diferentes Tipos Pokemon")


count(df, vars = type1,NULL)
count(df, vars = type2,NULL)






