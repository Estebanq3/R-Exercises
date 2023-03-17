#1
#a
(1- pbinom(0,size = 1, prob = 1/30))

#b
(1-pbinom(1,size = 6, prob = 1/30))

(1-pbinom(1,size = 5, prob = 1/120))

#d
#Importante aclaración, se toma en cuenta que 0 sea un número par, entonces se cuenta la probabilidad de
#que cantidad de llamadas igual a 0, sea par y se tome en cuenta
(pbinom(0,size = 2, prob = 1/30) +(pbinom(2,size = 2, prob = 1/30)-pbinom(1,size = 2, prob = 1/30)))

#Caso de que 0 no sea considerado como una cantidad par de llamadas válida
(1-pbinom(1,size = 2, prob = 1/30))

#2
#a
(ppois(3, (1/30)*5))

( ppois(3, (1/30)*1/3) - ppois(1, (1/30)*1/3) )

#c
#Diferente forma de calcular la probabilidad, una con gamma otra con ppois
#Forma 1: 
pgamma(20,shape = 4,rate = 1/30)
#Forma 2 : 
1 - ppois(3,(1/30)*20)

