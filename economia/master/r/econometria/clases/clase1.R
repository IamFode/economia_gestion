# CLASE 1.
## Supuesto 1.1

library("readxl")
library("ggplot2")

data = read_excel("data/Datos Suposto1. 1 Exercicio 1.4 Dougherty (1).xls")

# Modelo lineal
lineal = lm(EMPLOY~GDP,data=data)
summary(lineal)

#Modelo inversa de GDB
inversa_DBP = lm(EMPLOY~I(1/GDP),data=data)
summary(inversa_DBP)
lines((1/data$GDP),fitted(inversa_DBP))

#Gráfica
ggplot(data,aes(GDP,EMPLOY))+
  geom_point() +
  geom_smooth(method = "lm", formula = y~I(1/x),color="red")+
  geom_smooth(method = "lm", formula = y~x,color="blue")+
  geom_hline(yintercept=0, color = "black")



