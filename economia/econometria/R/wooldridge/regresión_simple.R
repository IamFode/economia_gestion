# librarias y funciones
source("funciones.R")
library(ggplot2)

################################## CAPÍTULO 2 ##################################
########################### REGRESIÓN LINEAL SIMPLE ############################

############################## EJERCICIO 2.3 ###################################
# Sueldo de los directores generales (CEO) y rendimiento sobre le capital
load("~/git/ciencias_sociales/economia/econometria/R/wooldridge/data/ceosal1.RData")
y = data$salary
x = data$roe
summary(y)
summary(x)
summary(lm(y~x))
beta1(y,x)
beta0(y,x)
residual(y,x) # si es positivo predice un valor inferior al de y_i
ggplot(data = data, aes(roe, salary)) +
  geom_point( color = "black", size = 2) +
  geom_smooth(method = lm, formula = y~x, color = "blue") +
  xlab("Rendimiento sobre el capital (ROE) en %") +
  ylab("Salario en Dolares") +
  ggtitle("Sueldo de los directores generales (CEO) y rendimiento sobre el capital") 
sueldo <- function(roe){
  return(beta0(y,x)+beta1(y,x)*roe)
}
sueldo(30)

############################## EJERCICIO 2.4 ###################################
# Salario y educación
# y = salario en dolares por hora
# x = educación en años
load("~/git/ciencias_sociales/economia/econometria/R/wooldridge/data/wage1.RData")
y = data$wage
x = data$educ
summary(lm(y~x))
beta1(y,x)
beta0(y,x)
residual(y,x) # si es positivo predice un valor inferior al de y_i
ggplot(data = data, aes(educ, wage)) +
  geom_point( color = "black", size = 2) +
  geom_smooth(method = lm, formula = y~x, color = "blue") +
  xlab("Eduación en años") +
  ylab("Salario en dolares por hora") +
  ggtitle("Salario y educación") 
# un año adicional de educación hace que el salario por hora aumente en 46c/hora
salario <- function(edu){
  return(beta0(y,x)+beta1(y,x)*edu)
}
salario(0)
salario(8)

############################## EJERCICIO 2.5 ###################################
# Resultados de una votación y gastos de campaña
load("~/git/ciencias_sociales/economia/econometria/R/wooldridge/data/vote1.RData")
# y = porcentaje de votos obtenidos por el candidato A
# x = porcentaje del total de los gastos de campaña atribuidos al candidato A
y = data$voteA
x = data$shareA
summary(lm(y~x))
beta1(y,x)
beta0(y,x)
residual(y,x) # si es negativo predice un valor superior al de y_i
votos <- function(share){
  return(beta0(y,x)+beta1(y,x)*share)
}
votos(50)
" Si la cantidad gastada por el candidato A aumenta en un punto porcentual, él 
obtendrá medio punto porcentual más del total de los votos"
votos(60)

