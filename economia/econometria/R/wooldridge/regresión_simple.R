# librarias y funciones
library(ggplot2)
library(wooldridge)


################################## CAPÍTULO 2 ##################################
########################### REGRESIÓN LINEAL SIMPLE ############################

############################## EJERCICIO 2.3 ###################################
# Sueldo de los directores generales (CEO) y rendimiento sobre le capital
y = ceosal1$salary
x = ceosal1$roe
summary(y)
summary(x)
summary(lm(y~x))
beta1(y,x)
beta0(y,x)
residual(y,x) # si es positivo predice un valor inferior al de y_i
ggplot(aes(x, y)) +
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
y = wage1$wage
x = wage1$educ
summary(lm(y~x))
sum(lm(y~x)$residuals)
beta1(y,x)
beta0(y,x)
# [2.30]
residual(y,x) # si es positivo predice un valor inferior al de y_i
#[2.31]
covmuestral(y,x)
# [2.32]
meanyhat(y,x)
mean(y)

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
# y = porcentaje de votos obtenidos por el candidato A
# x = porcentaje del total de los gastos de campaña atribuidos al candidato A
y <- vote1$voteA
x <- vote1$shareA
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


# SST
SST(y)

### varianza muestral de y SST/(n-1)
SST(y)/(length(y)-1)
var(y)

# SSE
SSE(y)

# SSR
SSR(y,x)

# SST = SSE +SSR
SSE(y) + SSR(y,x)


################################## Ejemplo 2.8 #################################
# En la regresión de salarios de CEO obtenemos
y = ceosal1$salary
x = ceosal1$roe

r2(y,x)
"podemos ver qué parte de la variación en el salario se explica realmente por el 
rendimiento del capital. La respuesta es no mucha ya que solo es explicada al
rededor del 1.3% Eso significa que el 98,7% de las variaciones salariales de 
estos directores ejecutivos quedan sin explicación"

################################## Ejemplo 2.9 #################################
y <- vote1$voteA
x <- vote1$shareA

r2(y,x)
"En la ecuación del resultado de la votación en (2.28), R2 = 0.856. Por lo tanto,
la participación de los gastos de campaña explica más del 85% de la variación en
los resultados electorales para esta muestra. Esta es una porción considerable."

################################# Ejemplo 2.10 #################################
y <- log(wage1$wage)
x <- wage1$educ
beta0(y,x)
beta1(y,x)
length(y)
r2(y,x)
"
el salario aumenta en un 8,3% por cada año adicional de educación.
"

################################# Ejemplo 2.11 #################################
y <- log(ceosal1$salary)
x <- log(ceosal1$sales)
beta1(y,x)
beta0(y,x)
length(y)
r2(y,x)
"
Significa que un 1% de incrementos de los ingresos de la empresa incrementa en
0.257% del sueldo de los CEOs.
"

################################## funciones ###################################
source("funciones.R")


