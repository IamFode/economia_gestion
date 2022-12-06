# Clase 2.
## Supuesto 1.2

# librerias
library("readxl")
library("ggplot2")

# Variable
## X = Tasa de crecimiento de los gastos en publicidad (%)
## Y = Tasa de crecimiento de las ventas (%)

#data
ruta="~/git/ciencias_sociales/economia/master/r/econometria/data/Datos Supuesto 1.2.xlsx"
data = read_excel(ruta)
data = data[,1:3]

# 1. Modelo Lineal 
lineal = lm(Y~X,data=data)
summary(lineal)
#Gráfica
ggplot(data,aes(X,Y))+
  geom_point() +
  geom_smooth(method = "lm", formula = y~x,color="red")


# 2. Modelo Cuadrático 
square = lm(Y~X+I(X^2),data = data)
summary(square)
ggplot(data,aes(X,Y))+
  geom_point() +
  geom_smooth(method = "lm", formula = y~x+I(x^2),color="red")


# 3. Modelo Modelo de elasticidad constante o modelo logarítmico.
log = lm(log(Y)~log(X),data=data)
summary(log)
ggplot(data,aes(X,Y))+
  geom_point() +
  geom_smooth(method = "lm", 
              formula = y~x,
              method.args=list(family=gaussian((link='log'))),
              color="red")


# 4. El modelo log-livel o modelo semilogaritmico
slog = lm(log(Y)~X,data=data)
summary(slog)
ggplot(data,aes(X,Y))+
  geom_point() +
  geom_smooth(method = "glm", 
              formula = y~x,
              method.args=list(family=gaussian((link='log'))),
              color="red")


# 5. El modelo nivel-log
nlog = lm(Y~log(X),data = data)
summary(nlog)
ggplot(data,aes(X,Y))+
  geom_point() +
  geom_smooth(method = "lm", formula = y~log(x),color="red")

################################################################################
################################################################################

# El R2 ajustado no es un buen indicador para comparar los modelos anteriores.
# Suma cuadrada de residuos tampoco es buen indicador de comparación.
# Entonces construiremos un R2 en términos equivalentes.


################# Modelo 3 con respecto a los modelos 1,2,5 ####################

# 3. Modelo de elasticidad constante o modelo logarítmico.
model = lm(log(Y)~log(X),data=data)
# Variable dependiente o regresando o variable endogena
y = data$Y
# Variables independiente o regresor o variable exogena
X = data$X
x = log(X)
#cantidad de variables explicativas o independientes
k=1
# Suma de cuadrados totales en términos equivalentes
scteq=0
for(i in 1:length(y)){
  scteq = scteq + (y[i]-mean(y))^2
}
scteq
#Suma de cuadrados en términos equivalentes de Y
sceeq = 0
for(i in 1:length(y)){
  equivY = exp(model$coefficients[1]+model$coefficients[2]*x)[i]
  sceeq = sceeq + ( y[i] - equivY )^2
}
sceeq
#t-k-1 k= números de variables independientes 
tk1 = length(y)-k-1
# t-1 
t1 = length(y)-1
# R2 ajustado en términos equivalentes
r2aeq = 1-(sceeq/tk1)/(scteq/t1)
r2aeq

################# Modelo 4 con respecto a los modelos 1,2,5 ####################
model = lm(log(Y)~X,data=data)
y = data$Y
X = data$X
x = X
k=1
scteq=0
for(i in 1:length(y)){
  scteq = scteq + (y[i]-mean(y))^2
}
scteq
sceeq4 = 0
for(i in 1:length(y)){
  equivY = exp(model$coefficients[1]+model$coefficients[2]*x[i])
  print(equivY)
  sceeq4 = sceeq4 + ( y[i] - equivY )^2
}
tk1 = length(y)-k-1
t1 = length(y)-1
r2aeq4 = 1-(sceeq4/tk1)/(scteq/t1)
r2aeq4

########################## COMPARANDO MODELOS ##################################
summary(lineal)$adj.r.squared
summary(square)$adj.r.squared
r2aeq
r2aeq4
summary(nlog)$adj.r.squared
