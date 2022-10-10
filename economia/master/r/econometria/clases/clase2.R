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


# 2. Modelo Cuadratico 
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


# El R2 ajustado no es un buen indicador para comparar los modelos anteriores.
# Suma cuadrada de residuos tampoco es buen indicador de comparación.

# Entonces construiremos un R2 en términos equivalentes.



