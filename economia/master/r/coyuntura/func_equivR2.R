########### Función de equivalencia de R2 ajustodo para una variable ###########

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
}