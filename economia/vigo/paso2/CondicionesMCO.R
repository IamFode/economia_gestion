#LIBRARIAS
library(lmtest)
library(MASS)

############## DATOS para Análisis Dwyer and Fisher (2009, fig 3) ###############

#1. LINEALIDAD
plot(P~M.Y,data=P_M.Y)
'"
Cumple con la Linealidad
"'

#2. Homocedasticidad
# Test de Breusch-Pagan (H_0: Los errores tienen varianza constante)
reg = lm(P~M.Y,data=P_M.Y,na.action = "na.omit")
bptest(reg)
'"
Se rechaza la hipótesis nula
Por lo que utilizamos la función `rml()` que es una función de la librería MASS 
que se utiliza para ajustar  modelos lineales generalizados (GLM) mediante el 
método de máxima verosimilitud restringida (REML). 
El método REML es una variante del método de máxima verosimilitud (ML) que se 
utiliza para ajustar modelos lineales cuando se tienen datos con estructuras de 
varianza heterogéneas o correlaciones entre las observaciones. 
El método REML ajusta el modelo utilizando solo la información de las diferencias 
entre las observaciones, lo que lo hace menos sensible a los valores atípicos y 
a las distribuciones no normales de los datos.
"'
GLM = glm(P~M.Y,data=P_M.Y,na.action = "na.omit")
summary(GLM)
 
#3. Normalidad de los errores. 
#Test Shapiro-Wilk (H_0: La distribución es normal)
qqnorm(GLM$residuals)
qqline(GLM$residuals)
'"
Ya que  los puntos están alineados cerca de la línea diagonal entonces se podría 
decir que los residuos se distribuyen aproximadamente acuerdo con una 
disttribución normal
"'


################################ REMOVE ########################################
rm(GLM)

