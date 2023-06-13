
library(hexView)
library(forecast)
library(lmtest)
library(tseries)

# Definición de estacionaridad:
# los momentos de primer y segundo orden sean consntantes
# y que las correlaciones solo dependan del retardo temporal
# de las observaciones.
# eso quiere decir que no tiene una dependencia lineal.
# pero si de otro tipo.


# La volatilidad = varianza condicional = valor en riesgo.
# es mas importantes las distribuciones condicionadas que las marginales



# MODELOS GARCH para volatilidad. (Alta frecuencia)

# poca estructura en la media
# Si existe paseaos aleatorios o procesos AR de orden bajo
# y coeficiente pequeño
# Suele ocurrir:
# - Su distribución no es normal, con colas pesadas
# - las autoccrelaciones de sus cuadrados se observa una
#   fuerte dependencia
# La varianza condicional de los residuos no es constante,
# y aparecen rachas de mayor variabilidad seguidas por otras
# de menor variablilidad.

# surge para agrupar esos clusters de volatilidad
# Existen dependencia lineal cuadratica


# El modelo ARCH supone que la varianza condicional depende
# del pasado con estructura autorregresiva



# MODELOS GARCH LINEAL (BASICOS)

# PRACTICA 2.
df=readEViews("data/practica_ii.wf1",time.stamp = FALSE)
# rentimientos
rsp500=ts(df$R_SP500_OPEN)
#correlograma
acf=acf(rsp500)
acf$acf
#estimar
arima = auto.arima(rsp500)
arima

plot(rsp500)
hist(rsp500)
