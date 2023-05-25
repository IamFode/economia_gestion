
#libraria para foreign
library(hexView)
library(forecast)
library(lmtest)
library(tseries)

df=readEViews("data/dolar_euro.wf1")

tc_d_euro = ts(df$TC_D_EURO,frequency = 1,start = c(1999,1))


# a) Analizar la estacionariedad del tipo de cambio Dólar/Euro. 
plot(tc_d_euro) #No es estacionaria


# Correlograma
acf(tc_d_euro) # Ya que no decrece rapido no es estacionaria
pacf(tc_d_euro)


# Test de Dicky fouller
adf.test(tc_d_euro) # Es no estacionaria ya que no se rechaza la H_0
# tiene raíz unitaria


#Diferencia
dtc_d_euro = diff(tc_d_euro)

plot(dtc_d_euro)
acf(dtc_d_euro)
pacf(tc_d_euro)
adf.test(dtc_d_euro)


#b)Especificar y estimar un modelo ARMA para predecir el cambio mensual del
#tipo de cambio (la predicción se efectúa para julio de 2012 excluyendo esta
#observación del periodo de estimación). 
# Auto arima
arima=auto.arima(dtc_d_euro,trace = TRUE)
#Ljung-Box H_0: los residuos son iid,
Box.test(arima$residuals,lag=30,type="Ljung-Box")
#c) Efectuar la predicción para julio de 2012 (predicción estática). 

