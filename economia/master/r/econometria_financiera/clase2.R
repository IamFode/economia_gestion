
#libraria para foreign
library(hexView)
library(forecast)
library(lmtest)
library(tseries)

df=readEViews("data/dolar_euro.wf1")
tc_d_euro = ts(df$TC_D_EURO,frequency = 12,start = c(1999,1),end = c(2012,6))

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
acf=acf(dtc_d_euro,lag.max = 30)
pacf=pacf(tc_d_euro,lag.max = 30)
adf.test(dtc_d_euro)


#b)Especificar y estimar un modelo ARMA para predecir el cambio mensual del
#tipo de cambio (la predicción se efectúa para julio de 2012 excluyendo esta
#observación del periodo de estimación). 
# Auto arima
arima=auto.arima(dtc_d_euro,trace = TRUE)
acf(arima$residuals)# se tiene que analizar los residuos
#Ljung-Box H_0: los residuos son iid,
Box.test(arima$residuals,lag=30,type="Ljung-Box")

#c) Efectuar la predicción para julio de 2012 (predicción estática). 
pred = predict(arima,h.ahead=1)
# Horizonste de predicción: Julio 2022 h=1
#cambio actual:
diff(df$TC_D_EURO)
#Predicción:
pred$pred
#Varianza de la predicción
pred$se
#Densidad de predicción: N(-0.004896598,0.002959111^2)

#prob(aprecision) = phi(0-(0.00489)/(0.02959))
pnorm(pred$pred/pred$se,lower.tail = FALSE)
#Prob(deprecisiacion) = 1-prob(aprecision)
pnorm(pred$pred/pred$se,lower.tail = TRUE)
