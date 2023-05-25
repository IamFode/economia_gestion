# PIB LATINOAMERICA Y EL CARIBE 
# Producto interno bruto trimestral por tipo del gasto a precios corrientes
library(jsonlite) 
library(forecast)
library(tseries)  # carga el paquete tseries

download.file("https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/294/data?members=20183%2C511%2C512%2C513%2C514%2C21046%2C29150%2C29151%2C29152%2C29153%2C29154%2C29155%2C29156%2C29157%2C29158%2C29159%2C29160%2C29161%2C29162%2C29163%2C29164%2C29165%2C29166%2C29167%2C29168%2C29169%2C29170%2C29171%2C29172%2C29173%2C29174%2C29175%2C29176%2C29177%2C29178%2C29179%2C29180%2C29181%2C29182%2C29183%2C29184%2C29185%2C29186%2C29187%2C29188%2C29189%2C29190%2C29191%2C29192&lang=es&format=json&in=1&app=dashboardn", "archivo.json")
df = fromJSON("archivo.json")
pib = as.numeric(df$body$data$value)
st = ts(pib,start = c(1990,1),end = c(2021,4),frequency = 4)

logSt = log(st)
# aplica el test de Dickey-Fuller 
# H_0: La serie temporal tiene una raíz unitaria, 
# lo que significa que no es estacionari
adf.test(logSt)
# Ya que p-value es = 0.7688, no rechazamos la H_0, 
# y concluimos que es no estacionaria
DlogSt = diff(logSt)
plot(DlogSt)

#autocorrelación simple
acf(DlogSt,lag.max = 100)

#autocorrelacion parcial
pacf(DlogSt,lag.max = 100)

# MODELO AR (autoregresivo)
model = auto.arima(DlogSt)
model

# Ljung-Box
# H_0: los residuos son iid,
# por lo tanto no hay autocorrelación.
Box.test(model$residuals,lag=100,type="Ljung-Box")



