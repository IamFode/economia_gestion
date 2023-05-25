

###############################################################################
#LIBRARY
library(readxl)
library(forecast)
library(zoo)
library(lmtest)
library(ggplot2)
library(tseries)
library(urca)
###############################################################################
#DATA
okun = read_excel("./data/datos_Ley Okun.xlsx", sheet = "Hoja3")
okun = okun[,2:3]
okun=ts(okun,start = c(1980,1),frequency = 1)
l.okun = log(okun)
###############################################################################
# u = desempleo
# y = producción
###############################################################################
# Cointegración.
ca.jo(l.okun)
