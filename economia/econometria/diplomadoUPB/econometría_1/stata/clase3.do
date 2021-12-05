

clear
set more off
cd "D:\git\ciencias_sociales\economia\econometria\diplomadoUPB\econometría_1\stata\data"
capture log close 
use "hprice1.dta"
log using ramr.log, replace

*1. Describa las variables de las bases media, varianza, min, max, cantidad de 


sum

*2. 

des 
mdesc
*3. grafique  la distribución de todas las variables
histogram 

*regresion
reg price lotsize bdrms assess sqrft

*genere una serie

 
hist r, kdensity

generate r2 = r*r

*10
estat ovtest
