*** clase 4 16-12-21 ***

sysuse dir
sysuse auto.dta

*** Analizar mi base de datos ***
browse

codebook foreign

*** cambio de variable para escribir codigo ***
global y foreign
global vi mpg length displacement

*** regresion ***
reg $y $vi
*
*** es mejor utilizar un modelo logit = cuando la variable dependiente es dicotomica ***
logit $y $vi
*
*** construir una lista de todas las variables mas importantes del modelo ***
ereturn list

*** aproximar por maxiama verosimilitud ***
display 2*(e(ll)-e(ll_0))

display 1-2*(e(ll)-e(ll_0))   

*** Como se predicen los valores para la variable dicotómica ***
estat class

*** programar maxima verosimilitud ***
program define mv
args lnf Xb
quietly replace `lnf'= -ln(1+exp(-`Xb')) if $ML_y1==1
quietly replace `lnf'= -`Xb'-ln(1+exp(-`Xb')) if $ML_y1==0
end
ml model lf mv (foreign= mpg length displacement)
ml maximize
