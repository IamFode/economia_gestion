
use http://www.stata.com/data/jwooldridge/eacsap/mroz.dta, clear

*** Para tomar en cuenta solo a las mujeres que trabajaron en 1975 ***
keep if inlf==1

reg lwage educ exper expersq

*** Analizamos heterocedasticidad mediante white ***
imtest, white 

** De otra manea
estat imtest

*** Graficando la distribución ***
kdensity lwage

*** Test Breusch Pagan Godfrey ***
estat hettest, normal
** parece que el modelo presenta heterocedasticidad tipo D **


