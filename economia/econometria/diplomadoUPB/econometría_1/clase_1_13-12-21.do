*** Clase 1 13-12-21 ***

*** Base de datos wooldridge ***
use http://www.stata.com/data/jwooldridge/eacsap/mroz.dta, clear

*** variables exógenas: nwifeinc educ age kidsge6 
ivregress 2sls nwifeinc educ age kidsge6 (lwage = exper expersq fatheduc motheduc) if inlf == 1, robust

* todos los test sirven para la última regresión

*** Hacemos el test de variables instrumentales ***
rivtest, ci

*** test de hendogeneidad ***
estat endog

*** Replicamos prueba de relevancia (contrastable) ***
reg educ exper expersq 

*** 
reg lwage educ