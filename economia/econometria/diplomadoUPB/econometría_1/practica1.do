*** Nombres ***

*** Práctica 1 ***

*** Base de datos 
use http://www.stata.com/data/jwooldridge/eacsap/card.dta, clear

*** Corra una regresión de "lwage" con "educ". También incluya los siguientes controles: exper, expersq, smsa, black, reg661-reg668
reg lwage educ exper expersq smsa black married reg661-reg668

*** Interprete el coeficiente de la variable "educ". Porqué ésta regresión no puede tener una interpretación causal? ***
*Respuesta.- lwage aumenta en 7.2% por cada año más de educación. Esto es lo que se llama rendimiento de un año más de educación. No podria tener una interpretación cuasal porque puede que no capte por completo la no linealidad de la relación entre salarios y escolaridad.

*** Cree usted que existe algún sesgo en los coeficientes? Hacia que lado es el sesgo? Por qúé? ***
Si podría haber debido a que podríamos estar omitiendo alguna variable explicativa, en neustro caso la variable 

*** 
