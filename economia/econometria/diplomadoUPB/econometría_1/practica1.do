*** Nombres ***

*** Práctica 1 ***

*** Base de datos 
use http://www.stata.com/data/jwooldridge/eacsap/card.dta, clear

*** Corra una regresión de "lwage" con "educ". También incluya los siguientes controles: exper, expersq, smsa, black, reg661-reg668
reg lwage educ exper expersq smsa black married reg661-reg668

*** Interprete el coeficiente de la variable "educ". Porqué ésta regresión no puede tener una interpretación causal? ***
*Respuesta.- lwage aumenta en 7.2% por cada año más de educación. Esto es lo que se llama rendimiento de un año más de educación. No podria tener una interpretación cuasal porque puede que no capte por completo la no linealidad de la relación entre salarios y escolaridad.

*** Cree usted que existe algún sesgo en los coeficientes? Hacia que lado es el sesgo? Por qúé? ***
* Respuesta.- Si podría haber debido a que podríamos estar omitiendo alguna variable explicativa, en neustro caso la variable * 

*** Explique el razonamiento de usar la proximidad al campus como un instrumento del nivel de educación que una persona recibe. Cree usted que éste instrumento cumple con las condiciones básicas para usar variables instrumentales ? ***

*** Corra la regresión con la forma "reducida" (corra MCO como en el paso anterior pero reemplazando "educ" con "nearc4") ***
reg lwage nearc4 exper expersq smsa black married reg661-reg668

*** Existe una relación significativa entre proximidad al campus y el salario? ***
*Respuesta.- Podría existir una relación significativa debido a que el p-value es menor a 0.05.

*** Qué necesita asumir para que variables instrumentales sea una estrategia válida? ***

*** Usted cree en ese supuesto ? por qué o por qué no? ***

*** Añada la variable "libcrd14" a la regresión en forma reducida ***
reg lwage nearc4 libcrd14 exper expersq smsa black married reg661-reg668
 
*** Cómo pudo ayudar ésta variable a los requisitos del supuesto ? ***
*Respuesta.- A priori sabemos que la variable independiente "libcrd14" es significativa. Luego vemos que el aumento de la variable independiente mencionada ayudará de una manera eficiente al regresor  o variable explicada.*

*** Algunas observaciones tienen información iq (coeficiente intelectual), educación de la madre y educación del padre. Corra una regresión con cada una de esas variables y "nearc4" ***
reg nearc4 fatheduc motheduc


*** Que nos dice esa regresión acerca de la validez de la forma reducida, sin incluir esas variables? Notese que no podemos incluir éstas variables en la regresión principal porque no tenemos datos para todas las observaciones ***


*** Corra la regresión de la primera etapa. Recuerde que necesita tener los mismos controles que la forma reducida. ***

*** Tiene un problema de debilidad de instrumentos? como lo sabe? ***

*** Basado en los resultados de la primera etapa y de la forma reducida, cual debería ser el coeficiente cuando use variables instrumentales? Cómo lo sabe? ***

*** Corra una regresión de 2SLS con "educ" como la variable endogena y "nearc4" como el isntrumento ***

*** Interprete todos los resultados ***



