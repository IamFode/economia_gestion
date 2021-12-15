*** clase 2 , 14-12-21 ***

*** importa base de datos ***
clear all
use http://www.stata-press.com/data/r15/hsng, clear

*** Se analizará el ingreso y de que dependerá éste ingreso ***

*** descripción ***
describe

*** analizaremos el ingreso (rent) ***
** variable independiente hsngval = valor del hogar o casa **
** variable independiente pcturban = % urbano lugar donde vive
sum rent hsngval pcturban

*** construimos la matriz de correlaciones ***
pwcorr rent hsngval pcturban 

*** Estimamos el modelo por MCO ***
reg rent hsngval pcturban

*** guardamos la regresión con endogeneidad ***
estimates store con_endo

*** Le pido a stata que guarde los betas ***
matrix A = e(b)

*** guardamos la varianza ***
matrix a = e(V)

*** Veremos multicolinialidad *** proyección lineal de una varibal y otra
** Si el valor del test vif > 10 existe multicolinialidad fuerte
** vif -> 1/(1-r^2)
estat vif

*** Necsitamos modelar este problema utilizando una variable instrumental ***
** Necesitamos alguna variabla que este fuertemente correlacionado con la varaible explicativa del modelo pero no este relacionada con el error **
* Será un buen istrumento faminc de hsngval *
* Para que sea un buen instrumento necesitamos que se cumpla para el principio de relevancia es decir, Cov(X,Z) != 0 *
twoway scatter hsngval faminc
*calcular la correlación
pwcorr hsngval faminc
* Si no es significativa no puede ser una variable instrumental
reg hsngval faminc

*** Aparentamente si parece un instrumento para tratar el problema de endogeneidad ***

*** Pasamos a metodología de variables instrumentales
** Corremos una regresión con el regresor endogeno como variable dependiente en una primera etapa **
* Primera etápa *
reg hsngval faminc pcturban
* faminc es significativo P>|t|

*** ahora este instrumento faminc, sirve para pronosticar al regresor endogeno
** Realizamos el pronostico del regresor endogeno hat(endogeno)**
predict hsngvalhat, xb

*** Correr la regresión con la variable pronosticada hsngvalhat ***
reg rent hsngvalhat pcturban
* Ya hemos solucionado el problema de endogeneidad, utilizando el isntrumento faminc *

*** Otra manera más corta es la siguiente ***
ivregress 2sls rent pcturban (hsngval=faminc), first 

estimates store sin_endo

matrix B = e(b)

matrix b = e(V)


***Aplicando  Test de endogeneidad de Hausman ***
hausman sin_endo con_endo

** Cómo investigador, se estará comprobando que el instrumento Z soluciona los problemas de endogeneidad si rechazo la Ho en el test de hausman **
estat endog

*** Podríamos ver si las otras variables podrían 
estat firststage



