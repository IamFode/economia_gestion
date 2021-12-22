*** a) 
reg yield  cocoaland  fertilizer_50kg
imtest, white

*** b)
ivregress 2sls plotcount totland hafullprod hybridsl (logtotprice = priceha yield nnoboa_days), robust

rivtest, ci

estat endog

reg totland nnoboa_days

reg logtotprice totland

reg logtotprice nnoboa_days


*-------------*
** estimamos el modelo por MCO
reg logtotprice totland plotcount hafullprod hybridsl

** guardar la regresión con endogeneidad
estimates store con_endoge

** guardar los betas de la regresión con endogeneidad
matrix A = e(b)

** guardamos la varianza del regresor con endogeneidad
matrix a = e(V)

** veremos multicolinealidad
estat vif
*Toma las variables explicativas, si el valor del VIF es mayor a 10 entonces hay una multicolinealidad fuerte. En este caso no se tiene multicolinealidad. y por lo tanto cumple el supuesto de Gauss-Markov

** Necesitamos modelar el problema utilizando una variable instrumental.
ivregress 2sls plotcount totland hafullprod hybridsl (logtotprice = priceha yield nnoboa_days), robust

** Será un buen instrumento nnoboa_days de totland
** para que sea un buen isntrumento necesitamos que se cumpla con el principio de relevancia
twoway scatter totland nnoboa_days

* pareciera tener una relación. Es verdad que otras variables tienen a tener mejor relación pero en este caso decidimos tomar nnoboa_days.

pwcorr totland nnoboa_days

reg totland nnoboa_days

* a pesar de la poca correlación observada vemos que podría ser una buena variable instrumental

** Pasamos a la metodología corriendo una regresión con el regresor endógeno cmo variable dependiente en una primera etapa.
*** primera etapa
reg totland nnoboa_days
* Se ve que nnoboa_days es significativo
* Ahora si este instrumento nnoboa_days sirve para pronosticar al regresor endógeno

** Realizamos el pronostico del regresor endógeno
predict totlandhat, xb

** Correr la regresión con la variable pronosticada.



*----------------------*
ivregress 2sls plotcount totland hafullprod hybridsl (logtotprice = priceha yield nnoboa_days), robust




*-------------------------------------------------------------------------------------------------------------------------------------------*
*-------------------------------------------------------------------------------------------------------------------------------------------*
*-------------------------------------------------------------------------------------------------------------------------------------------*



*** c) Una estimación por máxima verosimilitud en la cual la variable depedendiente sea dicotómica. (regresores a elección del grupo, siempre y cuando los estimaodres sean estadísticamente significativos). *** 

** Las estimaciones En un modelo probit o logit ya se encuentan construidas por máxima verosimilitud. Donde  estaremos buscando primeramente si las varaibles independientes son significativas. Para luego entender si la variable dependiente tendrá un cambio de probabilidad entre 0 y 1, es decir, veremos si afecta postiva o negativamente a la probabilidad de que la variable dependiente sea 1 ó en su defecto se verá un efecto marginal viendo en cuanto contribuye que la probabilidad sea 1. **

codebook fert_yn
*Vemos que 1622 cultivos de cacao no usan fertilizantes y 869 si las usan. Se tiene 29 29 valores ausente.

logit fert_yn fertilizer_50kg plotcount 
* fert_yn = fertilizante usado (S/N)
* plotcount = número de parcelas de cacao cultivadas

*Ahora veremos el resumen de todos los resultados de la última estimación*
eretur list
** Donde vemos que el valor de la máximaverosimilitud es -59.4331...

*Luego aproximamos los valores para ver la distancia de máximaverosimilitud
display 2*(e(ll)-e(ll_0))
** Donde nos da un valor de  3036.5245  que será la distancia del estadistico por máximaverosimilitud en la regresión logística.

* Ahora veremos como se predicen los valores para la variable dicotómica
estat class
** Donde nos menciona que se a clasificado a las 2449 observaciones con una capacidad de predicción al 99.80%

* Veamos ahora los efectos marginales y luego daremos una interpretación al mismo.
margins, dydx(*)
** donde nos muestra que por una bolsa de fertilizante que se tenga la probabilidad de que se use el fertilizante aumentará en un 6%. 

** Luego la probabilidad de que use fertilizante debido a que se tiene bolsas de fertilizante es muy alto. 
** Y la probabilidad de que se use fertilizante debido al número de parcelas de cacao cultivadas es de aproximadamente .49 mayor a que no se use algún fertilizante.


*Por último pasemos a los Odds ratios. 
logit fert_yn fertilizer_50kg plotcount, or
** Donde podemos apostar 2 a 1 que si se tiene parcelas cultivadas de cacao entonces si se utilizará fertilizante.