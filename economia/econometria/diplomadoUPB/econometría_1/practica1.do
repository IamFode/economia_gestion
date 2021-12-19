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
* Respuesta-  Para que la proximidad a la universidad sirva como un instrumento legítimo para la educación completa, debe afectar las decisiones individuales de escolarización, pero no tiene un efecto directo sobre los ingresos. Hay al menos tres razones por las que los hombres que crecieron cerca de una universidad pueden tener ingresos más altos que otros hombres, controlando la educación, la información geográfica y los antecedentes de los padres. Primero, las familias que ponen un fuerte énfasis en la educación pueden optar por vivir cerca de una universidad. Los niños de estas familias pueden tener una mayor "capacidad" o pueden estar más motivados para lograr el éxito en el mercado laboral. Cualquiera de los dos factores podría inducir una correlación positiva entre la proximidad de la universidad y los determinantes no observados de los salarios. En segundo lugar, la presencia de una universidad puede estar asociada con una mayor calidad escolar en las escuelas primarias y secundarias cercanas. Card y Krueger (1992) muestran que una mayor calidad escolar se asocia con mejores ingresos. Finalmente, si sólo hay indicadores imperfectos disponibles para el lugar de residencia en 1976, y si los hombres que crecieron en áreas con una universidad cercana tienden a vivir en áreas con salarios más altos, entonces la proximidad de la universidad puede estar correlacionada con áreas geográficas no observadas.

*** Corra la regresión con la forma "reducida" (corra MCO como en el paso anterior pero reemplazando "educ" con "nearc4") ***
reg lwage nearc4 exper expersq smsa black married reg661-reg668

*** Existe una relación significativa entre proximidad al campus y el salario? ***
*Respuesta.- Podría existir una relación significativa debido a que el p-value es menor a 0.05.

*** Qué necesita asumir para que variables instrumentales sea una estrategia válida? ***

*** Usted cree en ese supuesto? por qué o por qué no? ***

*** Añada la variable "libcrd14" a la regresión en forma reducida ***
reg lwage nearc4 libcrd14 exper expersq smsa black married reg661-reg668
 
*** Cómo pudo ayudar ésta variable a los requisitos del supuesto ? ***
*Respuesta.- A priori sabemos que la variable independiente "libcrd14" es significativa. Luego vemos que el aumento de la variable independiente mencionada ayudará de una manera eficiente al regresor  o variable explicada.*

*** Algunas observaciones tienen información iq (coeficiente intelectual), educación de la madre y educación del padre. Corra una regresión con cada una de esas variables y "nearc4" ***
reg nearc4 fatheduc motheduc


*** Que nos dice esa regresión acerca de la validez de la forma reducida, sin incluir esas variables? Notese que no podemos incluir éstas variables en la regresión principal porque no tenemos datos para todas las observaciones ***
* Respuesta.- Los coeficientes de forma reducida confirman que los efectos de vivir cerca de una universidad son mayores para los hombres con padres con poca educación.

*** Corra la regresión de la primera etapa. Recuerde que necesita tener los mismos controles que la forma reducida. ***

*** Tiene un problema de debilidad de instrumentos? como lo sabe? ***

*** Basado en los resultados de la primera etapa y de la forma reducida, cual debería ser el coeficiente cuando use variables instrumentales? Cómo lo sabe? ***

*** Corra una regresión de 2SLS con "educ" como la variable endogena y "nearc4" como el isntrumento ***

*** Interprete todos los resultados ***
* Respuesta.- Los resultados muestra que los hombres que crecieron en áreas con una universidad cercana de 4 años tienen una educación significativamente más alta y ganancias significativamente más altas. Estos efectos se concentran entre los hombres con padres con poca educación, hombres que de otro modo dejarían de estudiar en niveles relativamente bajos. Las estimaciones de las variables instrumentales implícitas de la ganancia de ingresos por año de educación adicional (10-14%) están sustancialmente por encima de las ganancias de ingresos estimadas por un procedimiento convencional de mínimos cuadrados ordinarios (7,3%). Estas inferencias son robustas a cambios menores en la especificación, incluida la adición de seores de prueba medidos al modelo de ingresos y cambios en la definición de proximidad a universidades. Sin embargo, se basan en el supuesto restrictivo de que vivir cerca de una universidad no tiene ningún efecto sobre los ingresos, aparte del efecto a través de la educación. Para probar esta suposición, utilizo el hecho de que la proximidad de la universidad tiene un mayor impacto en las opciones de educación de los hombres con antecedentes familiares más pobres. Por lo tanto, una interacción de la proximidad a la universidad y los antecedentes familiares bajos se puede utilizar como una variable instrumental para la escolaridad observada incluso en modelos de ingresos que incluyen un efecto directo de proximidad a la universidad. Los resultados de esta prueba dan lugar a estimaciones en el mismo rango que las estimaciones de variables instrumentales más simples basadas solo en la proximidad de la universidad. Si bien ninguna de las estimaciones de las variables instrumentales del retorno a la educación es muy precisa, todas apuntan hacia retornos relativamente altos a la escolarización para los hijos de padres con poca educación. · Este patrón es coherente con un modelo económico simple de escolarización endógena en el que el acceso diferencial a los fondos conduce a una relativa falta de inversión en la escolarización entre los niños de familias de menores ingresos.



* ALTERNATIVAS *

*** la tasa "verdadera" de retorno a la educación varía entre la población, y que el aumento en la educación asociado con la proximidad a la universidad ocurre para individuos con tasas relativamente altas de retorno a la educación.

*** Cuando la proximidad a la universidad se toma como un determinante exógeno de la escolaridad, las estimaciones de las variables instrumentales implícitas del retorno a la educación son entre un 25 y un 60 por ciento más altas que las estimaciones correspondientes de mínimos cuadrados ordinarios

***  La presencia de una universidad cercana puede ser una variable. Los estudiantes que crecen en un área sin una universidad enfrentan un costo más alto de educación universitaria, ya que se excluye la opción de vivir en casa.  Uno esperaría que este mayor costo redujera Inversiones en educación superior, al menos entre los niños de familias de ingresos relativamente bajos.

*** En cada cuartil, el nivel medio de educación es más alto para aquellos que crecieron cerca de una universidad. Para los hombres en los tres cuartiles de educación más altos previstos, el efecto de la proximidad a la universidad es modesto (0,2 a 0,4 años). Sin embargo, para los hombres en el cuartil más bajo, la diferencia en la educación media es de 1,1 años. Como era de esperar, la presencia de una universidad cercana tiene su efecto más fuerte en los hombres con menor propensión a continuar su educación.

*** Crecer cerca de una universidad tiene un fuerte efecto positivo tanto en la educación (0,32 a 0,38 años de escolaridad) como en los ingresos (4,2 a 4,8 por ciento). El uso puntual de la proximidad a la universidad como un determinante exógeno de la escolaridad produce estimaciones IV del retorno a la educación en el rango de 0,12 a 0,14

*** En las ecuaciones de forma reducida, la presencia de una universidad cercana de 2 años tiene pequeños efectos positivos sobre la escolaridad y los ingresos (se incluya o no un indicador de proximidad a una universidad de 4 años)