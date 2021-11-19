**** use "C:\Users\fode\Desktop\diplomadoUPB\stata\practicas\practica1\data\mroz.dta" ****

*** 1. Construya un archivo Do File con su nombre y su profesión que tenga como título ***

*Carla Sánchez - Ing. Financiera, Ana María Apaza - Contador Público, Ana Aliaga - Economista, Christian Paredes - Ing. Acústico, Alvaro Mendoza - Ing. Comercial*


*** 2. Cree una variable denominada OBS que marque el número de dato encuestado ***

g OBS = _n

  
*** 3. Describa la base de datos incluidas las estadísticas descriptivas de cada variable que no sea dicotómica ***

describe
codebook
sum

label variable whrs "Horas de trabajo de la mujer en 1975"
label variable kl6 "Número de niños menores de 6 años en el hogar"
label variable k618 "Número de niños entre 6 y 18 años en el hogar"
label variable wa "Edad"
label variable we "Nivel de educación de la mujer, en años"
label variable ww "Ganancias medias por hora de la mujer, en dólares de 1975"
label variable rpwg "Edad de la mujer informada en el momento de la entrevista de 1976"
label variable hhrs "Horas del marido trabajadas en 1975"
label variable ha "Edad del marido"
label variable he "Nivel educativo alcanzado por el marido, en años"
label variable hw "Salario del marido, en dólares de 1975"
label variable faminc "Renta familiar, en dólares de 1975. Esta variable se utiliza para construir la variable de ingreso sin trabajo."
label variable mtr "Tasa impositiva marginal que enfrenta la mujer. Tomado de tablas de impuestos federales publicadas (se excluyen los impuestos sobre la renta estatales y locales)"
label variable wmed "Nivel de educación de la madre de la mujer, en años"
label variable wfed "Nivel de educación del padre de la mujer, en años"
label variable un "Tasa de desempleo en el país de residencia, en puntos porcentuales"
label variable cit "variable si = 1 si vive en una gran ciudad, o no = 0"
label variable ax "años reales de experiencia previa en el mercado laboral de la esposa"
label variable prin "ingreso de la familia excluyendo las ganancias de la mujer."
label variable wa2 "edad al cuadrado"
label variable prin4 "PRIN*10-4"


*** 4. Cambie el nombre de las variables todas a minúscula, incluyendo en todas ellas etiquetas que expliquen el significado de cada una ***

rename OBS, lower


*** 5.  ¿Cuantas fueron las mujeres que trabajaron en 1975? *** 

count


*6. ¿Cuál es el menor ingreso de la familia excluyendo las ganancias de la mujer? ¿Y el mayor? *

summarize prin
display r(max)
display r(min)


*7. Diseñe una variable que mida la experiencia en el mercado laboral de la mujer elevada al cuadrado *

gen ax2 = ax^2


*8. Construya una variable que muestre el salario mensual de la mujer asumiendo que haya trabajado 8 horas por día y que un mes tiene 30 días *




*9. ¿Cuál es el promedio de edad de las mujeres que viven en una ciudad grande (large city)? *



*10. En promedio, ¿quién tiene más años de educación, los hombres o las mujeres? *



*11. En promedio, ¿quién tiene más años de educación, los hombres o las mujeres mayores de 30 años? *



*12. Quien tiene mayor promedio de años de educación, el padre o la madre de las mujeres encuestadas * 



*13. ¿Qué variable presenta mayor varianza? Los años de educación de la madre o del padre de las mujeres cuando estas son mayores de 25 años *



*14. ¿Existe discriminación salarial para las mujeres que no viven una ciudad grande respecto a las mujeres que si lo hacen? *



*15. Genere una variable denominada efecto que realice la suma de filas de las variables salario del hombre por hora y salario de la mujer por hora *



*16. Grafique mediante torta de porcentajes (pie) todas las variables dicotómicas que existan en la base de datos *



*17. Grafique mediante un histograma de cinco barras (bin 5) todas las variables no dicotómicas que contenga toda la base de datos * 



*18. Grafique un diagrama de dispersión donde se encuentren las variables: salario por hora de los hombres y su nivel de educación, ¿diría usted que existe una relación significativa? *



*19. Grafique un diagrama de dispersión donde se encuentren las variables:  salario por hora de las mujeres y su nivel de educación, ¿diría usted que existe una relación significativa? *



*20. Grafique un diagrama de dispersión donde se encuentren las variables: salario por hora de las mujeres con respecto a su número de hijos menores de 6 años, ¿qué puede inferir de este gráfico? *



*21. Grafique un diagrama de dispersión donde se encuentren las variables:el salario por hora de las mujeres con respecto a su número de hijos entre 6 y 18 años, ¿qué puede inferir de este gráfico? * 



*22. ¿Los años de educación de la mujer tienen alguna relación con la tasa de desempleo? * 



*23. Realice un modelo de regresión lineal replicando el modelo de Mincer (Mincer,1978) cuya variable dependiente sea el logaritmo del salario de la mujer por horas y evalúe la significatividad de las variables explicativas *


