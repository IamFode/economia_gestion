/*Universidad Privada Boliviana. Econometría Clásica /*
/* Christian Limbert Paredes Aguilera /*


clear
set more off
set mem 50m
cd "C:\Users\fode\Downloads"
use "WAGE1.DTA"
capture log close
log using ramr.log, replace


/*1. Describa las variables de la base: media, varianza, min., máx., cantidad de observaciones, etc./*
sum

/*2. Explore cuidadosamente la base de datos con los comandos aprendidos en clase. Y explique brevemente si encontró alguna anomalía./*
des
mdesc


/*3 Grafique la distribución de todas las variables/* 
histogram educ, normal kdensity title(Estimación no paramétrica de la distribucióm empírica) subtitle(Comparación con la distribución normal)

histogram exper, normal kdensity title(Estimación no paramétrica de la distribucióm empírica) subtitle(Comparación con la distribución normal)

histogram tenure, normal kdensity title(Estimación no paramétrica de la distribucióm empírica) subtitle(Comparación con la distribución normal)


/*4 4 Realice un gráfico de la variable wage como función de la variable "educ", "exper" y "tenure" /*

twoway (lfit wage educ) (scatter wage educ), title(Gráfico de wage en función de educ)
twoway (lfit wage exper) (scatter wage exper), title(Gráfico de wage en función de exper)
twoway (lfit wage tenure) (scatter wage tenure), title(Gráfico de wage en función de tenure)

/*5 Encuentre la matriz de correlaciones/*
corr wage educ exper tenure
 
/* 6 Realice una regresión de price respecto de educ, exper y tenure /*
reg wage educ exper tenure


/*7 Genere una serie con los residuos de su estimación en 6). Grafique el histograma de esta serie, ¿Parece seguir una distribución normal?/* 
predict r, resid

histogram r, kdensity title("Histograma de los residuos")  

/*8 Genere los residuos al cuadrado (aproximación de la varianza para cada elemento de la muestra)/*
generate r2=r*r


/*9 Grafique la varianza como función de los regresores y encuentre la matriz de correlaciones/*
twoway (scatter r2 educ)

twoway (scatter r2 exper)

twoway (scatter r2 tenure)

corr r2 educ exper tenure 

/*10 Realice la prueba de variables omitidas/*
estat ovtest

/*11 Realice la prueba de heterocedasticidad de Breusch-Pagan / Cook-Weisberg/*
 estat hettest

/*12 Estandarice los residuos y compare los resultados con la distribución normal estandarizada/*
pnorm r, title("Standarized normal probability plot")
qnorm r, title("Cuantiles de los residuos vs. cuantiles de la distrib. normal")


/*13 Realice la prueba de normalidad de los residuos/*
sktest r

/*14 Estudie la multicolinealidad del modelo y determine el mayor grado de correlación entre las variables/*
vif

/*15 Comente los resultados obtenidos y sugiera enmiendas./*


/*16 Realice la estimación en logaritmos/* 




log close

