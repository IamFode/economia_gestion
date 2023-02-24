################################################################################
################################ EJERCICIOS ####################################
################################################################################

#LIBRERIAS
library(ggplot2)
library(dplyr)
library(tibble)
library(ggrepel)
library(MASS)


# DATA
world2016 <- read.csv("data/data-world2016.csv",header=TRUE,dec=".",sep=",")
attach(world2016)

################################# EJERCICIO 3 ##################################

' (a) Encuentre los países con las diez distancias Mahalanobis más altas y las 
diez más bajas en el ejemplo anterior. '
data.for.mahalanobis=data.frame(log.GDP.percapita,life.expectancy)
M <- colMeans(data.for.mahalanobis)
S <- cov(data.for.mahalanobis)
mahalanobis.distances.2 <- apply(data.for.mahalanobis,
                                 1,
                                 mahalanobis,
                                 center=M,
                                 cov=S)
mahalanobis.distances <- sqrt(mahalanobis.distances.2)
dist_mah = cbind.data.frame(country,mahalanobis.distances,income.level.code)
# 10 distancias más altas
dist_mah[order(-dist_mah$mahalanobis.distances),][1:10,]
# 10 distancias más bajas
dist_mah[order(dist_mah$mahalanobis.distances),][1:10,]


' (b) Construya un diagrama de caja de las distancias de Mahalanobis según el 
nivel de ingresos e identifique los países correspondientes a los valores 
atípicos de cada grupo.'
is_outlier <- function(x) {
  return(x < quantile(x, 0.25)-1.5*IQR(x) | x>quantile(x,0.75)+1.5*IQR(x))
}

dat <- dist_mah %>% tibble::rownames_to_column(var="outlier") %>% 
  group_by(income.level.code) %>% 
  mutate(is_outlier=ifelse(is_outlier(mahalanobis.distances), 
                           country, as.character(NA)))

ggplot(dat, 
       aes(y=mahalanobis.distances, x=factor(income.level.code))) + 
  geom_boxplot() + 
  geom_text_repel(aes(label=is_outlier),
            na.rm=TRUE,
            size = 3 
            ) +
  labs(x="Distancia de Mahalanobis", y = "Nivel de ingreso")

################################################################################


################################# EJERCICIO 4 ##################################

'Considere el marco de datos que consta de las variables (life.expectancy) 
esperanza de vida, (fertility.rate) tasa de fertilidad, (mortality.rate) tasa de 
mortalidad, (pop.growth) crecimiento de la población, (pop.rural) población 
rural y  log.PIB.percapita del conjunto de datos world2016. Calcule las 
distancias de Mahalanobis de todos los países con respecto al vector de medias. 
Analizar y comentar los resultados obtenidos.'
data.for.mahalanobis = data.frame(life.expectancy,
                                  fertility.rate,
                                  mortality.rate,
                                  pop.growth,
                                  pop.rural,
                                  log.GDP.percapita)
M <- colMeans(data.for.mahalanobis)
S <- cov(data.for.mahalanobis)
mahalanobis.distances.2 <- apply(data.for.mahalanobis,
                                 1,
                                 mahalanobis,
                                 center=M,
                                 cov=S)
mahalanobis.distances <- sqrt(mahalanobis.distances.2)
dist_mah = cbind.data.frame(country,mahalanobis.distances)
# 10 distancias más altas
dist_mah[order(-dist_mah$mahalanobis.distances),][1:10,]
# 10 distancias más bajas
dist_mah[order(dist_mah$mahalanobis.distances),][1:10,]

'A diferencia del anterior ejercicio vemos que al utilizar las variables:

- esperanza de vida, 
- tasa de fertilidad, 
- tasa de mortalidad, 
- crecimiento de la población, 
- población rural y  
- logaritmno del pib per cápita 

la distancia de Mahalanobis cambia y por ende cambia los paises que tiene mayor 
y menor distancia. Por ejemplo Oman esta más alejado del vector de medias y 
Paraguay es el que está más cerca se encuentra.'

################################################################################


############################### EJERCICIO 5 ####################################
'Considere el conjunto de datos world2016. Realice un PCA al conjunto de 
variables (life.expectancy) esperanza.vida, (fertility.rate) tasa.fecundidad, 
(mortality.rate) tasa.mortalidad, (pop.growth) crecimiento.población, 
(pop.rural) población.rural, log.PIB.percapita y (GDP.growth) crecimiento.PIB.'

pca_data = data.frame(country,
                      life.expectancy,
                      fertility.rate,
                      mortality.rate,
                      pop.growth,
                      pop.rural,
                      log.GDP.percapita,
                      GDP.growth)

pca = pca_data[,2:8]

'(a) ¿Realizaría el PCA con base en la matriz de varianza-covarianza o en la 
matriz de correlación?'
'Respuesta.- Realizaremos con la matriz de correlación $R$ ya que en la práctica 
tenderá a eliminar el efecto de las unidades de cada variables.'

# Matriz de correlaciones estandarizada
pca_world = princomp(pca,cor=TRUE)
# Resumen
summary(pca_world)
# autovectores
# Vemos las variables esta más relacionada cada componente principal
pca_world$loadings
# nuevo conjunto de datos
df_pca = data.frame(pca_data$country, pca_world$scores[,1:3])

'(b) ¿Cuántos componentes principales son relevantes para describir este 
conjunto de variables? ¿Por qué?'

screeplot(pca_world)

'Respuesta.- Nos quedaremos con 3 componentes porque, la
porporción acumulativa hasta el tercer componentes es del casi 90 %. También 
podemos ver este hecho si realizamos un gráfico de barras.'


'(c) Interprete los dos primeros componentes principales. Representar las 
puntuaciones correspondientes a los dos primeros componentes principales en un 
diagrama de dispersión y describir el resultado.'
'
Respuesta.- Con respeto al primer componente vemos que mientras más alto es el 
es el valor de cada país entonces existe más esperanza de vida, menos tasa de 
fecundación, menos tasa de mortalidad y más PIB per cápita. Por ejemplo Japón 
encabeza la lista y el último es Chad.
Con respecto a la segunda componente la única variable explicativa sería el
crecimiento del PIB es decir a mayor valor mayor crecimiento.
'

plot(pca_world$scores[,1],
     pca_world$scores[,2],
     xlab="1st PC",
     ylab="2nd PC",
      type="n",frame=F)
text(pca_world$scores[,1],
     pca_world$scores[,2],
     pca_data$country,
     cex=0.75)
     
'
El gráfico da una idea de cómo se comporta los paises en términos de los dos 
primeros componentes principales. Podríamos estar tal vez en una regresión no 
lineal. 
Por ejemplo:
Japón se encuentra al extremo derecho de la gráfica con mayor calidad de vida y
mejor PIB per cápita. Vemos a Iraq con una moderada calidad de vida y un alto 
PIB pc, no así con Nigeria que tiene una baja calidad de vida y bajo PIB pc.
'
################################################################################


############################## EJERCICIO 8 #####################################
'Considere las siguientes siete variables del conjunto de datos world2016: 
life.expectancy, fertility.rate, mortality.rate, pop.growth, pop.rural, 
log.GDP.percapita y GDP.growth.'
fac_data = data.frame(country,
                      life.expectancy,
                      fertility.rate,
                      mortality.rate,
                      pop.growth,
                      pop.rural,
                      log.GDP.percapita,
                      GDP.growth)
' 
a) ¿Cuál es el número máximo de factores permitidos en un modelo de análisis 
factorial con siete variables? ¿Cuántos factores deberían emplearse para 
explicar estas siete variables?
Respuesta.- El número máximo permitido será de 3 variables. Y se debería emplear
3 factores.
'
'
b) Realice un análisis factorial con tres factores. Encuentre la comunalidad y 
la variabilidad específica para cada variable observada. Comente los resultados 
(preste especial atención a la variable GDP.growth
Respuesta.- Vemos que la única variable que se queda sin explicar es GDP.growth
Esto se reflja cuando analizamos las matrices de carga. Con respecto a las demás
variables observamos que life.expectancy es la que más puede explicar seguido de
fertility.rate. pop.rural se queda con un 30% sin explicar. 
'
factanal(fac_data[,2:8],factors=1)
factanal(fac_data[,2:8],factors=2)
factanal(fac_data[,2:8],factors=3)
'
c) Interpretar los factores obtenidos en términos de la matriz de cargas
Respuesta.- Con respecto el matriz de cargas se tiene 7 filas y tres columnas
donde la columna 3 es la única que aparece con 4 "blancos". El factor 1 o
columna 1 explica de mejor manera a las variables life.expectancy, pop.rural y
log.GDP.percapita, con respecto al factor 2 las variables que tienen una mejor 
capacidad explicativa son fertility.rate y pop.growth.
La variabilidad explicada total en el primer factor es del casi 40%, del segundo
es del 30% y el último sólo de un 4%.
'
################################################################################


############################### EJERCICIO 11 ###################################
'
Considere el conjunto de datos world2016. Aplique el método de K-medias para 
crear grupos de países basados en las variables esperanza de vida, tasa de 
fertilidad, tasa de mortalidad, crecimiento de la población, población rural, 
log.PIB percapita y crecimiento del PIB. Compruebe si los países de la OCDE 
pertenecen al mismo grupo cuando se considera un número pequeño de grupos 2 o 3.
Respuesta.- Efectivamente los paises de la OCDE pertenecen al mismo grupo.
'
k_data = data.frame(life.expectancy,
                      fertility.rate,
                      mortality.rate,
                      pop.growth,
                      pop.rural,
                      log.GDP.percapita,
                      GDP.growth)

km3 <- kmeans(k_data,3)
cluster1 = as.character(country)[km3$cluster==1]
cluster2 = as.character(country)[km3$cluster==2]
cluster3 = as.character(country)[km3$cluster==3]

OECD_Group = subset(country,OECD=="yes")

cluster = cluster1

for (i in 1:length(OECD_Group)){
  for (j in 1:length(cluster)){
    if (OECD_Group[i] == cluster[j]){
      print(OECD_Group[i])
    }
  }
}

################################################################################


################################ EJERCICIO 13 ##################################
'
Considere el conjunto de datos world2016.
'
'
(a) Considere dos clases de países: aquellos con ingresos "bajos" o "medios 
bajos" y aquellos con ingresos "medios altos" o "altos". Cree una variable 
binaria para asignar cada país a uno de estos dos grupos.
'
world2016 = mutate(world2016,
       ingreso = ifelse(income.level.code=="H" | 
                          income.level.code=="UM","high","low"))

'
(b) Cree una regla de discriminación para clasificar los países en los dos 
grupos creados en (a) de acuerdo con las variables demográficas esperanza.vida, 
tasa.fecundidad, tasa.mortalidad, crecimiento.población y población.rural.
'
attach(world2016)
dis_data = data.frame(life.expectancy,
                      fertility.rate,
                      mortality.rate,
                      pop.growth,
                      pop.rural,
                      log.GDP.percapita,
                      GDP.growth,
                      ingreso)
lda.word <- lda(ingreso~life.expectancy+
                  fertility.rate+
                  mortality.rate+pop.growth+
                  pop.rural+
                  log.GDP.percapita+
                  GDP.growth
                )
plot(lda.word)

'
(c) Encuentre la tabla de confusión basada en la validación cruzada. Discutir 
los resultados obtenidos.
Respuesta.- Vemos que se tiene 6 falsos positivos y 8 falsos negativos. De los 
175 datos (está claro que estos resultados pueden variar). Visto esto el modelo 
clasifica y/o predice a un 92%. 
'
n <- length(ingreso)
predicted.class <- factor(rep(NA,n),levels=c("high","low"))
for (i in 1:n){
lda.cv <- lda(ingreso~life.expectancy+
                  fertility.rate+
                  mortality.rate+pop.growth+
                  pop.rural+
                  log.GDP.percapita+
                  GDP.growth, data=dis_data[-i,])
predicted.class[i] <- predict(lda.cv,dis_data[i,])$class
}
table(predicted.class,ingreso)

################################################################################