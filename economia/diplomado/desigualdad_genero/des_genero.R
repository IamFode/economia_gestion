# library
library(ggplot2)
library(dplyr)
library(devtools)
library(reshape2)
library(patchwork)
library(hrbrthemes)

# data
milenio <- read.csv("data/milenio.csv")
meta <- read.csv("data/meta_datos_milenio.csv")

# Bolivia
bolivia <- subset(milenio, milenio$Country.Name=="Bolivia")
colnames(bolivia)

bolivia$Country.Code <- NULL
bolivia$Country.Name <- NULL
bolivia$Series.Code <- NULL
bolivia$Scale..Precision. <- NULL

# cambiar nombre de columnas
colnames(bolivia) <- c("name","1991","1992","1993","1994","1995",
                       "1996","1997","1998","1999","2000","2001",
                       "2002","2003","2004","2005","2006","2007",
                       "2008","2009","2010","2011","2012","2013",
                       "2014","2015")

bol <- data.frame(t(bolivia[-1]))
colnames(bol) <- bolivia[,1]

año <- c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,
         2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)

attach(bol)
par(mfrow=c(2,2))
# contexto institucional
## Proporción de escaños ocupados por mujeres en los parlamentos nacionales (%)
plot(año,`Proportion of seats held by women in national parliaments (%)`,ylab="Proporción de escaños ocupados por mujeres en los parlamentos nacionales (%)")


# oportunidades laborales
## Relación entre empleo y población, mayores de 15 años, mujeres (%) 
### La relación entre empleo y población es la proporción de la población de 
#### un país que está empleada. 
plot(año,`Employment to population ratio, 15+, female (%) (modeled ILO estimate)`,ylab ="Relación entre empleo y población, mayores de 15 años, mujeres (%)"  )


# rol de la mujer en la fuerza de trabajo
## Trabajadoras familiares auxiliares, mujeres (%) 
"""Los trabajadores familiares auxiliares son aquellos trabajadores que tienen 
 empleos por cuenta propia como trabajadores por cuenta propia en un 
 establecimiento orientado al mercado operado por una persona relacionada 
 que vive en el mismo hogar."""
 
#plot(año,bol$`Contributing family workers, female (% of female employment)`)

# educación
## Tasa de alfabetización, mujeres jóvenes (% de mujeres de 15 a 24 años)
plot(año,`Literacy rate, youth female (% of females ages 15-24)`,ylab = "Tasa de alfabetización, mujeres jóvenes (% de mujeres de 15 a 24 años)")


# Embarazo adolescente
## Embarazadas de reciben atención prenatal (%)
plot(año,`Pregnant women receiving prenatal care (%)`,ylab ="Embarazadas de reciben atención prenatal (%)" )



""" Las mujeres en los parlamentos son el porcentaje de escaños parlamentarios en una cámara única 
o baja ocupados por mujeres."""

a<-ggplot(bol,aes(x=año)) + 
  geom_smooth(aes(y=bol$`Proportion of seats held by women in national parliaments (%)`))+
  geom_point(aes(y=bol$`Proportion of seats held by women in national parliaments (%)`),colour = "black",size=1.5)+
  labs(title="Proporción de escaños ocupados por mujeres en los parlamentos (%)",x="Año",y="Porcentaje (%)")+
  theme_linedraw()

""" La relación entre empleo y población es la proporción de la población de un 
país que está empleada. El empleo se define como personas en edad de trabajar 
que, durante un breve período de referencia, se dedicaron a cualquier actividad 
para producir bienes o prestar servicios a cambio de una remuneración o una 
ganancia, ya sea en el trabajo durante el período de referencia (es decir, 
que trabajaron en un empleo durante al menos una hora)"""

b<-ggplot(bol,aes(x=año)) + 
  geom_smooth(aes(y=bol$`Employment to population ratio, 15+, female (%) (modeled ILO estimate)`))+
  geom_point(aes(y=bol$`Employment to population ratio, 15+, female (%) (modeled ILO estimate)`))+
  labs(title="Relación entre empleo y población, mayores de 15 años, mujeres (%)",x="Año",y="Porcentaje (%)")+
  theme_linedraw()
  

""" La tasa de alfabetización juvenil es el porcentaje de personas de 15 a 24 
años que pueden leer y escribir con comprensión de una declaración breve y 
sencilla sobre su vida cotidiana."""

c<-ggplot(bol,aes(x=año)) + 
  geom_smooth(aes(y=bol$`Literacy rate, youth female (% of females ages 15-24)`))+
  geom_point(aes(y=bol$`Literacy rate, youth female (% of females ages 15-24)`))+
  labs(title=" Tasa de alfabetización, mujeres jóvenes (% de mujeres de 15 a 24 años)",x="Año",y="Porcentaje (%)")+
  theme_linedraw()


""" Las mujeres embarazadas que reciben atención prenatal son el porcentaje de 
mujeres atendidas al menos una vez durante el embarazo por personal de salud 
calificado por razones relacionadas con el embarazo."""

d<-ggplot(bol,aes(x=año)) + 
  geom_smooth(aes(y=bol$`Pregnant women receiving prenatal care (%)`),method = "lm")+
  geom_point(aes(y=bol$`Pregnant women receiving prenatal care (%)`))+
  labs(title=" Embarazadas de reciben atención prenatal (%)",x="Año",y="Porcentaje (%)")+
  theme_linedraw()

a+b+c+d






