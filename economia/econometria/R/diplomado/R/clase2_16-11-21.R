library(foreign)

var <- read.dta("http://www.stata-press.com/data/r9/auto.dta")
summary(var)

# nombre de columnas
colnames(var)

# resumen estadístico de un variable
summary(var$price)

# Comandos condicionales 
foreign <- var[var$foreign == "Foreign",]
subset(var,foreign == "Foreign")

# las millas son menores a 20
var[var$length > 20,]

# contar observaciones
dim(foreign)
nrow(foreign)
ncol(foreign)

sumafil <- function(x){
  sum <- 0
  for (i in 1:nrow(x)){
    sum <- sum + 1
  }
  return(sum)
}
sumafil(foreign)

# eliminar variables 
foreign$make <- NULL

#eliminar observaciones


# import data
library(readxl)
excel <-read_excel("Dat/clase2_16-11-21/ejem_exel.xlsx") 

summary(excel)

# Prueba de test de media
t.test(Nota~Sexo, data = excel,mu=60, conf.level = .95)

#Criterio 3 p-value

