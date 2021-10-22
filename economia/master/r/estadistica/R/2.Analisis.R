# Análisis

source("Funciones.R")
source("1.Preparar.EPA.R")

epa <- read.csv2(file="../Dat/EPA/EPA_2021T2_prep.csv",encoding = "UTF8")

#----------Análisis univariante de EDAD----------

x <- epa$EDAD1

mean(x)
summary(x)
mean(x,na.rm=TRUE) # Datos faltantes (na.rm = TRUE)
var(x) # cuasivarianza (varianza muestral) 
sd(x) # cuasidesviación típica

var(x)*(length(x)-1)/length(x) # varianza poblacional
sqrt(var(x)*(length(x)-1)/length(x)) # Desviación típica 
sqrt(var(x)*(length(x)-1)/length(x))/mean(x) # coeficiente de variación
mean((x-mean(x))^2)
cv(x)
median(x)
quantile(x)
quantile(x,0.92)

## Crear una función moda
t <- table(x)
sort(t, TRUE)[1]

moda(x) # Función moda creada

## Gráficas
hist(x) # histograma
plot(density(x)) # represetación no paramétrica del histograma

### Sobre poner gráficas
hist(x, freq = FALSE) #  histograma de densidades (freq =FALSE)
lines(density(x),col = "red")
boxplot(x)

## Filtros
### Filtros horizontales --> variable condicionada
dat <- subset(epa, EDAD1>=16) #sobre toda la tabla
dat <- epa[epa$EDAD1>=16 & epa$SEXO == "M",]
dat <- epa[epa$EDAD1>=16 | epa$SEXO == "M",]

### Filtros verticales --> Selecionar variables
dat <- epa[epa$EDAD1>=16,c("SEXO", "EDAD1","REDAD","FACTOREL","OPI")]


#----------Análisis bivariante----------
x <- dat$SEXO
y <- dat$EDAD1
by(y,x,mean) # media condicionada de la edad para cada sexo
by(y,x,moda) # moda de la edad para cada sexo
by(y,x,cv) # coeficiente de variación
aggregate(y,list(x),mean) # tabla de la media
boxplot(y~x)

table(x,y)
table(x,dat$REDAD)
table(x,dat$REDAD)/length(x) # frecuencias relativas
prop.table(table(x, dat$REDAD), 1) # tabla de contingencias condicionada por filas 
prop.table(table(x, dat$REDAD), 2) # tabla de contingencias condicionada por columnas

rm(x,y,t)

## Crear estadística de los parados
aggregate(FACTOREL~SEXO,data = dat[dat$OPI=="P",], length) # factorel-> Cuantos encuestados hay hombre y mujer 
aggregate(FACTOREL~SEXO,data = dat[dat$OPI=="P",], sum) # inferencia la cantidad de parados 
parados <- aggregate(FACTOREL~SEXO,data = dat[dat$OPI=="P",], sum)
names(parados)[2] <- "NumParados"
parados
activos <- aggregate(FACTOREL~SEXO,data = dat[dat$OPI!="I",], sum)
names(activos)[2] <- "NumActivos"
activos

### taza de parados
s_epa <- merge(parados,activos) # fucionar dos tablas
s_epa$TasaParo <- s_epa$NumParados / s_epa$NumActivos*100
s_epa
diff(s_epa[,3]) # diferencia de parados

write.csv2(s_epa, file="../Dat/EPA/TasaParados.csv")
rm(epa,dat,parados,activos,s_epa)
