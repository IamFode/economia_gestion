
# --------------- Encuesta de hogares 2020 (Bolivia) ---------------

## library
library(haven) # Librería para importar data sets spss
library(ggplot2)
## Data set
personas = as.data.frame(read_sav("../Dat/EH2020_Persona.sav"))

## functions
cv=function(x){ # Función que calcula el coeficiente de variación de un vector de datos x
  sqrt(var(x)*(length(x)-1)/length(x))/ mean(x)
}
moda=function(x){ # Función que calcula la moda de un vector de datos y
  sort(table(x), TRUE)[1]
}


# --------------- Educación ---------------

## ----- Género ------

personas$s01a_02 <- factor(personas$s01a_02)
levels(personas$s01a_02) <- c("H","M")

genero <- table(personas$s01a_02)
generoFrame <- as.data.frame(genero)
colnames(generoFrame) <- c("Genero", "Cantidad")
### plot 
ggplot(data=generoFrame, aes(x=Genero, y=Cantidad)) +
  geom_bar(stat="identity", fill = "steelblue") + theme_minimal()
### distribución de frecuencias relativas
prop.table(x)*100

## ----- Nivel mas alto de instrucción ----- 
table(personas$s03a_02a)
personas$s03a_02a <- factor(personas$s03a_02a)

nivel_edu <- table(personas$s03a_02a)
table(personas$s03a_02a) / length(personas$s03a_02a)*100

## ----- Rango de edad -----

table(personas$s01a_03)
sum(table(personas$s01a_03))
table(personas$s01a_03)


## ---------- Salvar fichero ----------

write.csv2(personas, file="../Dat/educacion_prep.csv")



# --------------- Análisis univariante de EDAD ---------------
x <- personas$s01a_03
mean(x,na.rm = TRUE)
var(x)
sd(x)
median(x)
moda(x)
cv(x)
quantile(x)
hist(x)
plot(density(x))
boxplot(x)

## Filtros
### Nivel de educación más alto que se aprobó
sin_estudios <- subset(personas, s01a_03 >= 18 & s03a_02a == 11)

colegio_no_concluido <- subset(personas,
                              (s03a_02a == 12
                              | s03a_02a == 13 
                              | s03a_02a == 21
                              | s03a_02a == 22
                              | s03a_02a == 23
                              | s03a_02a == 31
                              | s03a_02a == 41
                              | s03a_02a == 51
                              | s03a_02a == 52
                              | s03a_02a == 61
                              | s03a_02a == 62
                              | s03a_02a == 64
                              | s03a_02a == 65) & s01a_03 >= 18)

colegio <- subset(personas, 
                  s03a_02a == 42 
                  | s03a_02a == 32
                  | s03a_02a == 63)

tecnico <- subset(personas, 
                  s03a_02a == 76
                  | s03a_02a == 77
                  | s03a_02a == 78
                  | s03a_02a == 79
                  | s03a_02a == 80)

pregrado <- subset(personas,
                  s03a_02a == 71
                | s03a_02a == 72)

postgrado <- subset(personas,
                  s03a_02a == 73
                  | s03a_02a == 74
                  | s03a_02a == 75)

nrow(sin_estudios)
nrow(colegio)
nrow(tecnico)
nrow(pregrado)
nrow(postgrado)

personas$s03a_02a <- factor(personas$s03a_02a)
levels(personas$s03a_02a) <- c("sin_colegio",
                               "no_concluido",
                               "no_concluido",
                               "no_concluido",
                               "no_concluido",
                               "no_concluido",
                               "no_concluido",
                               "colegio",
                               "no_concluido",
                               "colegio",
                               "no_concluido",
                               "colegio",
                               "no_concluido",
                               "no_concluido",
                               "colegio",
                               "tecnico",
                               "no_concluido",
                               "universidad",
                               "universidad",
                               "postgrado",
                               "postgrado",
                               "postgrado",
                               "tecnico",
                               "tecnico",
                               "tecnico",
                               "tecnico",
                               "tecnico",
                               "colegio")
table(personas$s03a_02a)
table(personas$s03a_02a) / length(personas$s03a_02a)*100


## --------------- Análisis bivariante ---------------

sexo <- c(colegio_no_concluido$s01a_02, sin_estudios$s01a_02)
edad <- c(colegio_no_concluido$s01a_03, sin_estudios$s01a_03)
by(edad,sexo,mean)
by(edad,sexo,cv)
aggregate(edad, list(sexo), mean)
edad <- cut(edad,breaks = c(18,28,38,48,58,68,78,88,98))
boxplot(edad ~ sexo)
table(edad, sexo)
prop.table(table(edad,sexo),1)
prop.table(table(edad,sexo),2)


## --------------- Series temporales ---------------

hidrocarburos = as.data.frame(read_sav("../Dat/BD-PET-GAS-1990-2017.sav"))
### objetivos para series de datos
hidrocarburos.ts <- ts(hidrocarburos$GASNATURALMPC, start=c(1990,1), frequency = 12)
plot(hidrocarburos.ts, type = "l", xlab = "Mes", ylab = "Metro pies cubicos")

### Descompocisión aditiva 
hidrocarburos.comp <- decompose(hidrocarburos.ts,type = "additive") 
plot(hidrocarburos.comp,xlab="Mes")

hidrocarburos$P <- hidrocarburos.comp$trend # tendencia a largo plazo, ciclotendencia
hidrocarburos$S <- hidrocarburos.comp$seasonal # estacionalidad
hidrocarburos$U <- hidrocarburos.comp$random # 
barplot(hidrocarburos.comp$figure, names=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")) ### los dos primeros meses año tras año son menores que la media, el tercero un poco mayor que la media  
#diagrama de secuencias de datos almacenados de una estructura ts
### Serie original
ts.plot(hidrocarburos.ts, lty="solid", col="black", xlab="Mes", ylab="Metro pies cubicos")
### ts menos estacionalidad, serie suavizada o serie real
ts.plot(hidrocarburos.ts-hidrocarburos.comp$seasonal, lty="solid", col="black", xlab="Año", ylab="Metro pies cubicos")
ts.plot(cbind(hidrocarburos.ts, hidrocarburos.ts-hidrocarburos.comp$seasonal), lty=c("solid", "dashed"), col=c("black", "blue"), xlab="Año", ylab="Metro pies cubicos")
#legend("bottom", c("Serie original", "Desestacionalizada"), lty=c("solid", "dashed"), col=c("black", "blue"), lwd=c(1,1))

t<- cbind(hidrocarburos.ts, hidrocarburos.ts-hidrocarburos.comp$seasonal)

##### Serie anual
### Sacar los primeros numeros y colocar como entero para colocar una serie anual
hidrocarburos$anual <- as.integer(substr(hidrocarburos$AÑOMES,1,4))
### Nos da para cada año una media
hidrocarburosanual <- data.frame(anual=seq(min(hidrocarburos$anual),max(hidrocarburos$anual)),hidrocar=tapply(hidrocarburos$GASNATURALMPC, hidrocarburos$anual,mean))
plot(hidrocarburosanual)

hidrocarburosanual.ts<- ts(hidrocarburosanual$hidrocar, start = 1990)

#  Si se quiere suavizar (Medias móviles de orden 3) toma las medias de tres en tres
plot(hidrocarburosanual$hidrocar, type = "o")
hidrocarburosanual$P3<- filter(hidrocarburosanual.ts, sides=2, rep(1/3,3))# MM3
lines(hidrocarburosanual$P3, lty = "dashed", col = "red")
hidrocarburosanual$P5<- filter(hidrocarburosanual.ts, sides=2, rep(1/5,5)) # MM5
lines(hidrocarburosanual$P5, lty = "dashed", col = "red")

plot(hidrocarburosanual$P3, type="o")
plot(hidrocarburosanual$P5, type="o")

reg<-lm(hidrocar~anual, data=hidrocarburosanual[hidrocarburosanual$anual>=2000,])
summary(reg)
abline(reg, col="blue")


## --------------- Regresión ---------------

# regresor Nivel de estudios
table(personas$s03a_02a) 
reg_simple<-lm(s04c_18a ~s03a_02a, data=personas)
summary(reg_simple)
### Como base a los que nunca asistieron al colegio, se tiene que los que no 
### concluyeron el colegio ganan en promedio 1218 bs más y así sucesivamente.


# Regresión múltiple con ambos regresores
reg_multiple<-lm(s04c_18a~s03a_02a+s01a_02, data=personas)
summary(reg_multiple)
#las mujeres en media tiene un sueldo 336 menos que los hombres.







