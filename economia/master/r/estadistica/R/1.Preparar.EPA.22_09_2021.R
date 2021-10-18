###
# 1. Recopilación y preparación de datos
###

##### Microdatos de la EPA del INE

epa <- read.table("../Dat/EPA/EPA_2021T2.csv", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE) # Carga de los microdatos de un trimestre de EPA

dim(epa) 
names(epa)
head(epa)
View(epa)
edit(epa)

# Sexo
str(epa$SEXO1)
head(epa$SEXO1)
summary(epa$SEXO1)
table(epa$SEXO1)
epa$SEXO <- factor(epa$SEXO1)
levels(epa$SEXO) <- c("H", "M")
table(epa$SEXO) # distribución de frecuencias
x<- table(epa$SEXO)
barplot(x, main = "Diagrama de barras", xlab = "Edad", ylab="frecuencia")
prop.table(x) # distribución de frecuencias relativas
prop.table(x) *100
rm(x)
table(epa$SEXO)/length(epa$SEXO)*100
sum(table(epa$SEXO))
sum(epa$FACTOREL) # Factor de elevación de la muestra por diversos atributos
xtabs(FACTOREL~SEXO, data=epa) # Distribución de frecuencias ponderadas por el factor de elevación
xtabs(FACTOREL~SEXO, data=epa)/sum(epa$FACTOREL)*100

# Situación laboral
table(epa$AOI)
epa$OPI=factor(epa$AOI) # Agrupación de AOI en: Ocupados, parados e inactivos
levels(epa$OPI)=c("O","O","P","P","I","I","I")
table(epa$OPI)
table(epa$OPI)/length(epa$OPI)*100
xtabs(FACTOREL~OPI, data=epa)/sum(epa$FACTOREL)*100 # Distribución de frecuencias ponderadas por el factor de elevación

# Rangos de edad
table(epa$EDAD1)
epa$REDAD<- cut(epa$EDAD1, breaks = c(-1, 16, 30, 65) ) # Variable agrupada en intervalos
table(epa$REDAD)
sum(table(epa$REDAD))
levels(epa$REDAD)[1]<- "[0,20]"
table(epa$REDAD)

# Salvo en fichero una copia de los datos de EPA ya preparados
write.csv2(epa, file="../Dat/EPA/EPA_2021T2_prep.csv")

