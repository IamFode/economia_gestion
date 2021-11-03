###
# 1. Recopilación y preparación de datos
###

##### Microdatos de la EES del INE

ees <- read.table("../Dat/EES/EES_2018.csv", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE) # Carga de los microdatos de un trimestre de EPA
#strip.white= #borra espacios de cada dato

# Sexo
## str(ees$SEXO)
head(ees$SEXO)
summary(ees$SEXO)
table(ees$SEXO)
ees$SEXO <- factor(ees$SEXO)
levels(ees$SEXO) <- c("H", "M")
table(ees$SEXO) # distribución de frecuencias

# Salario base
str(ees$BASE)
summary(ees$BASE)

# ANALISIS DE LOS SESGOS
## Datos faltantes
x <- ees$BASE
x[is.na(x)] <- mean(x,na.rm=TRUE) #sustituir por la media recortadatada 

## Datos Atipico
boxplot(x)
c1 <- quantile(x,0.25)
c3 <- quantile(x,0.75)
bi <- c1-1.5*(c3-c1)
bs <- c3+1.5*(c3-c1)
x[x<bi | x>bs]
y <- x[!(x<bi | x>bs)] #iliminar datos atípicos
x[(x<bi | x>bs)] <- mean(x[!(x<bi | x>bs)])

# Salvo en fichero una copia de los datos de EPA ya preparados
write.csv2(ees, file="../Dat/EES/EES_2018_prep.csv")
rm(x,y,c1,c3,bi,bs,ees)
