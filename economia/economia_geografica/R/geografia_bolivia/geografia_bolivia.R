#### Pib precios corriente 

# library
library(readxl)
library(ggplot2)

# data set
data <- read_excel("data/Bolivia - Exportaciones segun Pais de Destino por Año y Mes, 1992 - 2021.xlsx",
                   sheet = "ExpPaisesAño 92-21 Peso", 
                   skip = 3,
                   na = "")
pib <- read_excel("data/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_3585672.xls",
                     sheet = "Data",
                     skip = 3) 

code_pais <- read.csv("data/cod_paises.csv")

coor <- read.csv("data/world_country_and_usa_states_latitude_and_longitude_values.csv")

pob <- read_excel("data/API_SP.POP.TOTL_DS2_es_excel_v2_3467354.xls",
                  sheet = "Data",
                  skip = 3)

# oblación 2019
pob <- data.frame(pob$`Country Code`,pob$`2019`)
colnames(pob) <- c("code3","pob")

# latitud y longitud
coor <- data.frame(coor$country_code,coor$latitude,coor$longitude)
colnames(coor) <- c("code2","latitud","longitud")

# codigo de país
code_pais <- data.frame(code_pais$nombre,code_pais$iso2, code_pais$iso3)
colnames(code_pais) <- c("pais","code2","code3")

#borrar filas
data<-data[c(-1,-2,-3,-4,-5,-187,-188,-189,-190,-191,-192,-193,-194),]

# para el año 2019
data <- data.frame(data$`PAÍSES DE DESTINO`, data$`2019`)
colnames(data) <- c("pais","2019")

pib <- data.frame(pib$`Country Name`,pib$`Country Code`,pib$`2019`)
colnames(pib) <- c("pais","code3","2019")


# join code_pais and data

df <- merge(data,code_pais, by = c("pais"))
df <- merge(df,pib, by = c("code3"))
df$pais.y <- NULL
colnames(df) <- c("code3","pais","expor","code2","pib")
df <- df[,c(4,1,2,3,5)]
df <- merge(df,coor,by = c("code2"))
df["dist_bol"] <- my_dist(df$latitud,df$longitud)
df <- merge(df,pob,by = c("code3"))

#Eliminar NA
df<-subset(df,!is.na(expor) & !is.na(pib))

########### Formulas
my_dist <- function(lat1, long1, lat2=-16.290154, long2=-63.588653) {
  rad <- pi/180
  a1 <- lat1*rad
  a2 <- long1*rad
  b1 <- lat2*rad
  b2 <- long2*rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1)*cos(b1)*(sin(dlon/2))^2
  c <- 2*atan2(sqrt(a), sqrt(1 - a))
  R <- 6378137
  d <- R*c
  return(d)
}

## regresion
modelo <- lm(log(expor) ~ log(pib) + log(dist_bol) + log(pob), data = df)
summary(modelo)

## graficos
ggplot(data = df, aes(x = log(dist_bol), y = log(expor))) + 
  geom_text(label = df$pais,size=2.4) +
  geom_smooth(method = "lm")

ggplot(data = df, aes(x = log(pob), y = log(expor))) + 
  geom_text(label = df$pais,size=2.4) +
  geom_smooth(method = "lm")
