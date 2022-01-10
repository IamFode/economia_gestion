# data
library("readxl")
library(dplyr)

data_pwt <- read_excel("data/pwt100.xlsx", sheet = "Data")
data_efw <- read_excel("data/economic-freedom-of-the-world-2021-master-index-data-for-researchers-iso.xlsx",
                  sheet = "EFW Data 2021 Report",
                  col_names = TRUE,
                  skip = 4)
head(data)
attach(data_pwt)

############################ variable dependiente ##############################
y1 <- data.frame(country, year, rgdpo, pop)
p1990_2017 <- subset(y1, year==1990 | year==2017)

# remove NA
p1990_2017<-p1990_2017[!(p1990_2017$country=="Curaçao" | p1990_2017$country =="Sint Maarten (Dutch part)"),]

# for loop 
rgdpo2017 <- c() 
rgdpo1990 <- c()
pop2017 <- c()
pop1990 <- c()
country <- c()
for(i in 1:length(p1990_2017$year)){
  if(i %% 2 == 0){
    rgdpo2017 <- c(rgdpo2017,p1990_2017$rgdpo[i])
    pop2017 <- c(pop2017,p1990_2017$pop[i])
    country <- c(country,p1990_2017$country[i])
  }else{
    rgdpo1990 <- c(rgdpo1990,p1990_2017$rgdpo[i])
    pop1990 <- c(pop1990,p1990_2017$pop[i])
  }
}

# y = rgdpo/pop
año2017 <- rgdpo2017/pop2017
año1990 <- rgdpo1990/pop1990

# dependent variable 
y = 1/(2017-1990) * log(año2017/año1990)


########################## Variables independientes ############################
# 4
## i) logaritmo neperiano del PIB per capita inicial: ln y_i, t_0
lnyt_o <- log(rgdpo1990)

## ii) El valor del índice de libertad económica "economic freedom of the world"
efw <- data.frame(efw$Year, efw$Countries,efw$`Economic Freedom Summary Index`)
efw2017 <- subset(efw, efw$efw.Year==2017)
efw2017 <- data.frame(efw2017$efw.Countries , efw2017$efw..Economic.Freedom.Summary.Index.)
colnames(efw2017) <- c("country","efw_2017")
head(efw2017)

## iii)  
" El valor del Ìndice de libertad econÛmica (y sus diferentes componentes)
Economic Freedom of the World (EFW) elaborado por el Fraser Institute
para cada paÌs en el aÒo inicial ETt0+T (1990, por ejemplo)"
efw1990 <- subset(efw, efw$efw.Year==1990)
efw1990 <- data.frame(efw1990$efw.Countries,efw1990$efw..Economic.Freedom.Summary.Index.)
colnames(efw1990) <- c("country","efw_1990")

## iv) 
" La tasa de cambio del Ìndice de libertad econÛmica Economic Freedom of the 
World (EFW) elaborado por el Fraser Institute para cada paÌs en el añp final 
ET_t_0 + T / ET_t_0. Debes tambiÈn probar, introduciÈndolo y sac·ndolo, para ver su
poder explicativo"

## v)
"Indice de capital humano por trabajador provisto por las PWT en el
último perÌodo ht0+T . Para medir h usamos la variable hc de las PWT 9.1
Human capital index, based on years of schooling and returns to education."
hc_2017 <- subset(data_pwt, data_pwt$year==2017)
hc_2017 <- data.frame(hc_2017$country,hc_2017$hc)
colnames(hc_2017) <- c("country","hc_2017")
head(hc_2017)

## vi)
"Tasa media de inversión de cada país durante el perÌodo de análisis, s_i
Calculamos la media para cada paÌs durante el perÌodo de an·lisis de la variable
csh_i de las PWT 9.1"
csh_i <- data.frame(data_pwt$country,data_pwt$year,data_pwt$csh_i)
csh_i <- subset(csh_i, data_pwt$year>=1990 & data_pwt$year<=2017)
colnames(csh_i) <- c("country","year","csh_i")
csh_i <- csh_i %>%
group_by(country) %>%
summarise(csh_i = sum(csh_i)/2017)

## vii)
"Tasa anual media de crecimiento de la población durante el periódo de análisis
n_i. La calculamos usando las PWT 9.1. n_i = 1/T * ln(popt_o+T/pop_t_o)"
tasa_anual_crecimiento <- 1/2017 * log(pop2017/pop1990)
tasa_anual_crecimiento_2017 <- data_frame(country,tasa_anual_crecimiento)

################################# dataframe ####################################
df1990_2017 <- data.frame(country,y,lnyt_o)
df1990_2017 <- merge(df1990_2017,efw2017, by = c("country"))
df1990_2017 <- merge(df1990_2017,efw1990, by = c("country"))
df1990_2017["tasa_cambio_efw_2017"] <- df1990_2017$efw_2017/df1990_2017$efw_1990
df1990_2017 <- merge(df1990_2017,hc_2017, by = c("country"))
df1990_2017 <- merge(df1990_2017,csh_i, by = c("country"))
df1990_2017 <- merge(df1990_2017,tasa_anual_crecimiento_2017, by = c("country"))
head(df1990_2017)

plot(df1990_2017$y,df1990_2017$efw_2017)

################################### Regresiones ################################
modelo<-lm(y ~ 
             lnyt_o + 
             efw_2017 + 
             efw_1990 + 
             tasa_cambio_efw_2017 +
             hc_2017+
             csh_i +
             tasa_anual_crecimiento,
           data = df1990_2017)
summary(modelo)
