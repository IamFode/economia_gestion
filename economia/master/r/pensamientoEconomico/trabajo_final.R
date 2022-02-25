# library
library("readxl")
library(dplyr)

# data
data_pwt <- read_excel("data/pwt100.xlsx", sheet = "Data")
data_efw <- read_excel("data/economic-freedom-of-the-world-2021-master-index-data-for-researchers-iso.xlsx",
                  sheet = "EFW Data 2021 Report",
                  col_names = TRUE,
                  skip = 4)

head(data)
attach(data_pwt)

########## año inicio
año_inicio = 1990
########## año final
año_final = 2017

############################ variable dependiente ##############################
y1 <- data.frame(countrycode, year, rgdpo, pop)
año1_año2 <- subset(y1, year==año_inicio | year==año_final)

# for loop 
rgdpo_final <- c() 
rgdpo_inicio <- c()
pop_final <- c()
pop_inicio <- c()
code <- c()
for(i in 1:length(año1_año2$year)){
  if(i %% 2 == 0){
    rgdpo_final <- c(rgdpo_final,año1_año2$rgdpo[i]) 
    pop_final <- c(pop_final,año1_año2$pop[i])
    code <- c(code,año1_año2$countrycode[i])
  }else{
    rgdpo_inicio <- c(rgdpo_inicio,año1_año2$rgdpo[i]) 
    pop_inicio <- c(pop_inicio,año1_año2$pop[i])
  }
}

# y = rgdpo/pop
año_f <- rgdpo_final/pop_final
año_i <- rgdpo_inicio/pop_inicio

########################### dependent variable #################################
y = 1/(año_final-año_inicio) * log(año_f/año_i)


########################## Variables independientes ############################
# 4
## i) logaritmo neperiano del PIB per capita inicial: ln y_i, t_0
lnyt_o <- log(rgdpo_inicio)

## ii) El valor del índice de libertad económica "economic freedom of the world"
efw <- data.frame(data_efw$Year, data_efw$ISO_Code_3,data_efw$`Economic Freedom Summary Index`)
efw_final <- subset(efw, efw$data_efw.Year== año_final)
efw_final <- data.frame(efw_final$data_efw.ISO_Code_3 , efw_final$data_efw..Economic.Freedom.Summary.Index.)
colnames(efw_final) <- c("code","efw_final")
head(efw_final)

## iii)  
" El valor del Ìndice de libertad economica (y sus diferentes componentes)
Economic Freedom of the World (EFW) elaborado por el Fraser Institute
para cada paÌs en el aÒo inicial ETt0+T (1990, por ejemplo)"
efw_inicio <- subset(efw, efw$data_efw.Year==año_inicio) 
efw_inicio <- data.frame(efw_inicio$data_efw.ISO_Code_3, efw_inicio$data_efw..Economic.Freedom.Summary.Index.)
colnames(efw_inicio) <- c("code","efw_inicio")

## iv) 
" La tasa de cambio del Ìndice de libertad econÛmica Economic Freedom of the 
World (EFW) elaborado por el Fraser Institute para cada paÌs en el añp final 
ET_t_0 + T / ET_t_0. Debes tambiÈn probar, introduciÈndolo y sac·ndolo, para ver su
poder explicativo"

## v)
"Indice de capital humano por trabajador provisto por las PWT en el
último perÌodo ht0+T . Para medir h usamos la variable hc de las PWT 9.1
Human capital index, based on years of schooling and returns to education."
hc_final <- subset(data_pwt, data_pwt$year==año_final)
hc_final <- data.frame(hc_final$countrycode, hc_final$hc) 
colnames(hc_final) <- c("code","hc_final")
head(hc_final)

## vi)
"Tasa media de inversión de cada país durante el perÌodo de análisis, s_i
Calculamos la media para cada paÌs durante el perÌodo de an·lisis de la variable
csh_i de las PWT 9.1"
csh_i <- data.frame(data_pwt$countrycode,data_pwt$year,data_pwt$csh_i)
csh_i <- subset(csh_i, data_pwt$year>=año_inicio & data_pwt$year<=año_final)
colnames(csh_i) <- c("code","year","csh_i")
csh_i <- csh_i %>%
group_by(code) %>%
summarise(csh_i = sum(csh_i)/año_final)

## vii)
"Tasa anual media de crecimiento de la población durante el periódo de análisis
n_i. La calculamos usando las PWT 9.1. n_i = 1/T * ln(popt_o+T/pop_t_o)"
tasa_anual_crecimiento <- 1/año_final * log(pop_final/pop_inicio)
tasa_anual_crecimiento_final <- data_frame(code,tasa_anual_crecimiento)

## viii)
"Probamos a introducir diferentes componentes del Ìndice de libertad
económica para ver su capacidad explicativa de igual modo que hemos hecho
con el Ìndice agregado."
size_government <- subset(data_efw,Year==año_final)
size_government_final <- data_frame(size_government$ISO_Code_3,size_government$`1  Size of Government`)
colnames(size_government_final) <- c("code", "size_government_final")

## ix)
" Introducimos tambien como variables explicativas los Ìndices de calidad
de gobernanza Voice and Accountability y Political Stability and Absence of
Violence elaborados por el Banco Mundial. Tomamos el valor del último año
del periodo de análisis."

################################# dataframe ####################################
df <- data.frame(code,y,lnyt_o)
df <- merge(df,efw_final, by = c("code"))
df <- merge(df,efw_inicio, by = c("code"))
df["tasa_cambio_efw_final"] <- df$efw_final/df$efw_inicio
df <- merge(df, hc_final, by = c("code"))
df <- merge(df,csh_i, by = c("code"))
df <- merge(df,tasa_anual_crecimiento_final, by = c("code"))
df <- merge(df,size_government_final, by = c("code"))
head(df)

############ borrar NA ###################


################################### Regresiones ################################
modelo<-lm(y ~ 
             lnyt_o + 
             efw_final + 
             efw_inicio + 
             tasa_cambio_efw_final +
             hc_final+
             csh_i +
             tasa_anual_crecimiento + 
             size_government_final,
           data = df)
summary(modelo)
