################################################################################
#------------------------------- LIBRARY ---------------------------------------
library("readxl")
library(dplyr)
library(tidyr)
library(memisc)
library(stargazer)
################################################################################


################################################################################
#------------------------------- DATA ------------------------------------------
data_pwt <- read_excel("data/pwt100.xlsx", sheet = "Data")
data_efw <- read_excel("data/economic-freedom-of-the-world-2021.xlsx",
                  sheet = "EFW Data 2021 Report",
                  col_names = TRUE,
                  skip = 4)
voice <- read_excel("data/voice.xlsx",
                             col_names = TRUE
                             )
politic <- read_excel("data/political.xlsx",
                             col_names = TRUE
                             )
#-------------------------------------------------------------------------------
################################################################################


#####################################
########## año inicio ###############
año_inicio = 2008 # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#####################################
########## año final ################
año_final = 2017 # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#####################################


################################################################################
########################### VARIABLE DEPENDIENTE ###############################
y1 <- data.frame(data_pwt$country, data_pwt$countrycode, data_pwt$year, 
                 data_pwt$rgdpo, data_pwt$pop)
colnames(y1) = c("country","countrycode","year","rgdpo","pop")
año1_año2 <- subset(y1, year==año_inicio | year==año_final)
# for loop 
rgdpo_final <- c() 
rgdpo_inicio <- c()
pop_final <- c()
pop_inicio <- c()
code <- c()
country <- c()
for(i in 1:length(año1_año2$year)){
  if(i %% 2 == 0){
    rgdpo_final <- c(rgdpo_final,año1_año2$rgdpo[i]) 
    pop_final <- c(pop_final,año1_año2$pop[i])
    code <- c(code,año1_año2$countrycode[i])
    country <- c(country,año1_año2$country[i])
  }else{
    rgdpo_inicio <- c(rgdpo_inicio,año1_año2$rgdpo[i]) 
    pop_inicio <- c(pop_inicio,año1_año2$pop[i])
  }
}
rgdpo_final = data_frame(code,rgdpo_final)
rgdpo_inicio = data_frame(code,rgdpo_inicio)
pop_final = data_frame(code,pop_final)
pop_inicio = data_frame(code,pop_inicio)

año_f = merge(rgdpo_final,pop_final,by="code")
año_i = merge(rgdpo_inicio,pop_inicio,by="code")

# y = rgdpo/pop
año_f <- año_f$rgdpo_final / año_f$pop_final
año_i <- año_i$rgdpo_inicio / año_i$pop_inicio

anho = data_frame(country,code,año_f,año_i)

y = data_frame(anho$country, anho$code,1/(año_final-año_inicio) * log(anho$año_f / anho$año_i))
colnames(y) = c("country","code","y")
################################################################################
################################################################################


################################################################################
########################### VARIABLES INDEPENDIENTES ###########################
## i) logaritmo neperiano del PIB per capita inicial: ln y_i, t_0
lnyt_o = data_frame(rgdpo_inicio$code,log(rgdpo_inicio$rgdpo_inicio))
colnames(lnyt_o) =c("code","lnyt_o")

## ii) El valor del índice de libertad económica "economic freedom of the world"
efw <- data.frame(data_efw$Year, 
                  data_efw$ISO_Code_3,data_efw$`Economic Freedom Summary Index`)
efw_final <- subset(efw, efw$data_efw.Year== año_final)
efw_final <- data.frame(efw_final$data_efw.ISO_Code_3 , 
                        efw_final$data_efw..Economic.Freedom.Summary.Index.)
colnames(efw_final) <- c("code","efw_final")

## iii)  
" El valor del Ìndice de libertad economica (y sus diferentes componentes)
Economic Freedom of the World (EFW) elaborado por el Fraser Institute
para cada paÌs en el aÒo inicial ETt0+T (1990, por ejemplo)"
efw_inicio <- subset(efw, efw$data_efw.Year==año_inicio) 
efw_inicio <- data.frame(efw_inicio$data_efw.ISO_Code_3, 
                         efw_inicio$data_efw..Economic.Freedom.Summary.Index.)
colnames(efw_inicio) <- c("code","efw_inicio")

## iv) 
" La tasa de cambio del Ìndice de libertad econÛmica Economic Freedom of the 
World (EFW) elaborado por el Fraser Institute para cada paÌs en el año final 
ET_t_0 + T / ET_t_0. Debes también probar, introduciéndolo y sacandolo, para ver
su poder explicativo"

## v)
"Indice de capital humano por trabajador provisto por las PWT en el
último perÌodo ht0+T . Para medir h usamos la variable hc de las PWT 9.1
Human capital index, based on years of schooling and returns to education."
hc_final <- subset(data_pwt, data_pwt$year==año_final)
hc_final <- data.frame(hc_final$countrycode, hc_final$hc) 
colnames(hc_final) <- c("code","hc_final")

## vi)
"Tasa media de inversión de cada país durante el perÌodo de análisis, s_i
Calculamos la media para cada paÌs durante el perÌodo de an·lisis de la variable
csh_i de las PWT 9.1"
csh_i <- data.frame(data_pwt$countrycode,data_pwt$year,data_pwt$csh_i)
csh_i <- subset(csh_i, data_pwt$year>=año_inicio & data_pwt$year<=año_final)
colnames(csh_i) <- c("code","year","csh_i")
csh_i <- csh_i %>%
group_by(code) %>%
summarise(csh_i = sum(csh_i)/(año_final-año_inicio))

## vii)
"Tasa anual media de crecimiento de la población durante el periódo de análisis
n_i. La calculamos usando las PWT 9.1. n_i = 1/T * ln(popt_o+T/pop_t_o)"
tasa_anual_crecimiento <- (1/(año_final-año_inicio)) * 
  log(pop_final$pop_final/pop_inicio$pop_inicio)
tasa_anual_crecimiento_final <- data_frame(code,tasa_anual_crecimiento)

## viii)
"Probamos a introducir diferentes componentes del Ìndice de libertad
económica para ver su capacidad explicativa de igual modo que hemos hecho
con el Ìndice agregado."
names(data_efw)
# área 1 -> tamaño del gobierno
size_government <- subset(data_efw,Year==año_inicio)
size_government <- data_frame(size_government$ISO_Code_3,
                                    size_government$`1  Size of Government`)
colnames(size_government) <- c("code", "size_government_o")
# área 2 -> sistema legal y derechos de propiedad
legalSys_propertyRights <- subset(data_efw,Year==año_inicio)
legalSys_propertyRights <- data_frame(legalSys_propertyRights$ISO_Code_3,
                    legalSys_propertyRights$`2  Legal System & Property Rights`)
colnames(legalSys_propertyRights) <- c("code", "legalSys_propertyRights_o")

# área 3 -> la inflación monetaria sólita
sound_money <- subset(data_efw,Year==año_inicio)
sound_money <- data_frame(sound_money$ISO_Code_3, 
                            sound_money$`3  Sound Money`)
colnames(sound_money) <- c("code", "sound_money_o")

# área 4 -> libertad para comerciarlizar internacionalmente
freedom_trade <- subset(data_efw,Year==año_inicio)
freedom_trade <- data_frame(freedom_trade$ISO_Code_3, 
                            freedom_trade$`4  Freedom to trade internationally`)
colnames(freedom_trade) <- c("code", "freedom_trade_o")

# área 5 -> Regulación
regulation <- subset(data_efw,Year==año_inicio)
regulation <- data_frame(regulation$ISO_Code_3, 
                           regulation$`5  Regulation`)
colnames(regulation) <- c("code", "regulation_o")

## ix)
" Introducimos tambien como variables explicativas los Ìndices de calidad
de gobernanza Voice and Accountability y Political Stability and Absence of
Violence elaborados por el Banco Mundial. Tomamos el valor del último año
del periodo de análisis."
voice1 <- data_frame(voice$Country,voice$Year,voice$`Governance (-2.5 to +2.5)`)
colnames(voice1) <- c("country","year","i_voice")
#rellenar filas con la palabra anterior 
voice1 <- voice1 %>% fill(country)
voice1 <- voice1[voice1$year==año_final,]
voice1$year <- NULL 

political1 <- data_frame(politic$Country,politic$Year,
                         politic$`Governance (-2.5 to +2.5)`)
colnames(political1) <- c("country","year","i_political")
political1 <- political1 %>% fill(country)
political1 <- political1[political1$year==año_final,]
political1$year <- NULL 
################################################################################
################################################################################


################################################################################
################################# DATAFRAME ####################################
# variable dependiente
df = y
# i)
df <- merge(df,lnyt_o, by = c("code"))
# ii)
df = merge(df,efw_final,by = c("code"))
# iii)
df <- merge(df,efw_inicio, by = c("code"))
# iv)
df["tasa_cambio_efw_final"] <- df$efw_final/df$efw_inicio
# v)
df <- merge(df, hc_final, by = c("code"))
# vi)
df <- merge(df,csh_i, by = c("code"))
# vii)
df <- merge(df,tasa_anual_crecimiento_final, by = c("code"))
# xiii)
df <- merge(df,size_government, by = c("code"))
df <- merge(df,legalSys_propertyRights, by = c("code"))
df <- merge(df,sound_money, by = c("code"))
df <- merge(df,freedom_trade, by = c("code"))
df <- merge(df,regulation, by = c("code"))
#ix)
df <- merge(df,voice1,by=c("country"))
df <- merge(df,political1,by=c("country"))

# borrar NA
dff <- df %>% drop_na("efw_inicio","tasa_cambio_efw_final","hc_final")
colnames(dff) = c("pais","code","y","lny_0","ile_i","ile_0","tc_i","ich_i","tmi",
                  "tpob","a1","a2","a3","a4","a5","va","sav")
################################################################################
################################################################################


################################################################################
################################ REGRESION #####################################

# 1
model1<-lm(y ~ lny_0 + 
             ile_i + 
             ile_0 + 
             tc_i +
             ich_i +
             tmi +
             tpob, 
           data = dff)

model2<-lm(y ~ lny_0 + 
             tc_i +
             ich_i +
             tmi +
             tpob, 
           data = dff)

stargazer(model1,model2,type = "latex",report=('vc*p'))

# 2
model1<-lm(y ~ lny_0 + 
             ile_i + 
             ile_0 + 
             tc_i +
             ich_i +
             tmi +
             tpob, 
           data = dff)

model2<-lm(y ~ lny_0 + 
             ile_i + 
             ile_0 + 
             ich_i +
             tmi +
             tpob, 
           data = dff)

stargazer("Modelo 1"=model1,"Modelo 2"=model2,type = "latex",report=('vc*p'))

# 3
model1<-lm(y ~ lny_0 + 
             ich_i +
             tmi +
             tpob +
             a1 +
             a2 +
             a3 +
             a4 +
             a5,
           data = dff)

model2<-lm(y ~ lny_0 + 
             ich_i +
             tmi +
             tpob +
             a1, 
           data = dff)

model3<-lm(y ~ lny_0 + 
             ich_i +
             tmi +
             tpob +
             a2, 
           data = dff)

model4<-lm(y ~ lny_0 + 
             ich_i +
             tmi +
             tpob +
             a3, 
           data = dff)

model5<-lm(y ~ lny_0 + 
             ich_i +
             tmi +
             tpob +
             a4, 
           data = dff)

model6<-lm(y ~ lny_0 + 
             ich_i +
             tmi +
             tpob +
             a5,
           data = dff)

stargazer("Modelo 1"=model1,"Modelo 2"=model2,"Modelo 3"=model3,"Modelo 4"=model4,
       "Modelo 5"=model5,"Modelo 6"=model6,type = "latex",report=('vc*p'))

# 4

model1<-lm(y ~ lny_0 + 
             ile_i + 
             ile_0 + 
             tc_i +
             ich_i +
             tmi +
             tpob +
             va + 
             sav,
           data = dff)

model2<-lm(y ~ lny_0 + 
             ile_i + 
             ile_0 + 
             tc_i +
             ich_i +
             tmi +
             tpob +
             va, 
           data = dff)

model3<-lm(y ~ lny_0 + 
             ile_i + 
             ile_0 + 
             tc_i +
             ich_i +
             tmi +
             tpob +
             sav,
           data = dff)

stargazer("Modelo 1"=model1,"Modelo 2"=model2,"Modelo 3"=model3,type = "latex",
          report=('vc*p'))


################################################################################
################################################################################

cor(dff[,3:17])
pairs(dff[,3:17],panel=panel.smooth)
