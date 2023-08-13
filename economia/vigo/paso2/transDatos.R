
############################## FUNCIONES #######################################
source("paso2/functions.R")

############################## LIBRERIAS #######################################
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

################################ DATA ##########################################
GDPreal = read_excel("paso2/data/GDP-WB-IFS.xlsx", 
                         sheet = "GDPreal-IMF+WB")
P_t = read_excel("paso2/data/Prices-WB-IFS.xlsx", 
                            sheet = "GDPDefl(2010)WB+IMF")
M1 = read_excel("paso2/data/SeriesAnuales.xlsx", 
                    sheet = "M1")
############################# VARIABLES ########################################
`"
P_tY_t=v_tM_t

P_t = Indice de precios del deflactor
Y_t = PIB real.
P_tY_t = PIB nominal.
v_t = Velocidad del dinero.
M_t = Un agregado monetario.
"`

############################# LIMPIEZA #########################################
Y_t = slice(GDPreal,1:223)
M_t = slice(M1,1:223) 
rm(M1)

`"
Al verificar P_t y Y_t, nos damos cuenta que se tiene datos desde el año 1948, 
por lo que decidimos eliminar los años que no contiene datos.
"`
P_t = subset(P_t,select = -(6:which(names(P_t)=="1947")))
Y_t = subset(Y_t,select = -(6:which(names(Y_t)=="1947")))
M_t = subset(M_t,select = -ncol(M_t))
`"
procedemos a cambiar los nombres de países de M_t, con las de P_t ya que difieren 
algunos países en su escritura. 
"`
M_t = cbind(P_t[,1], M_t[, -1])
`"
Eliminar las primera 5 columnas de cada variables, para una mejor práctica de 
las series. Esto ya que sólo contienen metadatos.
"`
P_t = subset(P_t,select = -(1:5))
Y_t = subset(Y_t,select = -(1:5))
M_t = subset(M_t,select = -(1:5))

`"
Cambiar todos los ceros por NA
"`
P_t[P_t==0] = NA
Y_t[Y_t==0] = NA
M_t[M_t==0] = NA

`"
M_t aún tenia datos (string), por lo que debemos tranformarlos a datos (numeric)
"`
M_t = as.data.frame(apply(M_t,c(1,2),as.numeric))
any(sapply(M_t, is.character))
`"
Agreguemos dos columnas a M_t ya que las otras variables tiene datos hasta los 
años 2019 y M_t tiene datos hasta 2017
"`
M_t[,"2018"] <- NA
M_t[,"2019"] <- NA

#################### TRANFORMACIÓN DE VARIABLES I ##############################
#i. inflación (ln(P_{t+1}/(P_t)))
Ptrans = lDif(P_t)

#i. Crecimiento excesivo de dinero ln((M_{t+1}/Y_{t+1})/(M_t/Y_t))
M.Y_t = M_t/Y_t
M.Ytrans = lDif(M.Y_t)

# Construcción de un dataframe con nombres de países
Y_tt = slice(GDPreal,1:223)

# Transformar columnas (años) a fila
Pcountry = cbind(Y_tt[1],Ptrans)
Pcountry$indice = 1:nrow(Pcountry)
nuevo = gather(Pcountry,key = "año",value = "P",-"Country Name",-indice)
Pvertical = arrange(nuevo,indice)

MYcountry = cbind(Y_tt[1],M.Ytrans)
MYcountry$indice = 1:nrow(MYcountry)
nuevo = gather(MYcountry,key = "año",value = "M.Y",-"Country Name",-indice)
M.Yvertical = arrange(nuevo,indice)

# Unir Pvertical y MYvertical
P_M.Y = merge(M.Yvertical, Pvertical, 
                     by = c("Country Name", "indice", "año"))
P_M.Y$indice = NULL

# Transformación al 100


#################### TRANSFORMACIÓN DE VARIABLES II ############################

# Tasa de crecimiento geométrico para 5 años
M.TcrecGeom5 = tasaCrecGeom(M.Y_t,5)
# Tasa de crecimiento geométrico para 10 años
M.TcrecGeom10 = tasaCrecGeom(M.Y_t,10)
# Tasa de crecimiento geométrico para 25 años
M.TcrecGeom25 = tasaCrecGeom(M.Y_t,25)
# Tasa de crecimiento geométrico para 40 años
M.TcrecGeom40 = tasaCrecGeom(M.Y_t,40)



###################### BORRAR VARIABLES SIN USO ################################
rm(GDPreal)
rm(Y_tt)
rm(nuevo)
rm(Pcountry)
rm(M_t)
rm(M.Y_t)
rm(P_t)
rm(Y_t)
rm(Pvertical)
rm(M.Yvertical)
rm(MYcountry)
