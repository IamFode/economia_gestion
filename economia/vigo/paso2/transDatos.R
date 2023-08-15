
############################## FUNCIONES #######################################
source("paso2/functions.R")

############################## LIBRERIAS #######################################
library(readxl)
library(dplyr)


################################ DATA ##########################################
{GDPreal = read_excel("paso2/data/GDP-WB-IFS.xlsx", 
                         sheet = "GDPreal-IMF+WB")
P_t = read_excel("paso2/data/Prices-WB-IFS.xlsx", 
                            sheet = "GDPDefl(2010)WB+IMF")
M1 = read_excel("paso2/data/SeriesAnuales.xlsx", 
                    sheet = "M1")}


############################# VARIABLES ########################################
`"
P_tY_t = v_tM_t
   P_t = Indice de precios del deflactor
   Y_t = PIB real.
P_tY_t = PIB nominal.
   v_t = Velocidad del dinero.
   M_t = Un agregado monetario.
"`

############################# LIMPIEZA #########################################
{Y_t = slice(GDPreal,1:223)
M_t = slice(M1,1:223)}
`"
Al verificar P_t y Y_t, nos damos cuenta que se tiene datos desde el año 1948, 
por lo que decidimos eliminar los años que no contiene datos.
"`
{P_t = subset(P_t,select = -(6:which(names(P_t)=="1947")))
Y_t = subset(Y_t,select = -(6:which(names(Y_t)=="1947")))
M_t = subset(M_t,select = -ncol(M_t))}
`"
procedemos a cambiar los nombres de países de M_t, con las de P_t ya que difieren 
algunos países en su escritura. 
"`
M_t = cbind(P_t[,1], M_t[, -1])
`"
Eliminar las primera 5 columnas de cada variables, para una mejor práctica de 
las series. Esto ya que sólo contienen metadatos.
"`
{P_t = subset(P_t,select = -(1:5))
Y_t = subset(Y_t,select = -(1:5))
M_t = subset(M_t,select = -(1:5))}
`"
Cambiar todos los ceros por NA
"`
{{P_t[P_t==0] = NA
Y_t[Y_t==0] = NA
M_t[M_t==0] = NA}}
`"
M_t aún tenia datos (string), por lo que debemos tranformarlos a datos (numeric)
"`
M_t = as.data.frame(apply(M_t,c(1,2),as.numeric))
any(sapply(M_t, is.character))
`"
Agreguemos dos columnas a M_t ya que las otras variables tiene datos hasta los 
años 2019 y M_t tiene datos hasta 2017
"`
{M_t[,"2018"] <- NA
M_t[,"2019"] <- NA}

# Convertir P_t a dataframe
P_t=as.data.frame(P_t)

#################### TRANFORMACIÓN DE VARIABLES I ##############################
#i. inflación (ln(P_{t+1}/(P_t)))
Ptrans = lDif(P_t)

#i. Crecimiento excesivo de dinero ln((M_{t+1}/Y_{t+1})/(M_t/Y_t))
M.Y_t = M_t/Y_t
M.Ytrans = lDif(M.Y_t)

# Construcción de un dataframe con nombres de países
dfname = slice(GDPreal,1:223)

# De columnas a filas 
P_M.Y=ColToRow(dfname,Ptrans,M.Ytrans)

# Omisión de datos extremos según análisis.R
P_M.Y100per = P_M.Y[-c(821,823,825,10207),]

# Transformación en porcentajes (Dwyer and Fisher (2009, fig 3)) con relación
# a los países con una tasa de crecimiento del exceso de dinero.
MY=(max(P_M.Y100per$M.Y,na.rm = TRUE)-min(P_M.Y100per$M.Y,na.rm = TRUE))/2
P_M.Y50per = na.omit(P_M.Y100per) %>% filter(M.Y<MY)# Inferior al 50 %

MY=(max(P_M.Y100per$M.Y,na.rm = TRUE)-min(P_M.Y100per$M.Y,na.rm = TRUE))/5
P_M.Y20per = na.omit(P_M.Y100per) %>% filter(M.Y<MY)# Inferior al 20 %

MY=(max(P_M.Y100per$M.Y,na.rm = TRUE)-min(P_M.Y100per$M.Y,na.rm = TRUE))/10
P_M.Y10per = na.omit(P_M.Y100per) %>% filter(M.Y<MY)# Inferior al 10 %


#################### TRANSFORMACIÓN DE VARIABLES II ############################

M.YcrecGeom5 = tasaCrecGeom(M.Y_t,5)   # Tasa geométrica M/Y 5 años
M.YcrecGeom10 = tasaCrecGeom(M.Y_t,10) # Tasa geométrica M/Y 10 años
M.YcrecGeom25 = tasaCrecGeom(M.Y_t,25) # Tasa geométrica M/Y 25 años
M.YcrecGeom40 = tasaCrecGeom(M.Y_t,40) # Tasa geométrica M/Y 40 años

PcrecGeom5 = tasaCrecGeom(P_t,5)   # Tasa geométrica P  5 años
PcrecGeom10 = tasaCrecGeom(P_t,10) # Tasa geométrica P 10 años
PcrecGeom25 = tasaCrecGeom(P_t,25) # Tasa geométrica P 25 años
PcrecGeom40 = tasaCrecGeom(P_t,40) # Tasa geométrica P 40 años

P_M.Y5años=ColToRow(dfname,PcrecGeom5,M.YcrecGeom5)   # P|M/Y col to row 5 años
P_M.Y10años=ColToRow(dfname,PcrecGeom10,M.YcrecGeom10)# P|M/Y col to row 10 años
P_M.Y25años=ColToRow(dfname,PcrecGeom25,M.YcrecGeom25)# P|M/Y col to row 25 años
P_M.Y40años=ColToRow(dfname,PcrecGeom40,M.YcrecGeom40)# P|M/Y col to row 40 años


################### TRANSFORMACIÓN DE VARIABLES III ############################

P_M.Y_FI = as.data.frame(data_frame("Country Name"=dfname$`Country Name`,
                                    M.Y=lastFirst(M.Y_t),
                                    P=lastFirst(P_t)))


###################### BORRAR VARIABLES SIN USO ################################
{rm(M1)
rm(GDPreal)
rm(M_t)
rm(M.Y_t)
rm(P_t)
rm(Y_t)
rm(M.YcrecGeom5)
rm(M.YcrecGeom10)
rm(M.YcrecGeom25)
rm(M.YcrecGeom40)
rm(PcrecGeom5)
rm(PcrecGeom10)
rm(PcrecGeom25)
rm(PcrecGeom40)
rm(dfname)
rm(MY)
rm(M.Ytrans)
rm(Ptrans)
rm(P_M.Y)}
