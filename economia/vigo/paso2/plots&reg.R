
#FUNCIONES
source("paso2/functions.R")


############## I Análisis Dwyer and Fisher (2009, gráfico 3) ####################
data_list = list(P_M.Y100per,P_M.Y50per,P_M.Y20per,P_M.Y10per)

titulo = list("Todos los países",
              "Crecimiento del excedente de dinero \nmenor al 50 %",
              "Crecimiento del excedente de dinero \nmenor al 20 %",
              "Crecimiento del excedente de dinero \nmenor al 10 %"
              )

plotsIyII(data_list,titulo)

######## II Periodos amplios para todos los datos de todos los países ##########
data_list = list(P_M.Y5años,P_M.Y10años,P_M.Y25años,P_M.Y40años)

titulo = list("Todos los años",
              "Diez años",
              "Veinticinco años",
              "Cuarenta años"
              )

plotsIyII(data_list,titulo)


##### III Ultimo y primer dato. Análisis Dwyer and Fisher (2009, gráfico 4) #####

plotIII(P_M.Y_FI)


################################ REMOVE ########################################
rm(data_list)
rm(titulo)
