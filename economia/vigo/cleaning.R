#..............................................................................#
############################## FUNCTIONS #######################################
'"
Esta función filtra los datos por codigos financieros para luego borrar los
países que se repiten y que no tienen datos, para luego itera sobre todos los 
grupos de codigos financieros y los une en un solo dataframe.
"'
clean = function(dataset, condition_list) {
  df = data.frame()
  filtrar_datos = function(dataset, condition) {
    x = dataset[dataset$`Indicator Code` == condition, ]
    i = length(unique(x$`Country Name`))
    x = x[apply(x[, 6:ncol(x)], 1, function(x) any(!is.na(x))) & !duplicated(x$`Country Code`), ]
    f = length(x$`Country Name`)
    if (i == f) {
      df <<- rbind(df, x)
    }
  }
  for (condition in condition_list) {
    filtrar_datos(dataset, condition)
  }
  return(df)
}

################################· LIBRARY ######################################
library(dplyr)
library(openxlsx)

################################## DATA ########################################
serie = read.csv("timeSeries.csv",check.names=FALSE)
pais = read.csv("paises.csv",check.names = FALSE)

########################### LIMPIEZA DE DATOS ##################################
`"
Limpiemos todas las columnas con datos trimestrales o mensuales. En este caso,
Todas las columnas que en su nombre contengan la letra mayuscula M o Q y que a 
priori o posteriori contenga algún digito (\\d)
"`
serie = subset(serie,select = -grep("\\d[MQ]\\d",
                                    colnames(serie),
                                    perl = TRUE,
                                    ignore.case = FALSE))
`"
Verificamos que esten todos los años en el intervalo [1948,2017]
"`
colnames(serie)

`"
Observemos cuantos datos NA y faltantes existen en cada columna y reemplazemosla
por NA
"`
colSums(is.na(serie)) #NA's
colSums(serie=="") #" "
serie[is.na(serie)] = NA
serie[serie==""] = NA

######################## CLASIFICACIÓN DE DATOS ################################
`" 
Veamos todos los codigos que se encuentran 
"`
unique(serie$`Indicator Code`)

########## CIRCULATION ##########
##### Currency-IFS #####
currency.IFS = c("FASMBC_XDC",
               "FASMBC_USD",
               "FASMBC_EUR")
x=clean(serie,currency.IFS)
currency_IFS = left_join(pais, x, by = c("Country Name","Country Code"))

##### RM-Currency #####
RM.currency = c("14A___XDC",
               "14A___USD")
x=clean(serie,RM.currency)
RM_currency = left_join(pais,x,by = c("Country Name","Country Code"))

##### BM-IFS #####
BM.IFS = c("FASMB_XDC",
          "FASMBC_EUR",
          "FMA_USD", #Solo en el caso de que no haya datos para FASMB, o la serie sea idéntica a FASMB pero más larga.
          "FMA_XDC", #Solo en el caso de que no haya datos para FASMB, o la serie sea idéntica a FASMB pero más larga.
          "FM0_XDC") #Solo en el caso de que no haya datos para FASMB, o la serie sea idéntica a FASMB pero más larga.
x=clean(serie,BM.IFS)
BM_IFS = left_join(pais,x,by = c("Country Name","Country Code"))


##### BM-Nac-IFS #####
BM.Nac.IFS = c("FMA_USD",
               "FMA_XDC",
               "FM0_XDC")
x=clean(serie,BM.Nac.IFS)
BM_Nac_IFS = left_join(pais,x,by = c("Country Name","Country Code"))

##### M1 #####
M1. = c("FM1_XDC",
      "FDSBC_XDC",
      "FDSBT_XDC",
      "FMN_XDC",
      "FMN_USD",
      "FM1_A1_XDC",
      "FM1_A2_XDC",
      "FM1_A3_XDC",
      "FM1_EUR",
      "FM1_USD",
      "FMM_XDC") 
x=clean(serie,M1.)
M1 = left_join(pais,x,by = c("Country Name","Country Code"))

##### M2-Broad Money - IFS #####
M2.BroadMoney.IFS = c("FDSB_XDC",
                      "FMB_XDC",
                      "FDSBC_EUR",
                      "FMB_EUR",
                      "FMB_USD",
                      "FDSB_USD")
x=clean(serie,M2.BroadMoney.IFS)
M2_BroadMoney_IFS = left_join(pais,x,by = c("Country Name","Country Code"))

##### M2-Nac #####
M2.Nac = c("FM2_XDC",
           "FM2_A1_XDC",
           "FM2_A1_XDC",
           "FM2_USD",
           "FM2_EUR")
x=clean(serie,M2.Nac)
M2_Nac = left_join(pais,x,by = c("Country Name","Country Code"))

##### M3 #####
M3. = c("FM3_XDC")
x=clean(serie,M3.)
M3 = left_join(pais,x,by = c("Country Name","Country Code"))

##### M4 #####
M4. = c("FM4_XDC",
        "FM4_A1_XDC",
        "FM4_A2_XDC",
        "FM4_A3_XDC")
x=clean(serie,M4.)
M4 = left_join(pais,x,by = c("Country Name","Country Code"))

##### M5 #####
M5. = c("FMA_XDC",
        "FM5B_XDC",
        "FM5_XDC")
x=clean(serie,M5.)
M5 = left_join(pais,x,by = c("Country Name","Country Code"))

##### RR #####
RR. = c("FOSAAR_XDC",
        "FOSAAR_EUR",
        "FOSAAR_USD")
x=clean(serie,RR.)
RR = left_join(pais,x,by = c("Country Name","Country Code"))

##### RR2 #####

################################## EXPORT ######################################
wb = createWorkbook()
addWorksheet(wb,"Currency-IFS")
writeData(wb,"Currency-IFS",currency_IFS)
saveWorkbook(wb,"timeSeries.xlsx",overwrite = TRUE)
