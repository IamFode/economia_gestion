#..............................................................................#
# FUNCIONES
source("functions_clean.R")

#LIBRERIAS
library(dplyr)
library(openxlsx)
library(forcats)

################################## DATA ########################################
series = read.csv("timeSeries.csv",check.names=FALSE)
pais = read.csv("paises.csv",check.names = FALSE)

########################### LIMPIEZA DE DATOS ##################################
`"
Limpiemos todas las columnas con datos trimestrales o mensuales. En este caso,
Todas las columnas que en su nombre contengan la letra mayuscula M o Q y que a 
priori o posteriori contenga algún digito (\\d)
"`
#Series anuales
serie = subset(series,select = -grep("\\d[MQ]\\d",
                                    colnames(series),
                                    perl = TRUE,
                                    ignore.case = FALSE))

`"
Observemos cuantos datos NA y faltantes existen en cada columna y reemplazemosla
por NA
"`
serie[is.na(serie)] = NA
serie[serie==""] = NA


#Series trimestrales
columnasQ = grep("Q",names(series),value = TRUE)
serieQ = series[columnasQ]
serieQ = cbind(series[1:5],serieQ)

serieQ[is.na(serieQ)] = NA
serieQ[serieQ==""] = NA
serieQ[, 6:length(serieQ)] = sapply(serieQ[, 6:length(serieQ)], 
                                    function(col) replace(col, is.character(col), NA))

#Series mensuales
columnasM = grep("M",names(series),value = TRUE)
serieM = series[columnasM]
serieM = cbind(series[1:5],serieM)

serieM[is.na(serieM)] = NA
serieM[serieM==""] = NA
serieM[, 6:length(serieM)] = sapply(serieM[, 6:length(serieM)], 
                                    function(col) replace(col, is.character(col), NA))

#general
serie = serieM
rm(series)
rm(serieM)

`"
Verificamos que esten todos los años/trimestres/meses en el intervalo [1948,2017]
"`
colnames(serie)


######################## CLASIFICACIÓN DE DATOS ################################
`" 
Veamos todos los codigos que se encuentran 
"`
unique(serie$`Indicator Code`)

##### Currency-IFS #####
currency.IFS = c("FASMBC_XDC",
               "FASMBC_USD",
               "FASMBC_EUR")
x=clean(serie,currency.IFS) # para datos anuales
x=clean2(serie,currency.IFS) # para datos trimestrales y mensuales
currency_IFS = left_join(pais, x, by = c("Country Name","Country Code"))


##### RM-Currency #####
RM.currency = c("14A___XDC",
               "14A___USD")
x=clean(serie,RM.currency) # para datos anuales
x=clean2(serie,RM.currency) # trimetral y mensual
RM_currency = left_join(pais,x,by = c("Country Name","Country Code"))


##### BM-IFS #####
BM.IFS = c("FASMB_XDC",
          "FASMBC_EUR",
          "FMA_USD", #Solo en el caso de que no haya datos para FASMB, o la serie sea idéntica a FASMB pero más larga.
          "FMA_XDC", #Solo en el caso de que no haya datos para FASMB, o la serie sea idéntica a FASMB pero más larga.
          "FM0_XDC") #Solo en el caso de que no haya datos para FASMB, o la serie sea idéntica a FASMB pero más larga.
x=clean(serie,BM.IFS) # para datos anuales
x=clean2(serie,BM.IFS) # trimentral y mensual
BM_IFS = left_join(pais,x,by = c("Country Name","Country Code"))
BM_IFS = eliminar_fila_BM.IFS(BM_IFS,"FASMB_XDC")


##### BM-Nac-IFS #####
BM.Nac.IFS = c("FMA_USD",
               "FMA_XDC",
               "FM0_XDC")
x=clean(serie,BM.Nac.IFS) # para datos anuales
x=clean2(serie,BM.Nac.IFS) # Para datos Trimestral y Mensual
BM_Nac_IFS = left_join(pais,x,by = c("Country Name","Country Code"))


##### M1 #####
M1. = c("FM1_XDC",
      "FMN_XDC",
      "FMN_USD",
      "FM1_A1_XDC",
      "FM1_A2_XDC",
      "FM1_A3_XDC",
      "FM1_EUR",
      "FM1_USD",
      "FMM_XDC") 
x=clean(serie,M1.) # para datos anuales
x=clean2(serie,M1.) #Para datos trimestrales y mensuales
M1 = left_join(pais,x,by = c("Country Name","Country Code"))
M1 = eliminar_filas_duplicadas(M1,c("FM1_XDC", "FM1_A1_XDC", "FM1_A2_XDC", "FM1_A3_XDC"))


##### M2-Broad Money - IFS #####
M2.BroadMoney.IFS = c("FDSB_XDC",
                      "FDSBC_EUR",
                      "FMB_USD")
x=clean(serie,M2.BroadMoney.IFS) # para datos anuales
x=clean2(serie,M2.BroadMoney.IFS)#Para datos trimestrales y mensuales
M2_BroadMoney_IFS = left_join(pais,x,by = c("Country Name","Country Code"))


##### M2-Nac #####
M2.Nac = c("FM2_XDC",
           "FM2_A1_XDC",
           "FM2_USD",
           "FM2_EUR")
x=clean(serie,M2.Nac) # para datos anuales
x=clean2(serie,M2.Nac) #Para datos Trimestrales y mensuales
M2_Nac = left_join(pais,x,by = c("Country Name","Country Code"))
M2_Nac = eliminar_filas_duplicadas(M2_Nac,c("FM2_XDC", "FM2_A1_XDC"))

##### M3 #####
M3. = c("FM3_XDC")
x=clean(serie,M3.) # para datos anuales
x=clean2(serie,M3.)#Para datos trimestrales y mensuales
M3 = left_join(pais,x,by = c("Country Name","Country Code"))


##### M4 #####
M4. = c("FM4_XDC",
        "FM4_A1_XDC",
        "FM4_A2_XDC",
        "FM4_A3_XDC")
x=clean(serie,M4.) # para datos anuales
x=clean2(serie,M4.) #Para datos trimestrales y mensuales
M4 = left_join(pais,x,by = c("Country Name","Country Code"))
M4 = eliminar_filas_duplicadas(M4,M4.)

##### M5 #####
M5. = c("FMA_XDC",
        "FM5B_XDC",
        "FM5_XDC")
x=clean(serie,M5.) # para datos anuales
x=clean2(serie,M5.) #Para datos trimestrales y mensuales
M5 = left_join(pais,x,by = c("Country Name","Country Code"))
M5 = eliminar_filas_duplicadas(M5,M5.)

##### RR #####
RR. = c("FOSAAR_XDC",
        "FOSAAR_EUR",
        "FOSAAR_USD")
x=clean(serie,RR.) # para datos anuales
x=clean2(serie,RR.) #Para datos trimestrales y mensuales
RR = left_join(pais,x,by = c("Country Name","Country Code"))

##### RR2 #####
RR2=RR

################################## EXPORT ######################################
# Crear un objeto de libro de Excel
wb <- createWorkbook()

# Definir el estilo de celda numérico y editable
numericFormat <- createStyle(numFmt = "General", locked = FALSE)

# Lista de hojas de datos y dataframes correspondientes
hojas_data <- c("Currency-IFS", "RM-Currency", "BM-IFS", "BM-Nac-IFS", "M1", 
                "M2-Broad Money-IFS", "M2-Nac", "M3", "M4", "M5", "RR", "RR2")
dataframes <- list(currency_IFS, RM_currency, BM_IFS, BM_Nac_IFS, M1, 
                   M2_BroadMoney_IFS, M2_Nac, M3, M4, M5, RR, RR2)

# Iterar sobre las hojas de datos y dataframes
for (i in 1:length(hojas_data)) {
  hoja <- hojas_data[i]
  df <- dataframes[[i]]
  
  # Agregar hoja de datos al libro
  addWorksheet(wb, hoja)
  
  # Escribir los datos en la hoja
  writeData(wb, hoja, df, startCol = 1, startRow = 1)
  
  # Aplicar el estilo numérico y editable a las celdas a partir de la columna 6
  cols <- 6:ncol(df)
  for (col in cols) {
    addStyle(wb, sheet = hoja, style = numericFormat, rows = NULL, cols = col)
  }
}


# Guardar el archivo de Excel
saveWorkbook(wb,"SeriesAnuales.xlsx",overwrite = TRUE)
saveWorkbook(wb,"SeriesTrimestrales2.xlsx",overwrite = TRUE)
saveWorkbook(wb,"SeriesMensuales.xlsx",overwrite = TRUE)
