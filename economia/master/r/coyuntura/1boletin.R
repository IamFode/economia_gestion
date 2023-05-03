
# ALEMANIA

# EJERCICIO 1
library(readxl)
library(dplyr)
library(tidyverse)

# Escenario 2021 2022
escenario = read_excel("data/escenarioEconomico.xlsx")
df = escenario[,c("Variable",
                                  "2022",
                                  "2021",
                                  "2020")]
rm(escenario)
df = df[-3,]
df=df %>%
  remove_rownames() %>%
  column_to_rownames(var="Variable")

# Saldo exterior
n = df[1,]-df[7,]
rownames(n)<-"saldoExterior"
df = rbind(df,n)

# Demanda nacional
n = df[8,]+df[9,]+df[6,]
rownames(n)<-"demandaNacional"
df = rbind(df,n)

# Contribución del saldo Exterior al crecimiento del PIB 
n=df[10,]/df[2,]
rownames(n) = "partsaldoExterior"
df = rbind(df,n)

# Contribución de la demanda nacional al crecimiento del PIB 
n=df[11,]/df[2,]
rownames(n) = "partdemandaNacional"
df = rbind(df,n)


# calculo %
df["var2022"]=(df$"2022"/df$"2021"-1)*100
df["var2021"]=(df$"2021"/df$"2020"-1)*100



#EJERCICIO 2
df["contribPIB2022"] = df$"2022"*df$var2022
df["contribPIB2021"] = df$"2021"*df$var2021


#EJERCICIO 3
ej2 = read.csv("data/b1ej2.csv")
ej2=ej2 %>%
  remove_rownames() %>%
  column_to_rownames(var="X")

ej2["crec2021Q1"] = round((ej2[,5]/ej2[,1]-1)*100,1)
ej2["crec2021Q2"] = round((ej2[,6]/ej2[,2]-1)*100,1)
ej2["crec2021Q3"] = round((ej2[,7]/ej2[,3]-1)*100,1)
ej2["crec2021Q4"] = round((ej2[,8]/ej2[,4]-1)*100,1)

ej2["crec2022Q1"] = round((ej2[,9]/ej2[,5]-1)*100,1)
ej2["crec2022Q2"] = round((ej2[,10]/ej2[,6]-1)*100,1)
ej2["crec2022Q3"] = round((ej2[,11]/ej2[,7]-1)*100,1)
ej2["crec2022Q4"] = round((ej2[,12]/ej2[,8]-1)*100,1)

# Demanda nacional, contribución al crecimiento del PIB.
participacion = ej2[1,1:12]/ej2[12,1:12]
round((ej2[1,13:20])*participacion[1:8],1)

# Saldo exterior, contribución al crecimiento del PIB.
participacion = ej2[9,1:12]/ej2[12,1:12]
round((ej2[9,13:20])*participacion[1:8],1)
