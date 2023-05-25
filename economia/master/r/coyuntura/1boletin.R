
# ALEMANIA

# EJERCICIO 1
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(reshape2)
library(viridis)

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


#######################
#######################
# Escenario 200-2022
df00.22 = escenario[,c("Variable",seq("1999","2022"))]
df00.22 = df00.22[-3,]
df00.22=df00.22 %>%
  remove_rownames() %>%
  column_to_rownames(var="Variable")

# Saldo exterior
n = df00.22[1,]-df00.22[7,]
rownames(n)<-"saldoExterior"
df00.22 = rbind(df00.22,n)

# Demanda nacional
n = df00.22[8,]+df00.22[9,]+df00.22[6,]
rownames(n)<-"demandaNacional"
df00.22 = rbind(df00.22,n)

# Contribución del saldo Exterior al crecimiento del PIB 
n=df00.22[10,]/df00.22[2,]
rownames(n) = "partsaldoExterior"
df00.22 = rbind(df00.22,n)

# Contribución de la demanda nacional al crecimiento del PIB 
n=df00.22[11,]/df00.22[2,]
rownames(n) = "partdemandaNacional"
df00.22 = rbind(df00.22,n)


agregar_variables <- function(df, inicio, final){
  for(i in inicio:final){
    var_name <- paste0("var", i)
    var_value <- (df[,as.character(i)]/df[,as.character(i-1)]-1)*100
    df[var_name] <- var_value
  }
  return(df)
}

df00.22Var = agregar_variables(df00.22,2000,2022)
df00.22Var = df00.22Var[,-(1:24)]
# Media 2000-2022
rowMeans(df00.22Var)
# 2021 y 2022
n_col = ncol(df00.22Var)
subset(df00.22Var,select = (n_col-1):n_col)

add_contribPIB <- function(df, inicio, final) {
  for (i in inicio:final) {
    var_name <- paste0("var", i)
    pib_name <- paste0("contribPIB", i)
    df[pib_name] <- df[[as.character(i)]] * df[[var_name]]
  }
  return(df)
}

df00.22Var = agregar_variables(df00.22,2000,2022)
df00.22Contr = add_contribPIB(df00.22Var,2000,2022)
df00.22Contr = df00.22Contr[,-(1:47)]
# Media 2000-2022 COntribucion al PIB
rowMeans(df00.22Contr)
subset(df00.22Contr,select = (n_col-1):n_col)




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


###################################
#EJERCICIO 6
###################################

b1ej6 = read_excel("data/b1ej6.xlsx")

b1ej6=b1ej6 %>%
  remove_rownames() %>%
  column_to_rownames(var="TIME")

senda = apply(b1ej6,1,function(x) (x*100)/lag(x))
senda=t(senda)
senda[,1] = ifelse(is.na(senda[,1]),100,senda[,1])
df=senda


# Convertir el dataframe a un formato de datos largo utilizando la función melt() de la biblioteca reshape2
df_largo <- melt(df, id.vars = "Pais", variable.name = "Trimestre", value.name = "Valor")
colnames(df_largo)=c("País","Var2","Valor")

# Graficar cada fila en un solo gráfico de líneas
ggplot(data = df_largo, aes(x = Var2, y = Valor, color = País, group = País)) +
  geom_line(size = .37,alpha=.3) +
  labs(x = "Trimestres", 
       y = "Volúmen encadenado [2015]", 
       title = "Senda de recuperación económica europa",
       subtitle = "Evolución trimestral del PIB\nCuarto trimestre de 2019=base 100\nLa última cifra corresponde al cuarto trimestre del 2022") +
  theme_minimal()+
  theme(plot.subtitle = element_text(size=7),
        strip.text = element_blank())+
  geom_text(data = subset(df_largo, País %in% c("Germany",País="Spain") & Var2 != unique(Var2[País %in% c("Germany", "Spain")])[1]), aes(label = País),
            alpha = 1, size = 2.5,angle=30)
