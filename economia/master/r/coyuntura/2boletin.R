
#librerias
library(dplyr)
library(ggplot2)
library(ggthemes)
library(zoo)
library(readxl)

#EJERCICIO 5
# DATA
df = read.csv("data/b2ej5.csv",sep = ',',encoding = "UTF-8")

df$Edad = as.numeric(gsub(" años","",df$Edad))

df$Sexo = NULL
df$Provincias = NULL
df$Periodo <- as.Date(strptime(df$Periodo, "%d de %B de %Y"))

df = aggregate(Total~Periodo, data=df,sum)
apply(df$Total,1, function(x) (x*100)/lag(x))

lag(df$Total,1)
tasa=(diff(df$Total) / df$Total[-length(df$Total)]) * 100
fecha = df$Periodo[-1]
df2 = data.frame(fecha,tasa)

ggplot(df2, aes(x = fecha, y = tasa)) +
  geom_bar(stat = "identity", fill = "#FF6F61") +
  labs(x = "Periodo (Semestral)", y = "Tasa de crecimiento") +
  ggtitle("TASA DE CRECIMIENTO DE LA POBLACIÓN EN ESPAÑA DE 16 y MÁS AÑOS") +
  scale_x_date(date_breaks = "6 months") +
  theme_economist(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1,vjust = 1.2, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )

#alemania
df = read.csv("data/b2ej5a.csv",sep = ',',encoding = "UTF-8")
colnames(df) = c("Periodo","Total")
df$Periodo = as.Date(df$Periodo)
((df$Total[4]/df$Total[3])-1)*100
tasa=(diff(df$Total) / df$Total[-length(df$Total)]) * 100
fecha = df$Periodo[-1]
df2 = data.frame(fecha,tasa)

ggplot(df2, aes(x = fecha, y = tasa)) +
  geom_bar(stat = "identity", fill = "#FF6F61") +
  labs(x = "Periodo (Trimestral) desde 2005", y = "Tasa de crecimiento") +
  ggtitle("TASA DE CRECIMIENTO DE LA POBLACIÓN EN ALEMANIA DE 16 a 64 AÑOS") +
  scale_x_date(date_breaks = "3 months") +
  theme_economist(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1,vjust = 1.2, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )


# EJERCICIO 6
paro = read.csv("data/b2ej6.csv",dec = ",")
paro$tasa = rev(paro$tasa)
paro=ts(paro$tasa,start = c(2001,2),frequency = 4)

paro_zoo <- as.zoo(paro)

# Crear un data frame con las fechas y valores
df <- data.frame(Index = index(paro_zoo), Value = coredata(paro_zoo))

# Graficar la serie de tiempo con ggplot2
ggplot(df, aes(x = Index, y = Value)) +
  geom_line(color = "#FF6F61",size=1) +
  labs(x = "Trimestral", y = "Tasa de desempleo") +
  ggtitle("Tasa de desempleo trimestral en España (1T2001 al 1T2023)") +
  theme_economist()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1,vjust = 1.2, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )+
  scale_x_yearqtr(format = "T%q %Y",n=length(unique(df$Index)))

#alemania
paro = read_excel("data/b2ej6a.xlsx")
paro=ts(paro$Total,start = c(2007,1),frequency = 4)

paro_zoo <- as.zoo(paro)

# Crear un data frame con las fechas y valores
df <- data.frame(Index = index(paro_zoo), Value = coredata(paro_zoo))

# Graficar la serie de tiempo con ggplot2
ggplot(df, aes(x = Index, y = Value)) +
  geom_line(color = "#FF6F61",size=1) +
  labs(x = "Trimestral", y = "Tasa de desempleo") +
  ggtitle("Tasa de desempleo trimestral en Alemania (1T2007 al 1T2023)") +
  theme_economist()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1,vjust = 1.2, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )+
  scale_x_yearqtr(format = "T%q %Y",n=length(unique(df$Index)))


#EJERCICIO 7a
asalariados_completo = read.csv("data/b2ej7a.csv",dec = ";")
asalariados_completo$Total = rev(asalariados_completo$Total)
asalariados_completo$Total = (asalariados_completo$Total/sum(asalariados_completo$Total))*1000
asalariados_completo=ts(asalariados_completo$Total,start = c(2002,1),frequency = 4)


df <- data.frame(Index = index(asalariados_compleot_zoo),
                 Value = coredata(asalariados_compleot_zoo))
# Graficar la serie de tiempo con ggplot2
ggplot(df, aes(x = Index, y = Value)) +
  geom_line(color = "#FF6F61",size=1) +
  labs(x = "Trimestral", y = "Porcentaje") +
  ggtitle("Porcentaje de asalariados con contrato a tiempo completo en España (1T2002 al 1T2023)") +
  theme_economist()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1,vjust = 1.2, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )+
  scale_x_yearqtr(format = "T%q %Y",n=length(unique(df$Index)))


#EJERCICIO 7b
asalariados_completo = read.csv("data/b2ej7b.csv",dec = ";")
asalariados_completo$Total = rev(asalariados_completo$Total)
asalariados_completo=ts(asalariados_completo$Total,start = c(2002,1),frequency = 4)

asalariados_compleot_zoo <- as.zoo(asalariados_completo)

df <- data.frame(Index = index(asalariados_compleot_zoo),
                 Value = coredata(asalariados_compleot_zoo))

# Graficar la serie de tiempo con ggplot2
ggplot(df, aes(x = Index, y = Value)) +
  geom_line(color = "#FF6F61",size=1) +
  labs(x = "Trimestral", y = "Porcentaje") +
  ggtitle("Porcentaje de asalariados con contrato indefinido en España (1T2002 al 1T2023)") +
  theme_economist()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1,vjust = 1.2, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )+
  scale_x_yearqtr(format = "T%q %Y",n=length(unique(df$Index)))


#EJERCICIO 7c
parados = read.csv("data/b2ej7c.csv",dec = ";")
parados$total = rev(parados$total)
parados = ts(parados,start = c(2002,1),frequency = 4)

parados_zoo <- as.zoo(parados)

df <- data.frame(Index = index(parados_zoo),
                 Value = coredata(parados_zoo))

ggplot(df, aes(x = Index, y = total)) +
  geom_line(color = "#FF6F61",size=1) +
  labs(x = "Trimestral", y = "Porcentaje") +
  ggtitle("Porcentaje de parados que buscan empleo desde hace más de un año en España(1T2002 al 1T2023)") +
  theme_economist()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1,vjust = 1.2, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )+
  scale_x_yearqtr(format = "T%q %Y",n=length(unique(df$Index)))


#EJERCICIO 8b
porcentaje_empleo = read_excel("data/b2ej8b.xlsx")
colnames(porcentaje_empleo) = c("pais","porcentaje")
porcentaje_empleo = arrange(porcentaje_empleo,desc(porcentaje))

ggplot(porcentaje_empleo, aes(x = porcentaje, y = reorder(pais,porcentaje))) +
  geom_bar(stat = "identity", fill = "#FF6F61") +
  labs(x = "%", y = "País") +
  ggtitle("Porcentaje de empleo temporal. Cuarto trimestre del 2022") +
  theme_economist(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )+
  geom_vline(xintercept = 13.5,color="blue",)+
  annotate("text", x = 13.5, 
           y = max(porcentaje_empleo$porcentaje), 
           label = "Unión Europea 13.5%", 
           vjust = 40, 
           hjust = -0.2, 
           color = "blue")

