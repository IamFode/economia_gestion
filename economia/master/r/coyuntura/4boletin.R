
#librerias
library(dplyr)
library(ggplot2)
library(ggthemes)

#data
pib_r = read.csv("data/b4ej1e.csv",dec = ",")
pib_r = read.csv("data/b4ej1a.csv",dec = ",")
#clear
pib_r = pib_r %>% arrange(año)

#Crecimiento del PIB años 2002-2022
tasaCrecimiento <- function(df,var,i,f){
  n=f-i
  valor=(((df[df$año==f,var]/df[df$año==i,var])^(1/n)-1))*100
  return(valor)
}

#alpha
alpha <- function(df,i,f) {
  suma_alpha <- 0
  for (año in i:f) {
    renta <- df[df$año==año,"renta_salariales"]
    pib <- df[df$año==año,"pib"]
    alpha <- renta/pib
    suma_alpha <- suma_alpha+alpha
  }
  alpha_promedio <- suma_alpha/(f-i+1)
  return(alpha_promedio)
}

i = 2002
f = 2022

alfa=alpha(pib_r,i,f)

y=tasaCrecimiento(pib_r,"pib",i,f)
k=tasaCrecimiento(pib_r,"capital",i,f)*alfa
l=tasaCrecimiento(pib_r,"ocupados",i,f)*(1-alfa)
y
k
l
y-k-l


#EJERCICIO 2
pib = read.csv("data/b4ej2_1.csv",dec = ",")
ggplot(pib, aes(x = año)) +
  geom_line(aes(y = Alemania, color = "Alemania"), size = .7, se = FALSE) +
  geom_line(aes(y = España, color = "España"), size = .7, se = FALSE) +
  geom_line(aes(y = `Union.Europea...27`, color = "Unión Europea 27"), size = 1, se = FALSE) +
  labs(x = "Años", y = "%", color = "Lugar:") +
  ggtitle("Tasa de crecimiento del PIB (2011 vs. 2022)") +
  scale_x_continuous(breaks = seq(2011, 2022, by = 1)) +
  theme_economist(base_size = 10)

