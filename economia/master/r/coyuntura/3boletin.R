
library(ggplot2)
library(ggthemes)


########################### EJERCICIO 1
#alemania
df = read.csv("data/b3ej1.csv",sep = ',',encoding = "UTF-8")
df$Sex[df$Sex == "Men"] <- "Hombre"
df$Sex[df$Sex == "Women"] <- "Mujer"
ggplot(df, aes(x = Age, group = Sex)) +
  geom_smooth(aes(y = A2005,color=Sex,linetype="A2005"), linetype = "dashed") +
  geom_smooth(aes(y = A2021,color=Sex,linetype="A2021"), linetype = "solid") +
  
  geom_segment(x = 3.95, xend = 3.85, y =50, yend = 49.95,
              arrow = arrow(length = unit(0.3, "cm")),
              color = "black", size = 0.5) +
  annotate("text", x = 4, y = 50, label = "2005", color = "black", size = 4) +
  
  geom_segment(x = 4, xend = 3.9, y =64, yend = 63.95,
              arrow = arrow(length = unit(0.3, "cm")),
              color = "black", size = 0.5) +
  annotate("text", x = 4.05, y = 64, label = "2005", color = "black", size = 4) +
  
  geom_segment(x = 4, xend = 3.9, y =80, yend = 79.95,
              arrow = arrow(length = unit(0.3, "cm")),
              color = "black", size = 0.5) +
  annotate("text", x = 4.05, y = 80, label = "2021", color = "black", size = 4) +
  
  geom_segment(x = 4, xend = 3.9, y =71, yend = 70.95,
              arrow = arrow(length = unit(0.3, "cm")),
              color = "black", size = 0.5) +
  annotate("text", x = 4.05, y = 71, label = "2021", color = "black", size = 4) +
  
  labs(x = "Grupo de edad", y = "%", color = "Sexo:") +
  ggtitle("Tasa de participación en la fuerza laboral por sexo y grupo de edad para Alemania (2005 vs. 2021)") +
  theme_economist(base_size = 10) 

#españa
df = read.csv("data/b3ej1e.csv",sep = ',',dec = ',',encoding = "UTF-8")

ggplot(df, aes(x = Age, group = Sex)) +
  geom_smooth(aes(y = A2006,color=Sex,linetype="A2006"), linetype = "dashed",se=FALSE) +
  geom_smooth(aes(y = A2022,color=Sex,linetype="A2022"), linetype = "solid",se=FALSE) +
  
  annotate("text", x = 2.45, y = 20, label = "2022", color = "black", size = 3.8,angle=65) +
  
  annotate("text", x = 1.1, y = 23, label = "2022", color = "black", size = 3.8,angle=65) +
  
  annotate("text", x = 1.1, y = 40, label = "2006", color = "black", size = 3.9,angle=63) +
  
  annotate("text", x = 2, y = 30, label = "2006", color = "black", size = 3.9,angle=65) +
  
  labs(x = "Grupo de edad", y = "%", color = "Sexo:") +
  ggtitle("Tasa de actividad por sexo y grupo de edad para España (2006 vs. 2022)") +
  theme_economist(base_size = 10) 
##################################

#EJERCICIO 2
#españa
df = read.csv("data/b3ej2e.csv",sep = ',',dec = ',',encoding = "UTF-8")
ggplot(data = df, aes(x = año)) +
  geom_smooth(aes(y = total, color = "Total"), size = 1, method = "loess", se = FALSE) +
  geom_smooth(aes(y = male, color = "Hombres"), size = 1, linetype = "dashed", method = "loess", se = FALSE) +
  geom_smooth(aes(y = female, color = "Mujeres"), size = 1, linetype = "dotted", method = "loess", se = FALSE) +
  labs(title = "Evolución de la tasa de Actividad en España (1995-2022)",
       x = "Año",
       y = "%") +
  scale_color_manual(values = c("Total" = "blue", "Hombres" = "red", "Mujeres" = "purple")) +
  theme_economist(base_size = 10) 

#alemania
df = read.csv("data/b3ej2a.csv",sep = '\t',dec = '.',encoding = "UTF-8")
ggplot(data = df, aes(x = año)) +
  geom_smooth(aes(y = total, color = "Total"), size = 1, method = "loess", se = FALSE) +
  geom_smooth(aes(y = male, color = "Hombres"), size = 1, linetype = "dashed", method = "loess", se = FALSE) +
  geom_smooth(aes(y = female, color = "Mujeres"), size = 1, linetype = "dotted", method = "loess", se = FALSE) +
  labs(title = "Evolución de la tasa de Actividad en Alemania (1995-2022)",
       x = "Año",
       y = "%") +
  scale_color_manual(values = c("Total" = "blue", "Hombres" = "red", "Mujeres" = "purple")) +
  theme_economist(base_size = 10) 


#Ejercicio 3
#españa
df = read.csv("data/b3ej3e.csv",sep = ',',dec = ',',encoding = "UTF-8")
ggplot(data = df, aes(x = año)) +
  geom_smooth(aes(y = total, color = "Total"), size = 1, method = "loess", se = FALSE) +
  geom_smooth(aes(y = male, color = "Hombres"), size = 1, linetype = "dashed", method = "loess", se = FALSE) +
  geom_smooth(aes(y = female, color = "Mujeres"), size = 1, linetype = "dotted", method = "loess", se = FALSE) +
  labs(title = "Evolución de la tasa de Empleo en España (2005-2022)",
       x = "Año",
       y = "%") +
  scale_color_manual(values = c("Total" = "blue", "Hombres" = "red", "Mujeres" = "purple")) +
  theme_economist(base_size = 10) 

#alemania
df = read.csv("data/b3ej3a.csv",sep = ',',dec = ',',encoding = "UTF-8")
ggplot(data = df, aes(x = año)) +
  geom_smooth(aes(y = total, color = "Total"), size = 1, method = "loess", se = FALSE) +
  geom_smooth(aes(y = male, color = "Hombres"), size = 1, linetype = "dashed", method = "loess", se = FALSE) +
  geom_smooth(aes(y = female, color = "Mujeres"), size = 1, linetype = "dotted", method = "loess", se = FALSE) +
  labs(title = "Evolución de la tasa de Empleo en Alemania (2005-2022)",
       x = "Año",
       y = "%") +
  scale_color_manual(values = c("Total" = "blue", "Hombres" = "red", "Mujeres" = "purple")) +
  theme_economist(base_size = 10) 

#EJERCICIO 4
#españa
df = read.csv("data/b3ej4e.csv",sep = ',',dec = '.',encoding = "UTF-8")
ggplot(data = df, aes(x = año)) +
  geom_smooth(aes(y = total, color = "Total"), size = 1, method = "loess", se = FALSE) +
  geom_smooth(aes(y = male, color = "Hombres"), size = 1, linetype = "dashed", method = "loess", se = FALSE) +
  geom_smooth(aes(y = female, color = "Mujeres"), size = 1, linetype = "dotted", method = "loess", se = FALSE) +
  labs(title = "Evolución de la tasa de paro en España (1990-2022)",
       x = "Año",
       y = "%") +
  scale_color_manual(values = c("Total" = "blue", "Hombres" = "red", "Mujeres" = "purple")) +
  theme_economist(base_size = 10) 

#alemania
df = read.csv("data/b3ej4a.csv",sep = ',',dec = '.',encoding = "UTF-8")
ggplot(data = df, aes(x = año)) +
  geom_smooth(aes(y = total, color = "Total"), size = 1, method = "loess", se = FALSE) +
  geom_smooth(aes(y = male, color = "Hombres"), size = 1, linetype = "dashed", method = "loess", se = FALSE) +
  geom_smooth(aes(y = female, color = "Mujeres"), size = 1, linetype = "dotted", method = "loess", se = FALSE) +
  labs(title = "Evolución de la tasa de paro en Alemania (1990-2022)",
       x = "Año",
       y = "%") +
  scale_color_manual(values = c("Total" = "blue", "Hombres" = "red", "Mujeres" = "purple")) +
  theme_economist(base_size = 10) 


#EJERCICIO 5
df = read.csv("data/b3ej5.csv",sep = '\t',dec = '.',encoding = "UTF-8")

library(ggplot2)

# Crear el gráfico de líneas suavizadas
ggplot(df, aes(x = año)) +
  geom_smooth(aes(y = Alemania, color = "Alemania"), size = .7, se = FALSE) +
  geom_smooth(aes(y = España, color = "España"), size = .7, se = FALSE) +
  geom_smooth(aes(y = `Unión.Europea..27.`, color = "Unión Europea 27"), size = 2, se = FALSE) +
  geom_smooth(aes(y = Francia, color = "Francia"), size = .7, se = FALSE) +
  geom_smooth(aes(y = Portugal, color = "Portugal"), size = .7, se = FALSE) +
  labs(title = "Porcentaje de contratos temporales", x = "Año", y = "%", color = "País: ") +
  theme_economist(base_size = 10)
