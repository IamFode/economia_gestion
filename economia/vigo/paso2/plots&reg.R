#LIBRERIAS
library(tidyr)

# coeficiente de correlación
correlation = cor(P_M.Ymean$mediaM.Y,P_M.Ymean$mediaP,use = "pairwise.complete.obs")
reg = lm(mediaP~mediaM.Y,P_M.Ymean)
summary(reg)

# Plot
# Ajustar una regresión lineal
reg <- lm(mediaP ~ mediaM.Y, data = na.omit(P_M.Ymean))

# Crear un gráfico de dispersión con ggplot2
# Ajustar una regresión lineal
reg <- lm(mediaP ~ mediaM.Y, data = na.omit(P_M.Ymean))

# Crear un gráfico de dispersión con ggplot2
# Ajustar una regresión lineal
reg <- lm(mediaP ~ mediaM.Y, data = na.omit(P_M.Ymean))

# Crear un gráfico de dispersión con ggplot2
# Ajustar una regresión lineal
reg <- lm(mediaP ~ mediaM.Y, data = na.omit(P_M.Ymean))

# Calcular el coeficiente de correlación
correlation = round(cor(na.omit(P_M.Ymean$mediaM.Y), na.omit(P_M.Ymean$mediaP)), 2)

# Crear un gráfico de dispersión con ggplot2
ggplot(na.omit(P_M.Ymean), aes(x = mediaM.Y, y = mediaP)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Tasa de crecimiento del exceso de dinero") +
  ylab("Tasa de inflación")+
  annotate("text", x = min(na.omit(P_M.Ymean$mediaM.Y)), y = max(na.omit(P_M.Ymean$mediaP)), 
           label = paste0("Todos los paises","\n\n",
             "Intercepto = ", round(coef(reg)[1], 2), "\n",
                          "Pendiente = ", round(coef(reg)[2], 2), "\n",
                          "Rcuadrado = ", round(summary(reg)$r.squared, 2), "\n",
                          "Coef. corr. = ", round(correlation,2)),
           vjust = 1, hjust = 0) +
  theme_bw()+
  theme(plot.margin = unit(c(1, 1, 1, 3), "lines"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none",
        plot.caption = element_text(size = 10, hjust = 0))
#

ggplot(na.omit(P_M.Yvertical),aes(M.Y,P))+
  geom_point()
  
