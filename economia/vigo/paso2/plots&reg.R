#LIBRERIAS
library(tidyr)
library(ggpubr)
library(dplyr)
library(MASS)

############## Gráficos (Análisis Dwyer and Fisher (2009, fig 3)) ################

# Inferior al 50 %
MY=(max(P_M.Yextremo$M.Y,na.rm = TRUE)-min(P_M.Yextremo$M.Y,na.rm = TRUE))/2
P_M.Y50 = na.omit(P_M.Yextremo) %>% filter(M.Y<MY)
# Inferior al 20 %
MY=(max(P_M.Yextremo$M.Y,na.rm = TRUE)-min(P_M.Yextremo$M.Y,na.rm = TRUE))/5
P_M.Y20 = na.omit(P_M.Yextremo) %>% filter(M.Y<MY)
# Inferior al 10 %
MY=(max(P_M.Yextremo$M.Y,na.rm = TRUE)-min(P_M.Yextremo$M.Y,na.rm = TRUE))/10
P_M.Y10 = na.omit(P_M.Yextremo) %>% filter(M.Y<MY)

#PLOTS
axis_labels <- list(# Crear una lista de etiquetas de eje
  list(x = "", y = "Tasa de inflación"),
  list(x = "", y = ""),
  list(x = "Tasa de crecimiento del exceso de dinero", y = "Tasa de inflación"),
  list(x = "Tasa de crecimiento del exceso de dinero", y = "")
)

data_list = list(P_M.Yextremo,P_M.Y50,P_M.Y20,P_M.Y10)
titulo = list("Todos los países",
              "Crecimiento del dinero en exceso \nmenor al 50 %",
              "Crecimiento del dinero en exceso \nmenor al 20 %",
              "Crecimiento del dinero en exceso \nmenor al 10 %"
              )
plot_list <- list() # Crear una lista vacía para almacenar los gráficos

for(i in 1:4){#Iterar sobre los 4 conjuntos de datos y crear gráficos
  
  reg <- glm(P~M.Y, data=na.omit(data_list[[i]]))#Ajustar el modelo de regresión
  correlation <- cor(data_list[[i]]$M.Y, 
                     data_list[[i]]$P, 
                     use = "pairwise.complete.obs") #Coef corelación
  
  plot <- ggplot(na.omit(data_list[[i]]), aes(x = M.Y, y = P)) +
    geom_point() +
    geom_smooth(method = "glm", se = FALSE, color = "black", linewidth = 0.4) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    annotate("text", 
             x = min(na.omit(data_list[[i]]$M.Y)), 
             y = max(na.omit(data_list[[i]]$P)), 
             label = paste0(titulo[i],"\n","\n",
                            "Intercepto = ", round(coef(reg)[1], 2), "\n",
                            "Pendiente = ", round(coef(reg)[2], 2), "\n",
                            "Coef. corr. = ", round(correlation,2)),
             vjust = 1, hjust = 0) +
    theme_bw() +
    theme(plot.margin = unit(c(1, 1, 1, 3), "lines"),
          plot.background = element_rect(fill = "white"),
          plot.title = element_text(hjust = 0.5),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.position = "none",
          plot.caption = element_text(size = 10, hjust = 0)) +
    labs(x = axis_labels[[i]]$x, y = axis_labels[[i]]$y)
  plot_list[[i]] <- plot
}
# Combinar los cuatro gráficos en una cuadrícula de 2x2
ggarrange(plot_list[[1]], plot_list[[2]], 
          plot_list[[3]], plot_list[[4]], 
          ncol = 2, 
          nrow = 2)


################################ REMOVE ########################################
rm(MY)
rm(axis_labels)
rm(data_list)
rm(titulo)
rm(plot)
rm(plot_list)
rm(reg)
