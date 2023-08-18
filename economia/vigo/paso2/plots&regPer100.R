
library(ggplot2)
library(ggpubr)

plotsI <- function(data_list, titulo) {
  # Crear una lista de etiquetas de eje
  axis_labels <- list(
    list(x = "", y = "Tasa de inflación"),
    list(x = "", y = ""),
    list(x = "Tasa de crecimiento del excedente de dinero", y = "Tasa de inflación"),
    list(x = "Tasa de crecimiento del excedente de dinero", y = "")
  )
  
  plot_list <- list() # Crear una lista vacía para almacenar los gráficos
  
  for(i in 1:4) { # Iterar sobre los 4 conjuntos de datos y crear gráficos
    
    # Modelo de regresión
    reg <- glm(P ~ M.Y, data = na.omit(data_list[[i]]))
    
    # Coeficiente de correlación
    correlation <- cor(data_list[[i]]$M.Y, 
                       data_list[[i]]$P, 
                       use = "pairwise.complete.obs")
    
    # Valores mínimos y máximos de los ejes x e y
    min_x <- min(data_list[[i]]$M.Y, na.rm = TRUE)
    max_x <- max(data_list[[i]]$M.Y, na.rm = TRUE)
    min_y <- min(data_list[[i]]$P, na.rm = TRUE)
    max_y <- max(data_list[[i]]$P, na.rm = TRUE)
    
    # Lista de números para etiquetar los ejes
    number <- c(50, 100, 25, 50, 10, 20, 5, 10)
    # Crear el gráfico utilizando ggplot2
    plot <- ggplot(na.omit(data_list[[i]]), aes(x = M.Y, y = P)) +
      geom_point() +
      geom_smooth(method = "glm", se = FALSE, color = "black", linewidth = 0.4) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      annotate("text", 
               x = min(na.omit(data_list[[i]]$M.Y)), 
               y = max(na.omit(data_list[[i]]$P)), 
               label = paste0(titulo[i], "\n", "\n",
                              "Intercepto = ", round(coef(reg)[1], 2), "\n",
                              "Pendiente = ", round(coef(reg)[2], 2), "\n",
                              "Coef. corr. = ", round(correlation, 2)),
               vjust = 1, hjust = 0, size = 3) +
      theme_bw() +
      theme(plot.margin = unit(c(1, 1, 1, 3), "lines"),
            plot.background = element_rect(fill = "white"),
            axis.text = element_text(size = 8),
            legend.position = "none",
            plot.caption = element_text(size = 10, hjust = 0)) +
      labs(x = axis_labels[[i]]$x, y = axis_labels[[i]]$y) +
      scale_x_continuous(limits = c(min_x, max_x),
                         breaks = c(min_x, (max_x + min_x) / 2, max_x),
                         labels = c(0, number[(i-1)*2+1], number[(i-1)*2+2])) +
      scale_y_continuous(limits = c(min_y, max_y),
                         breaks = c(min_y, (max_y + min_y) / 2, max_y),
                         labels = c(0, number[(i-1)*2+1], number[(i-1)*2+2])) 
    
    plot_list[[i]] <- plot # Agregar el gráfico a la lista de gráficos
  }
  
  # Utilizar ggpubr para mostrar los gráficos en una cuadrícula
  ggarrange(plot_list[[1]], plot_list[[2]], 
            plot_list[[3]], plot_list[[4]], 
            ncol = 2, 
            nrow = 2)
}


plotsI(data_list,titulo)

