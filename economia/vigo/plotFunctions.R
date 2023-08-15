#PLOTS

plotsIyII <- function(data_list, titulo) {
  axis_labels <- list(# Crear una lista de etiquetas de eje
    list(x = "", y = "Tasa de inflación"),
    list(x = "", y = ""),
    list(x = "Tasa de crecimiento del excedente de dinero", y = "Tasa de inflación"),
    list(x = "Tasa de crecimiento del excedente de dinero", y = "")
  )
  
  plot_list <- list() # Crear una lista vacía para almacenar los gráficos
  
  for(i in 1:4){#Iterar sobre los 4 conjuntos de datos y crear gráficos
    
    reg <- glm(P~M.Y, data=na.omit(data_list[[i]]))#Modelo de regresión
    correlation <- cor(data_list[[i]]$M.Y, 
                       data_list[[i]]$P, 
                       use = "pairwise.complete.obs") #Coef. Corr.
    
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
               vjust = 1, hjust = 0, size=3) +
      theme_bw() +
      theme(plot.margin = unit(c(1, 1, 1, 3), "lines"),
            plot.background = element_rect(fill = "white"),
            axis.text = element_text(size = 1),
            legend.position = "none",
            plot.caption = element_text(size = 10, hjust = 0)) +
      labs(x = axis_labels[[i]]$x, y = axis_labels[[i]]$y)
    plot_list[[i]] <- plot
  }
  ggarrange(plot_list[[1]], plot_list[[2]], 
            plot_list[[3]], plot_list[[4]], 
            ncol = 2, 
            nrow = 2)
}


plotIII <- function(df) {
  reg <- glm(P~M.Y, data=na.omit(df))#Modelo de regresión
  correlation <- cor(df$M.Y, 
                     df$P, 
                     use = "pairwise.complete.obs") #Coef. Corr.
  
  ggplot(na.omit(df), aes(x = M.Y, y = P)) +
    geom_point() +
    geom_smooth(method = "glm", se = FALSE, color = "black", linewidth = 0.4) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    annotate("text", 
             x = min(na.omit(df$M.Y)), 
             y = max(na.omit(df$P)), 
             label = paste0("Primer y último año con T distintos","\n","\n",
                            "Intercepto = ", round(coef(reg)[1], 2), "\n",
                            "Pendiente = ", round(coef(reg)[2], 2), "\n",
                            "Coef. corr. = ", round(correlation,2)),
             vjust = 1, hjust = 0, size=3) +
    theme_bw() +
    theme(plot.margin = unit(c(1, 1, 1, 3), "lines"),
          plot.background = element_rect(fill = "white"),
          axis.text = element_text(size = 1),
          legend.position = "none",
          plot.caption = element_text(size = 10, hjust = 0))+
    labs( x = "Tasa de crecimiento del excedente de dinero", y = "Tasa de Inflación")
}
