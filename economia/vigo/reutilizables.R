
# PLOTS CON LM() PARA EL GRÄFICO 3.
for (i in 1:4) {
  # Ajustar el modelo de regresión para el conjunto de datos actual
  reg <- lm(P ~ M.Y, data = na.omit(data_list[[i]]))
  # Calcular el coeficiente de correlación para el conjunto de datos actual
  correlation <- cor(data_list[[i]]$M.Y, data_list[[i]]$P, use = "pairwise.complete.obs")
  # Crear el gráfico para el conjunto de datos actual
  plot <- ggplot(na.omit(data_list[[i]]), aes(x = M.Y, y = P)) +
    geom_point() +
    geom_smooth(method = "glm", se = FALSE, color = "black", linewidth = 0.4) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    annotate("text", x = min(na.omit(data_list[[i]]$M.Y)), y = max(na.omit(data_list[[i]]$P)), 
             label = paste0("Intercepto = ", round(coef(reg)[1], 2), "\n",
                            "Pendiente = ", round(coef(reg)[2], 2), "\n",
                            "Rcuadrado = ", round(summary(reg)$r.squared, 2), "\n",
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
    labs(x = axis_labels[[i]]$x, y = axis_labels[[i]]$y) # Agregar las etiquetas de eje específicas al gráfico actual
  
  # Agregar el gráfico a la lista de gráficos
  plot_list[[i]] <- plot
}