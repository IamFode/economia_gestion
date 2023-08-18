#LIBRERIAS
{library(ggplot2)
library(tidyr)
library(ggpubr)
  }

#FUNCIONES

# Definir una función que aplica la fórmula ln(P_{t+1}/(P_t)) a un data frame
lDif = function(df) {
  result <- matrix(NA, nrow = nrow(df), ncol = ncol(df))
  colnames(result) = colnames(df)
  for (i in 2:ncol(df)) {
    result[,i] = log(df[,i] / df[,i-1])
  }
  result <- as.data.frame(result)
  return(result)
}


# Tasa de crecimiento geométrica
tasaCrecGeom <- function(df,T){
  data = matrix(nrow = nrow(df),ncol = ncol(df)-T)
  for(row in 1:nrow(df)){
    for(col in (T+1):ncol(df)) {
      data[row,(col-T)] <- (1/T)*log(df[row,col]/df[row,col-T])
    }
  }
  data <- data.frame(data)
  colnames(data) <- colnames(df)[(T+1):ncol(df)]
  return(data)
}


# De columnas a filas y unión de df's.
ColToRow <- function(dfname, df1, df2) {
  Pcountry <- cbind(dfname[1], df1)
  Pcountry$indice <- 1:nrow(Pcountry)
  newdf <- gather(Pcountry, key = "año", value = "P", -"Country Name", -indice)
  Pvertical <- arrange(newdf, indice)
  
  MYcountry <- cbind(dfname[1], df2)
  MYcountry$indice <- 1:nrow(MYcountry)
  newdf <- gather(MYcountry, key = "año", value="M.Y", -"Country Name", -indice)
  M.Yvertical <- arrange(newdf, indice)
  
  P_M.Y <- merge(M.Yvertical, Pvertical, by = c("Country Name","indice","año"))
  P_M.Y$indice <- NULL
  
  return(P_M.Y)
}



# Tasa de crecimiento geométrica para el primer y último valor con T distintos.
lastFirst <- function(df) {
  resultados <- numeric(nrow(df))
  for (i in 1:nrow(df)) {
    indices <- which(!is.na(df[i, ]))
    if (length(indices) == 0) { #Verificar si el vector de índices está vacío
      resultados[i] <- NA
    } else {
      indice_primer <- min(indices)
      indice_ultimo <- max(indices)
      ultimo_valor <- df[i, indice_ultimo]
      primer_valor <- df[i, indice_primer]
      resultado <- (1/(indice_ultimo-indice_primer))*log(ultimo_valor/primer_valor)
      if (is.infinite(resultado)) {#Si el resultado es Inf o -Inf devolver N
        resultados[i] <- NA
      } else {
        resultados[i] <- resultado
      }
    }
  }
  return(resultados)
}
    

plotsII <- function(data_list, titulo) {
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
            axis.text = element_text(size = 8),
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
          axis.text = element_text(size = 8),
          legend.position = "none",
          plot.caption = element_text(size = 10, hjust = 0))+
    labs( x = "Tasa de crecimiento del excedente de dinero", y = "Tasa de Inflación")
}




# Promedios para P en función de T
promediosT = function(df, T) {
  n = ncol(df)
  promedios = matrix(NA, nrow = nrow(df), ncol = n-T+1)
  for (i in 1:nrow(df)) {
    for (s in T:n) {
      group = df[i, (s-T+1):s]
      if (all(is.na(group))) {
        promedios[i, s-T+1] = NA
      } else {
        promedios[i, s-T+1] = sum(group, na.rm = TRUE) / T
      }
    }
  }
  promedios = as.data.frame(promedios)
  colnames(promedios) = colnames(df)[T:ncol(df)]
  return(promedios)
}



