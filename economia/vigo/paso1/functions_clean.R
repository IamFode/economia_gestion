################################################################################
###################### FUNCIONES DE LIMPIEZA ###################################
################################################################################

'"
Esta función filtra los datos por codigos financieros para luego borrar los
países que se repiten y que no tienen datos, para luego itera sobre todos los 
grupos de codigos financieros y los une en un solo dataframe.
"'
clean = function(dataset, condition_list) {
  df = data.frame()
  filtrar_datos = function(dataset, condition) {
    x = dataset[dataset$`Indicator Code` == condition,]
    i = length(unique(x$`Country Name`))
    xx = x[apply(x[, 6:ncol(x)], 1, 
                 function(x) any(!is.na(x))) & !duplicated(x$`Country Code`),]
    print(xx)
    f = length(xx$`Country Name`)
    if (i == f) {
      df <<- rbind(df, xx)
    }
  }
  for (condition in condition_list) {
    filtrar_datos(dataset, condition)
  }
  return(df)
}

clean2 = function(serie, condition) {
  x <- serie[!apply(is.na(serie[, 6:ncol(serie)]), 1, all), ]
  x <- x[x$`Indicator Code` %in% condition, ]
  return(x)
}


'"
Esta función limpia todos las filas repetidas priorizando los valores cualitativos
de la columna "Indicator Code"
"'
eliminar_filas_duplicadas <- function(df, categorias) {
  paises <- unique(df$`Country Name`)
  maxima_fila <- c()
  for (pais in paises) {
    M1prueba <- which(df$`Country Name` == pais & duplicated(df$`Country Name`))
    for (i in 1:length(M1prueba)) {
      filas_a_comparar <- c(M1prueba[i] - 1, M1prueba[i])
      paises_a_comparar <- unique(df$`Country Name`[filas_a_comparar])
      indices <- match(df[filas_a_comparar, "Indicator Code"], categorias)
      indice_max <- which.min(indices)
      fila_max <- filas_a_comparar[indice_max]
      maxima_fila <- c(maxima_fila, fila_max)
    }
  }
  df <- df[-maxima_fila, ]
  return(df)
}


'"
Esta función limpia todos las filas repetidas, solo en el caso de que no haya
datos para FASMB, o la serie sea idéntica a FASMB pero más larga.
"'
eliminar_fila_BM.IFS <- function(df, indicator) {
  M1prueba <- which(duplicated(df$`Country Name`))
  filas_eliminar <- c()
  for (i in M1prueba) {
    row1_index <- i - 1
    row2_index <- i
    has_indicator <- grepl(indicator, df$`Indicator Code`[c(row1_index, row2_index)])
    has_non_na <- sum(!is.na(df[row1_index, 6:ncol(df)])) > 0 | sum(!is.na(df[row2_index, 6:ncol(df)])) > 0
    if (has_indicator[1] | has_indicator[2] && has_non_na) {
      row1 <- df[row1_index, ]
      row2 <- df[row2_index, ]
      data1 <- row1$`Indicator Code`
      data2 <- row2$`Indicator Code`
      if (all(data1 == data2) | data1 != indicator | data2 != indicator) {
        if (sum(!is.na(row1)) <= sum(!is.na(row2))) {
          filas_eliminar <- c(filas_eliminar, row1_index)
        } else {
          filas_eliminar <- c(filas_eliminar, row2_index)
        }
      }
    }
  }
  df <- df[-filas_eliminar, ]
  return(df)
}