
#FUNCIONES

# Definir una función que aplica la fórmula ln(P_{t+1}/(P_t)) a un data frame
lDif = function(df) {
  result <- data.frame(matrix(NA, nrow = nrow(df), ncol = ncol(df)))
  colnames(result) = colnames(df)
  for (i in 2:ncol(df)) {
    result[,i] = log(df[,i] / df[,i-1])
  }
  return(result)
}

# Tasa de crecimiento geométrica
tasaCrecGeom <- function(df, T) {
  n <- ncol(df)
  promedios <- data.frame(matrix(NA, nrow = nrow(df), ncol = n-T))
  for (i in 1:nrow(df)) {
    for (s in T:n) {
      group <- df[i, (s-T):s]
      if (all(is.na(group))) {
        promedios[i, s-T] <- NA
      } else {
        promedios[i, s-T] <- (1/T) * log(df[i, s] / df[i, s-T])
      }
    }
  }
  colnames(promedios) <- colnames(df)[T:ncol(df)]
  return(promedios)
}


# Promedios para P en función de T
promediosT = function(df, T) {
  n = ncol(df)
  promedios = data.frame(matrix(NA, nrow = nrow(df), ncol = n-T+1))
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
  colnames(promedios) = colnames(df)[T:ncol(df)]
  return(promedios)
}


