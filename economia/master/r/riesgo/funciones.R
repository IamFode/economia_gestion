
############### FORMULAS DE LIMPIEZA DE DATOS ##################
limpiar <- function(df) {
  df[,1] <- as.Date(df[,1], format = "%d.%m.%Y")
  df[,2:ncol(df)] <- lapply(df[,2:ncol(df)], 
                            function(x) gsub("\\.", "", x))
  df[,2:ncol(df)] <- lapply(df[,2:ncol(df)], 
                            function(x) gsub(",", ".", x))
  df[,2:ncol(df)] <- lapply(df[,2:ncol(df)], 
                            as.numeric)
  return(df)
}
################################################################


################ RENTABILIDAD DE UN ACTIVO #####################
rentabActivo <- function(data,horizonte) {
  x = as.matrix(data)
  log_x <- log(x)
  res = diff(log_x,lag=horizonte)
  return(res)
}
################################################################


################# SIMULACIÓN HISTÓRICA #########################
simHistorica = function(x,quantil,horizonte){
  res = diff(x, lag=horizonte)
  last = tail(x,n=1)
  Var = c()
  for(i in res){
    Var = c(Var,last+i)
  }
  rent = c()
  for(i in Var){
    rent = c(rent,log(i/last))
  }
  VaR = unname(quantile(rent,probs=quantil,rm.na=FALSE))
  ES = sum(rent[rent<=VaR])/length(rent[rent<=VaR])
  result = cbind(VaR,ES)
  return(result)
}
################################################################


################## Simulación por Monte Carlo ##################
simMonteCarlo <- function(horizonte,quantil,dias,...) {
  set.seed(2000)
  rentab <- as.matrix(data.frame(...))
  rentabilidades = rentabActivo(rentab,horizonte=horizonte)
  matriz_cor <- cor(rentabilidades)
  cholesk <- chol(matriz_cor)
  aleatorio <- matrix(rnorm(nrow(rentab) * ncol(rentab)), nrow = nrow(rentab))
  colnames(aleatorio) = colnames(rentab)
  epsilons = aleatorio %*% cholesk
  last = tail(rentab,n=1)
  mediasanuales = dias*colMeans(rentabilidades)
  delta = 1/dias
  deAnuales = sqrt(dias)*apply(rentabilidades,2,sd)
  uno = sqrt(delta)*epsilons
  dos = t(apply(uno, 1, function(x) x * deAnuales))
  tres = mediasanuales*delta
  cuatro = t(apply(dos, 1, function(x) x + tres))
  cinco = t(apply(cuatro, 1, function(x) x * last))
  seis = t(apply(cinco, 1, function(x) x + last))
  correlacion = cor(seis)
  colnames(correlacion) = colnames(rentab)
  rownames(correlacion) = colnames(rentab)
  colnames(seis) = colnames(rentab)
  rentabi = rentabActivo(seis,horizonte = horizonte)
  #VaR
  var = apply(rentabi, 2, quantile, probs = quantil, type = 6, na.rm = TRUE)
  #ES
  ES = function(x, prob) {
      VarEmpirico = apply(x,
                          2, 
                          quantile, 
                          probs = quantil, 
                          type = 6, 
                          na.rm = TRUE)
      x_trunc <- lapply(seq_len(ncol(x)), function(i) {
      col <- x[, i]
      return(col[col <= VarEmpirico[i]])
    })
    result <- sapply(x_trunc, mean, na.rm = TRUE)
    names(result) <- colnames(x)
    return(result)
  }
  es = ES(rentabi,quantil)
  # Imprimir las variables que tienen menos de 5 filas
  cat("\nMEDIAS:\n")
  print(mediasanuales)
  cat("\nDESVIACIONES ESTÁNDAR:\n")
  print(deAnuales)
  cat("\n")
  cat("DELTA:",delta)
  cat("\n")
  # Imprimir las primeras 5 filas de las variables que tienen más de 5 filas
  cat("\nDESCOMPOSICIÓN DE CHOLESKI (3 primeros):\n")
  print(head(cholesk,3))
  cat("\nEPSILON (3 primeros):\n")
  print(head(epsilons,3))
  cat("\nCORRELACIONES DE LAS RENTABILIDADES:\n")
  print(correlacion)
  cat("\nACTIVOS ALEATORIOS (3 primeros):\n")
  print(head(seis,3))
  cat("\nRENTABILIDADES (3 primeros):\n")
  print(head(rentabi,3))
  cat("\nVaR:\n")
  print(var)
  cat("\nES:\n")
  print(es)
}

################################################################



################### ESTIMACIÓN PARAMÉTRICA #####################
estimParamétrica = function(data,quantil,dias,horizonte,gl){
  rentabilidad =  rentabActivo(data,horizonte = horizonte)
  medias = colMeans(rentabilidad)*dias
  delta = 1/dias
  de = apply(rentabilidad,2,sd)*sqrt(dias)
  quant = qnorm(quantil)
  VaRnormal = medias*delta+de*sqrt(delta)*quant
  ESnormal = medias*delta-(de*sqrt(delta)/quantil)*dnorm(quant)
  studentT = qt(quantil,gl)
  VaRT = medias*delta+de*sqrt(delta)*studentT
  EST = medias*(1/dias)-(de*sqrt(delta)/quantil)*sqrt(gl/(gl-2))*dt(quant,gl)*((gl+studentT^2)/(gl-1))
  print(EST)
  
}
estimParamétrica(cartera,0.05,252,1,10)
################################################################

