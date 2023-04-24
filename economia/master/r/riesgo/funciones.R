
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
rentabActivo <- function(data) {
  x = as.matrix(data,lag=1)
  log_x <- log(x)
  res = diff(log_x,lag=1)
  return(res)
}
################################################################


################ RENTABILIDAD DE UNA CARTERA ###################
rentabCartera <- function(peso, ...) {
  vec <- list(...)
  result <- lapply(seq_along(vec), function(i) rentabActivo(vec[[i]]) * peso[i])
  result <- do.call(cbind, result)
  rowSums(result)
}
################################################################


################# SIMULACIÓN HISTÓRICA #########################
simHistorica = function(x,quantil,n){
  res = diff(x,n)
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

simHistoricaVaR = function(x,quantil,n){
  res = diff(x,n)
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
  return(VaR)
}
################################################################


################## Simulación por Monte Carlo ##################
simMonteCarlo <- function(quantil,tiempo,...) {
  set.seed(2000)
  rentab <- as.matrix(data.frame(...))
  rentabilidades = rentabActivo(rentab)
  matriz_cor <- cor(rentabilidades)
  cholesk <- chol(matriz_cor)
  aleatorio <- matrix(rnorm(nrow(rentab) * ncol(rentab)), nrow = nrow(rentab))
  colnames(aleatorio) = colnames(rentab)
  epsilons = aleatorio %*% cholesk
  last = tail(rentab,n=1)
  delta=1/tiempo
  medias = tiempo*colMeans(rentabilidades)
  desvEstandar = sqrt(tiempo)*apply(rentabilidades,2,sd)
  uno = sqrt(delta)*epsilons
  dos = t(apply(uno, 1, function(x) x * desvEstandar))
  tres = medias*delta
  cuatro = t(apply(dos, 1, function(x) x + tres))
  cinco = t(apply(cuatro, 1, function(x) x * last))
  seis = t(apply(cinco, 1, function(x) x + last))
  correlacion = cor(seis)
  colnames(correlacion) = colnames(rentab)
  rownames(correlacion) = colnames(rentab)
  colnames(seis) = colnames(rentab)
  rentabi = rentabActivo(seis)
  #VaR
  VaR = apply(rentabi, 2, quantile, probs = quantil, type = 6, na.rm = TRUE)
  #ES
  es = function(x, prob) {
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
  ES = es(rentabi,quantil)
  # Imprimir las variables que tienen menos de 5 filas
  cat("Media *",tiempo,"\n")
  cat("Desviación Estándar *",tiempo,"\n")
  cat("delta 1 /",tiempo,"\n")
  result1 = cbind(medias,desvEstandar,delta)
  print(result1)
  # Imprimir las primeras 5 filas de las variables que tienen más de 5 filas
  cat("\nDESCOMPOSICIÓN DE CHOLESKI (3 primeros):\n")
  print(head(cholesk,3))
  cat("\nCORRELACIONES DE LAS RENTABILIDADES:\n")
  print(correlacion)
  result2 = cbind(VaR,ES)
  return(result2)
}

simMonteCarloVaR <- function(quantil,tiempo,activo,...) {
  set.seed(2000)
  rentab <- as.matrix(data.frame(...))
  rentabilidades = rentabActivo(rentab)
  matriz_cor <- cor(rentabilidades)
  cholesk <- chol(matriz_cor)
  aleatorio <- matrix(rnorm(nrow(rentab) * ncol(rentab)), nrow = nrow(rentab))
  colnames(aleatorio) = colnames(rentab)
  epsilons = aleatorio %*% cholesk
  last = tail(rentab,n=1)
  delta=1/tiempo
  mediasanuales = tiempo*colMeans(rentabilidades)
  deAnuales = sqrt(tiempo)*apply(rentabilidades,2,sd)
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
  rentabi = rentabActivo(seis)
  #VaR
  VaR = apply(rentabi, 2, quantile, probs = quantil, type = 6, na.rm = TRUE)
  #ES
  es = function(x, prob) {
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
  ES = es(rentabi,quantil)
  return(VaR[activo])
}


simMonteCarloUnaVariable = function(x,dias,prob){
  rentabilidad = rentabActivo(x)
  deanual = sqrt(dias)*sd(rentabilidad)
  mediaanual = dias*mean(rentabilidad)
  delta = 1/dias
  last = tail(x,n=1)
  set.seed(2000)
  aleatorion = rnorm(length(x),0,1)
  precio = last+last*(mediaanual*delta+deanual*sqrt(delta)*aleatorion)
  rent = log(precio/last)
  Var = quantile(rent,prob)
  ES = sum(rent[rent<=Var])/length(rent[rent<=Var])
  result = cbind(Var,ES)
  return(result)
}
################################################################

################### ESTIMACIÓN PARAMÉTRICA #####################
estimParametrica = function(...,quantil,tiempo,gl){
  data = data.frame(...)
  rentabilidad =  rentabActivo(data)
  medias = colMeans(rentabilidad)*tiempo
  de = apply(rentabilidad,2,sd)*sqrt(tiempo)
  delta = 1/tiempo
  quant = qnorm(quantil)
  VaRnormal = medias*delta+de*sqrt(delta)*quant
  ESnormal = medias*delta-(de*sqrt(delta)/quantil)*dnorm(quant)
  studentT = qt(quantil,gl)
  VaRtStudent = medias*delta+de*sqrt(delta)*studentT
  EStStudent = medias*(delta)-(de*sqrt(delta)/quantil)*sqrt(gl/(gl-2))*dt(quant,gl)*((gl+studentT^2)/(gl-1))
  
  S = skewness(rentabilidad)
  K=apply(rentabilidad, 2,kurtosis, type=2)
  H2 = quant^2-1
  H3 = quant^3 - 3*quant
  cuantilCorregido = quant+(S/6)*H2+(K/24)*H3-(S^2/36)*(2*H3+quant)
  VaRCorregido = medias*delta+de*sqrt(delta)*cuantilCorregido
  
  # Imprimir las variables que tienen menos de 5 filas
  cat("Media *",tiempo,"\n")
  cat("Desviación Estándar *",tiempo,"\n")
  cat("delta 1 /",tiempo,"\n")
  result1 = cbind(medias,de,delta)
  print(result1)
  cat("\n")
  result = cbind(VaRnormal,ESnormal)
  print(result)
  cat("\n")
  result2 = cbind(VaRtStudent,EStStudent)
  print(result2)
  cat("\n")
  cat("\nVaR CORREGIDO:\n")
  print(VaRCorregido)
  
}

estimParametricaVaR = function(...,quantil,tiempo,gl){
  library(e1071)
  data = data.frame(...)
  rentabilidad =  rentabActivo(data)
  medias = colMeans(rentabilidad)*tiempo
  de = apply(rentabilidad,2,sd)*sqrt(tiempo)
  delta = 1/tiempo
  quant = qnorm(quantil)
  VaRnormal = medias*delta+de*sqrt(delta)*quant
  ESnormal = medias*delta-(de*sqrt(delta)/quantil)*dnorm(quant)
  studentT = qt(quantil,gl)
  VaRT = medias*delta+de*sqrt(delta)*studentT
  EST = medias*(delta)-(de*sqrt(delta)/quantil)*sqrt(gl/(gl-2))*dt(quant,gl)*((gl+studentT^2)/(gl-1))
  
  S = skewness(rentabilidad)
  K=apply(rentabilidad, 2,kurtosis, type=2)
  H2 = quant^2-1
  H3 = quant^3 - 3*quant
  cuantilCorregido = quant+(S/6)*H2+(K/24)*H3-(S^2/36)*(2*H3+quant)
  VaRCorregido = medias*delta+de*sqrt(delta)*cuantilCorregido
  return(VaRnormal)
}
################################################################

####################### MODELO GARCH ###########################
garchModel = function(activo,quantil,n){
  rentabilidad = diff(activo,n)
  modelo <- ugarchspec(variance.model = list(model = "sGARCH", 
                                             garchOrder = c(1,1)),
                            mean.model = list(armaOrder = c(0,0)),
                            distribution.model = "norm")
  
  modeloGarch = ugarchfit(data=rentabilidad,spec = modelo)
  ajustado = modeloGarch@fit$fitted.values
  varianza = modeloGarch@fit$var
  cuantil = qnorm(quantil)
  VaR = ajustado+sqrt(varianza)*cuantil
  ES = ajustado-(varianza/quantil)*pnorm(cuantil)
  result = cbind(VaR,ES)
  return(result)
}

garchModelVaR = function(activo,quantil,n){
  rentabilidad = diff(activo,n)
  modelo <- ugarchspec(variance.model = list(model = "sGARCH", 
                                             garchOrder = c(1,1)),
                            mean.model = list(armaOrder = c(0,0)),
                            distribution.model = "norm")
  
  modeloGarch = ugarchfit(data=rentabilidad,spec = modelo)
  ajustado = modeloGarch@fit$fitted.values
  varianza = modeloGarch@fit$var
  cuantil = qnorm(quantil)
  VaR = ajustado+sqrt(varianza)*cuantil
  ES = ajustado-(varianza/quantil)*pnorm(cuantil)
  return(VaR[1])
}
################################################################


######################### BACKTESTING ##########################
backtesting = function(func,activo,p){
  rentabilidad = rentabActivo(activo)
  comparar = rentabilidad<=func
  t = length(rentabilidad)
  N = sum(comparar==TRUE)
  pEstimado = N/t
  test = -2*log((1-p)^(t-N)*p^N)+2*log((1-pEstimado)^(t-N)*pEstimado^N)
  chidist = pchisq(test,1,lower.tail = FALSE)
  if(test>chidist){
    cat("p estimado =", pEstimado, "es distinto a p =",p)
  } else{
    cat("p estimado =", pEstimado, "es igual a p =",p)
  }
  return(chidist)
}
################################################################
