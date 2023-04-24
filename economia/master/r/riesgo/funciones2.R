

################ RENTABILIDAD DE UN ACTIVO #####################
rentabActivo <- function(data) {
  x = as.matrix(data,lag=1)
  log_x <- log(x)
  res = diff(log_x,lag=1)
  return(res)
}
################################################################

################ RENTABILIDAD DE UN ACTIVO #####################
rentabActivoEj2 <- function(data,n) {
  x = as.matrix(data,lag=1)
  log_x <- log(x)
  res = diff(log_x,lag=n)
}
################################################################

################ RENTABILIDAD DE UNA CARTERA ###################
rentabCartera <- function(vec,peso) {
  result <- lapply(seq_along(vec), function(i) rentabActivo(vec[[i]]) * peso[i])
  result <- do.call(cbind, result)
  rowSums(result)
}

rentabCarteraEj2 <- function(vec,peso,nn) {
  result <- lapply(seq_along(vec), function(i) rentabActivoEj2(vec[[i]],nn) * peso[i])
  result <- do.call(cbind, result)
  rowSums(result)
}
################################################################


################# SIMULACIÓN HISTÓRICA #########################
simHistoricaEj2 = function(...,quantil,n,peso){
  df = data.frame(...)
  Var <- list()
  for (col in names(df)) {
    x <- df[[col]]
    res <- diff(x, n)
    last <- tail(x, n=1)
    var <- c()
    for (i in res) {
      var <- c(var, last+i)
    }
    Var[[col]] <- var
  }
  rent=rentabCartera(Var,peso)
  VaR = unname(quantile(rent,probs=quantil,rm.na=FALSE))
  ES = sum(rent[rent<=VaR])/length(rent[rent<=VaR])
  result = cbind(VaR,ES)
  return(result)
}

simHistoricaEj2VaR = function(...,quantil,n,peso){
  df = data.frame(...)
  Var <- list()
  for (col in names(df)) {
    x <- df[[col]]
    res <- diff(x, n)
    last <- tail(x, n=1)
    var <- c()
    for (i in res) {
      var <- c(var, last+i)
    }
    Var[[col]] <- var
  }
  rent=rentabCartera(Var,peso)
  VaR = unname(quantile(rent,probs=quantil,rm.na=FALSE))
  ES = sum(rent[rent<=VaR])/length(rent[rent<=VaR])
  return(VaR)
}
################################################################


################## Simulación por Monte Carlo ##################
simMonteCarloEj2 = function(x,dias,prob,peso){
  rentabilidad = rentabCartera(x,peso)
  deanual = sqrt(dias)*sd(rentabilidad)
  mediaanual = dias*mean(rentabilidad)
  delta = 1/dias
  last = tail(rentabilidad,n=1)
  set.seed(2000)
  aleatorion = rnorm(length(x),0,1)
  precio = last+last*(mediaanual*delta+deanual*sqrt(delta)*aleatorion)
  rent = log(precio/last)
  VaR = quantile(rent,prob)
  ES = sum(rent[rent<=VaR])/length(rent[rent<=VaR])
  result = cbind(VaR,ES)
  return(result)
}

simMonteCarloEj2VaR = function(x,dias,prob,peso){
  rentabilidad = rentabCartera(x,peso)
  deanual = sqrt(dias)*sd(rentabilidad)
  mediaanual = dias*mean(rentabilidad)
  delta = 1/dias
  last = tail(rentabilidad,n=1)
  set.seed(2000)
  aleatorion = rnorm(length(x),0,1)
  precio = last+last*(mediaanual*delta+deanual*sqrt(delta)*aleatorion)
  rent = log(precio/last)
  VaR = quantile(rent,prob)
  ES = sum(rent[rent<=VaR])/length(rent[rent<=VaR])
  return(VaR)
}

################################################################

################### ESTIMACIÓN PARAMÉTRICA #####################
estimParametricaEj2 = function(...,quantil,tiempo,gl,peso){
  data = data.frame(...)
  rentabilidad =  rentabCartera(data,peso)
  rentabilidad = as.matrix(rentabilidad)
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

estimParametricaEj2VaR = function(...,quantil,tiempo,gl,peso){
  data = data.frame(...)
  rentabilidad =  rentabCartera(data,peso)
  rentabilidad = as.matrix(rentabilidad)
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
  return(VaRnormal)
}


################################################################


####################### MODELO GARCH ###########################
garchModelEj2 = function(activo,quantil,peso,n){
  rentabilidad = rentabCarteraEj2(activo,peso,n)
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

garchModelEj2VaR = function(activo,quantil,peso,n){
  rentabilidad = rentabCarteraEj2(activo,peso,n)
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
backtestingEj2 = function(func,activo,p,peso){
  rentabilidad = as.matrix(rentabCartera(activo,peso))
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

  