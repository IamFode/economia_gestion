################################ Capítulo 2 ####################################
######################## El modelo de gresión lineal ###########################

################ Estimadores de mínimos cuadrados ordinarios ###################

#media
media <- function(x){
  sum <- 0
  for (i in 1:length(x)){
   sum <- sum + y[i] 
  }
  return(sum)
}


# pendiente
beta1 <- function(y,x){
  sumnam <- 0
  sumden <- 0
  n = length(x)
  n1 = length(y)
  mediax = sum(x) / n 
  mediay = sum(y) / n1 
  for (i in 1:n){
    sumnam <- sumnam + ( (x[i]-mediax) * (y[i]-mediay) )
    sumden <- sumden + ( (x[i]-mediax)^2 )
  }
  beta1 <- sumnam / sumden
  return(beta1)
}

# intercepto
beta0 <- function(y,x){
  mediax <- sum(x) / length(x) 
  mediay <- sum(y) / length(y) 
  beta0 <- mediay - beta1(y,x)*mediax
  return(beta0)
}

meanyhat <- function(y,x){
  res <- 0
  for (i in 1:length(x)){
    res <- res + (beta0(y,x) + beta1(y,x)*x[i])
  }
  return(res/length())
}

## Propiedades de MCO
# residual
residual <- function(y,x){
  res <- 0
  for (i in 1:length(x)){
    res <- res + ( y[i] - beta0(y,x) - beta1(y,x)*x[i] )
  } 
  return(res)
}

# covarianza muestral entre los regresores de MCO y los reciduos es cero

covmuestral <- function(y,x){
  res <- 0
  for (i in 1:length(x)){
    res <- res + (x[i]*( y[i] - beta0(y,x) - beta1(y,x)*x[i] ))
  } 
  return(res)
}

# SST suma de cuadrados totales
SST <- function(y){
  sum <- 0
  for (i in 1:length(y)){
   sum <- sum + (y[i]-mean(y))^2 
  }
  return(sum)
}

# SSE suma explicada de cuadrados 
SSE <- function(y){
  sum <- 0
  for (i in 1:length(y)){
   sum <- sum + (beta0(y,x)+beta1(y,x)*x[i]-mean(y))^2 
  }
  return(sum)
}

# SSR suma de cuadrados residuales

SSR <- function(y,x){
  res <- 0
  for (i in 1:length(x)){
    res <- res + (y[i]-beta0(y,x)-beta1(y,x)*x[i])^2
  } 
  return(res)
}

# Bondad de ajuste R^2
r2 <- function(y,x){
  return( 1-SSR(y,x)/SST(y) )
} 

