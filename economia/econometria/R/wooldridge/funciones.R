################################ Capítulo 2 ####################################
######################## El modelo de gresión lineal ###########################

################ Estimadores de mínimos cuadrados ordinarios ###################
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

# residual
residual <- function(y,x){
  n <- length(x) 
  res <- 0
  for (i in 1:n){
    res <- res + ( y[i] - beta0(y,x) - beta1(y,x)*x[i] )
    print(res)
  } 
}

