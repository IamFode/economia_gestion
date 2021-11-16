################################ Capítulo 2 ####################################
######################## El modelo de gresión lineal ###########################

################ Estimadores de mínimos cuadrados ordinarios ###################
beta1y2 <- function(x,y){
  sumnam <- 0
  sumden <- 0
  mediax = sum(x) / length(x)
  mediay = sum(y) / length(y)
  for (i in range(length(x))){
    sumnam <- (x[i]-mediax) * (y[i]-mediay)
    sumden <- (x[i]-mediax)^2
  }
  beta1 <- sumnam / sumden
  beta0 <- mediay - beta1*mediax
  return(beta1, beta0)
}