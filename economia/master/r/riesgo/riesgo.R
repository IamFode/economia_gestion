
riesgo = data.frame(
  cartera = c("bonoA", "bonoB"),  
  vencimiento = c(10, 4),       
  vn = c(1000, 2000),       
  cupon = c(0.06, 0.04)
)

# Crear el data frame
df = data.frame(
  num = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  FD = c(0.99765855, 0.995342123, 0.993152749, 0.991075578, 0.989241414, 
         0.987374908, 0.976018682, 0.962154182, 0.947061336, 0.946061336)
)

# Definir una función para calcular el porcentaje de cambio
porcCambio = function(x) {
  (1/x[2] - 1) * (1/x[1])
}

# Aplicar la función a cada fila del data frame
df$porcCambio <- apply(df[, c("num", "FD")], 1, porcentaje_cambio)

# Forward (h,h+1,h+1) (tipo de interes)
forward <- function(x) {
  result <- numeric(length(x))
  result[-1] <- 1/(x[-1]/x[-length(x)]) - 1
  result[1] <- NA
  return(result)
}

df["forward"]=forward(df$FD)

#Curva de factor de descuento
result=(1/(1+df$forward[-1]))*df$FD
df["factDescuento"]=c(NA,result[1:(length(result)-1)])

# Cuales son las rentas que genera el bono.
# genera bono A
riesgoBonoA=c()
for (i in 1:riesgo$vencimiento[1]) {
  if (i == riesgo$vencimiento[1]) {
    riesgoBonoA=c(riesgoBonoA, riesgo$cupon[1]*riesgo$vn[1] + riesgo$vn[1])
  } else {
    riesgoBonoA=c(riesgoBonoA, riesgo$cupon[1]*riesgo$vn[1])
  }
}

riesgoBonoB=c()
for (i in 1:riesgo$vencimiento[2]) {
  if (i == riesgo$vencimiento[2]) {
    riesgoBonoB=c(riesgoBonoB, riesgo$cupon[2]*riesgo$vn[2] + riesgo$vn[2])
  } else {
    riesgoBonoB=c(riesgoBonoB, riesgo$cupon[2]*riesgo$vn[2])
  }
}

#CUAL ES EL VALOR DEL BONO A? 
valorBonoA = riesgoBonoA%*%df$FD

#CUAL ES EL VALOR DEL BONO A? 
valorBonoB = riesgoBonoB %*% df$FD[1:4]

# VALOR de la CARTERA
CARTERA = valorBonoA+valorBonoB

# SUMA DE BONOS SEGUN LOS AÑOS
sumaBono=riesgoBonoA[1:4]+riesgoBonoB



############################3
#TIPOS FORWARD
volat = 0.2
df$forward
