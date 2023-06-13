library(hexView)
library(quantmod)
library(fGarch)

data=readEViews("data/Tarea1.wf1",time.stamp = FALSE)
data$DATE = NULL

# Obtener los rendimientos diarios de los stocks
ret_AAPL <- diff(log(data$AAPL))
ret_NLY <- diff(log(data$NLY))
ret_XOM <- diff(log(data$XOM))

# Ajustar los modelos GARCH a los rendimientos de cada stock
model_AAPL <- garchFit(formula = ~garch(1, 1), data = ret_AAPL, cond.dist = "QMLE")
model_NLY <- garchFit(formula = ~garch(1, 1), data = ret_NLY, cond.dist = "QMLE")
model_XOM <- garchFit(formula = ~garch(1, 1), data = ret_XOM, cond.dist = "QMLE")

# Obtener la media condicional y la varianza condicional de cada stock
mu_AAPL <- predict(model_AAPL, n.ahead = 1)$meanForecast
sigma2_AAPL <- (predict(model_AAPL, n.ahead = 1)$standardDeviation)^2
mu_NLY <- predict(model_NLY, n.ahead = 1)$meanForecast
sigma2_NLY <- (predict(model_NLY, n.ahead = 1)$standardDeviation)^2
mu_XOM <- predict(model_XOM, n.ahead = 1)$meanForecast
sigma2_XOM <- (predict(model_XOM, n.ahead = 1)$standardDeviation)^2


# Calcular las ponderaciones óptimas
w1 <- (mu_AAPL / sigma2_AAPL) / ((mu_AAPL^2 / sigma2_AAPL) + (mu_NLY^2 / sigma2_NLY) + (mu_XOM^2 / sigma2_XOM))
w2 <- (mu_NLY / sigma2_NLY) / ((mu_AAPL^2 / sigma2_AAPL) + (mu_NLY^2 / sigma2_NLY) + (mu_XOM^2 / sigma2_XOM))
w3 <- (mu_XOM / sigma2_XOM) / ((mu_AAPL^2 / sigma2_AAPL) + (mu_NLY^2 / sigma2_NLY) + (mu_XOM^2 / sigma2_XOM))

# Calcular las ponderaciones óptimas en función del tiempo
n <- nrow(data)  # Número total de observaciones
weights <- matrix(NA, nrow = n, ncol = 3)
for (i in 1:n) {
  # Obtener la media condicional y la varianza condicional en el momento i
  mu_AAPL_i <- predict(model_AAPL, n.ahead = 1, n.start = i)$meanForecast
  sigma2_AAPL_i <- (predict(model_AAPL, n.ahead = 1, n.start = i)$standardDeviation)^2
  mu_NLY_i <- predict(model_NLY, n.ahead = 1, n.start = i)$meanForecast
  sigma2_NLY_i <- (predict(model_NLY, n.ahead = 1, n.start = i)$standardDeviation)^2
  mu_XOM_i <- predict(model_XOM, n.ahead = 1, n.start = i)$meanForecast
  sigma2_XOM_i <- (predict(model_XOM, n.ahead = 1, n.start = i)$standardDeviation)^2
  
  # Calcular las ponderaciones óptimas en el momento i
  weights[i, 1] <- (mu_AAPL_i / sigma2_AAPL_i) / ((mu_AAPL_i^2 / sigma2_AAPL_i) + (mu_NLY_i^2 / sigma2_NLY_i) + (mu_XOM_i^2 / sigma2_XOM_i))
  weights[i, 2] <- (mu_NLY_i / sigma2_NLY_i) / ((mu_AAPL_i^2 / sigma2_AAPL_i) + (mu_NLY_i^2 / sigma2_NLY_i) + (mu_XOM_i^2 / sigma2_XOM_i))
  weights[i, 3] <- (mu_XOM_i / sigma2_XOM_i) / ((mu_AAPL_i^2 / sigma2_AAPL_i) + (mu_NLY_i^2 / sigma2_NLY_i) + (mu_XOM_i^2 / sigma2_XOM_i))
}


# Graficar la evolución temporal de las ponderaciones óptimas
plot(data$Date, weights[, 1], type = "l", xlab = "Fecha", ylab = "Ponderación", col = "blue", ylim = c(0, 1))
lines(data$Date, weights[, 2], col = "red")
lines(data$Date, weights[, 3], col = "green")
legend("topright", legend = c("AAPL", "NLY", "XOM"), col = c("blue", "red", "green"), lty = 1)

# Estadísticos descriptivos de las ponderaciones óptimas
descriptive_stats <- data.frame(
  "Stock" = c("AAPL", "NLY", "XOM"),
  "Media" = colMeans(weights),
  "Desviación Estándar" = apply(weights, 2, sd),
  "Mínimo" = apply(weights, 2, min),
  "Máximo" = apply(weights, 2, max)
)
print(descriptive_stats)
