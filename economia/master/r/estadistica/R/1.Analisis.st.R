# -------------- Series temporales ---------------

## Series con estacionalidad: serie trimestral del IPC

ipc <- read.csv2(file="../Dat/IPC/IPC_mensual.csv",encoding="UTF8")
ipc.ts <- ts(ipc$IPC, start = c(2002,1), frequency = 12)

plot(ipc.ts, type="o", xlab = "Mes", ylab = "IPC (%)")
plot(ipc.ts, type="l", xlab = "Mes", ylab = "IPC (%)")
# se observa estacionalidad --> la consideramos homocedástica

# Descomposición aditiva
ipc.comp <- decompose(ipc.ts, type = "additive") # Descompone la serie 
plot(ipc.comp, xlab="Mes") # gráfica de la descomposición aditiva

# 
ipcSP <- ipc.comp$trend # ciclo tendencia
ipc$S <- ipc.comp$seasonal
ipc$U <- ipc.comp$random

barplot(ipc.comp$figure, names = factor(1:12))
ts.plot(ipc.ts, lty = "solid", col = "black", xlab = "Mes", ylab = "IPC")
ts.plot(ipc.ts-ipc.comp$seasonal, lty = "solid", col = "black", xlab = "Mes", ylab = "IPC") # serie suavizada = a cada valor mensual se le resta el efecto estacional

# las series juntas anteriores juntas
t <- cbind(ipc.ts, ipc.ts - ipc.comp$seasonal)
ts.plot(t, lty = c("solid","dashed"), col = c("black","red"), xlab = "Mes", ylab = "IPC")
legend("bottomleft",c("Series original", "Serie desestacionalizada"), lty = c("solid","dashed"), col = c("black","red"),lwd=c(1,1))

#------------- Serie anual -------------
substr(ipc$Mes, 1, 4) # sustrae los cuatro primeros digitos de los datos
ipc$Ano <- as.integer(substr(ipc$Mes, 1, 4))
ipca <- data.frame(Ano = seq(min(ipc$Ano), max(ipc$Ano)), IPC=tapply(ipc$IPC, ipc$Ano, mean))
ipca.ts <- ts(ipca$IPC, start = 2002)

## Para suavizar se puede utilizar medias mobiles de orden 3

ipca$P3 <- filter(ipca.ts, sides = 2, rep(1/3,3)) #mm3
plot(ipca$IPC, type="o")
lines(ipca$P3, lty = "dashed", col = "blue")
reg <- lm(IPC ~ Ano, data = ipca)
summary(reg)
### representar la regresión simple
abline(reg,col="red")

## Medias modeviles de orden 5
ipca$P5 <- filter(ipca.ts, sides = 2, rep(1/5,5)) #mm5
plot(ipca$P5, type = "o")
summary(reg)
abline(reg,col="red")

### La idea es realizar regresiones para series de tiempo más cortos
reg <- lm(IPC ~ Ano, data = ipca[ipca$Ano>2014,])
summary(reg)
abline(reg, col="blue")
