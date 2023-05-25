#LIBRARY
library(dplyr)
library(stats)
library(ggplot2)

#DATA
pib = read.csv("data/b5e.csv",dec = ",")
pib = read.csv("data/b5a.csv",dec = ",")
colnames(pib) = c("año","PIB","C","G","I","X","IM")
#CLEAN
pib = pib %>% arrange(año)

########################################################
pib["XN"] = pib$X-pib$IM
pib["XN/PIB"] = pib$XN/pib$PIB

#LOG
columnas = c("PIB",
             "C",
             "G",
             "I",
             "X",
             "IM")
newColumnas = paste0("log",columnas)
pib[,newColumnas] = sapply(pib[,columnas],log)
lambda = 100            
PIB.hp=hpfilter(pib$logPIB,type = "lambda",freq = lambda)
C.hp=hpfilter(pib$logC,type = "lambda",freq = lambda)
G.hp=hpfilter(pib$logG,type = "lambda",freq = lambda)
I.hp=hpfilter(pib$logI,type = "lambda",freq = lambda)
X.hp=hpfilter(pib$logX,type = "lambda",freq = lambda)
IM.hp=hpfilter(pib$logIM,type = "lambda",freq = lambda)
NX.hp=hpfilter(pib$`XN/PIB`,type = "lambda",freq = lambda)


{par(mfrow=c(3,2))
plot(PIB.hp$cycle, 
     type="l", 
     col="darkgreen", 
     xlab="Periodo",
     ylab="Ciclo",
     main="vs C")
lines(C.hp$cycle, 
      col="darkblue")
abline(h=0, col="red")
legend("topright", 
       legend=c("PIB", 
                "C"), 
       col=c('darkgreen', 
             'darkblue'), 
       lty=1, 
       cex=0.5)
plot(PIB.hp$cycle, 
     type="l", 
     col="darkgreen", 
     xlab="Periodo",
     ylab="Ciclo",
     main="vs G")
lines(G.hp$cycle, 
      col="darkblue")
abline(h=0, col="red")
legend("topright", 
       legend=c("PIB", 
                "G"), 
       col=c('darkgreen', 
             'darkblue'), 
       lty=1, 
       cex=0.5)
plot(PIB.hp$cycle, 
     type="l", 
     col="darkgreen", 
     xlab="Periodo",
     ylab="Ciclo",
     main="vs I")
lines(I.hp$cycle, 
      col="darkblue")
abline(h=0, col="red")
legend("topright", 
       legend=c("PIB", 
                "I"), 
       col=c('darkgreen', 
             'darkblue'), 
       lty=1, 
       cex=0.5)
plot(PIB.hp$cycle, 
     type="l", 
     col="darkgreen", 
     xlab="Periodo",
     ylab="Ciclo",
     main="vs X")
lines(X.hp$cycle, 
      col="darkblue")
abline(h=0, col="red")
legend("topright", 
       legend=c("PIB", 
                "X"), 
       col=c('darkgreen', 
             'darkblue'), 
       lty=1, 
       cex=0.5)
plot(PIB.hp$cycle, 
     type="l", 
     col="darkgreen", 
     xlab="Periodo",
     ylab="Ciclo",
     main="vs IM")
lines(IM.hp$cycle, 
      col="darkblue")
abline(h=0, col="red")
legend("topright", 
       legend=c("PIB", 
                "IM"), 
       col=c('darkgreen', 
             'darkblue'), 
       lty=1, 
       cex=0.5)
plot(PIB.hp$cycle, 
     type="l", 
     col="darkgreen", 
     xlab="Periodo",
     ylab="Ciclo",
     main="vs NX")
lines(NX.hp$cycle, 
      col="darkblue")
abline(h=0, col="red")
legend("topright", 
       legend=c("PIB", 
                "NX"), 
       col=c('darkgreen', 
             'darkblue'), 
       lty=1, 
       cex=.5)}

#desviaciones
sd(PIB.hp$cycle)
sd(C.hp$cycle)
sd(G.hp$cycle)
sd(I.hp$cycle)
sd(X.hp$cycle)
sd(IM.hp$cycle)
sd(NX.hp$cycle)

#sd(x)/sd(y)
sd(PIB.hp$cycle)/sd(PIB.hp$cycle)
sd(C.hp$cycle)/sd(PIB.hp$cycle)
sd(G.hp$cycle)/sd(PIB.hp$cycle)
sd(I.hp$cycle)/sd(PIB.hp$cycle)
sd(X.hp$cycle)/sd(PIB.hp$cycle)
sd(IM.hp$cycle)/sd(PIB.hp$cycle)
sd(NX.hp$cycle)/sd(PIB.hp$cycle)

#prociclica? >0 
cor(PIB.hp$cycle,var)

# Función para calcular correlación con diferentes retardos entre dos columnas de un dataframe

var = PIB.hp$cycle
var = C.hp$cycle
var = G.hp$cycle
var = I.hp$cycle
var = X.hp$cycle
var = IM.hp$cycle
var = NX.hp$cycle

{cat("\n\n\n")
cat("t-2=",cor(PIB.hp$cycle,lag(var,n=2), use = "pairwise.complete.obs"),"\n")
cat("t-1=",cor(PIB.hp$cycle,lag(var,n=1), use = "pairwise.complete.obs"),"\n")
cat("t=",cor(PIB.hp$cycle,var, use = "pairwise.complete.obs"),"\n")
cat("t+1=",cor(lag(PIB.hp$cycle,n=1),var, use = "pairwise.complete.obs"),"\n")
cat("t+2=",cor(lag(PIB.hp$cycle,n=2),var, use = "pairwise.complete.obs"),"\n")}

