# LIBRERIAS
library(ggplot2)

####### ANÁLISIS DE DATOS (análisis Dwyer and Fisher (2009, Fig. 3)) ###########
# Histograms
hist(P_M.Y$M.Y)
hist(P_M.Y$P)

# Boxplots
boxplot(P_M.Y$M.Y,P_M.Y$P,na.action="na.omit", outlier.shape = ".")
par(mfrow=c(1,2))
sapply(seq(3,4), function(j)boxplot(P_M.Y[,j],
                                    main=colnames(P_M.Y)[j],xlab="",
                                    col="yellow",pch=1,cex=.3))

# Medias y Medianas
summary(P_M.Y$P)
summary(P_M.Y$M.Y)


# ANÁLISIS DE OUTLIERS
outlier = boxplot.stats(P_M.Y$M.Y)$out
outlier_index = which(P_M.Y$M.Y %in% c(outlier))
outlier_df = P_M.Y[outlier_index,-4]
plot(outlier_df$M.Y)
iqr = IQR(P_M.Y$M.Y,na.rm = TRUE)
up = quantile(P_M.Y$M.Y,0.75,na.rm = TRUE)+1.5*iqr
low = quantile(P_M.Y$M.Y,0.25,na.rm = TRUE)-1.5*iqr
P_M.Youtlier = subset(P_M.Y,P_M.Y$M.Y>low & P_M.Y$M.Y<up)

outlier = boxplot.stats(P_M.Youtlier$P)$out
outlier_index = which(P_M.Youtlier$P %in% c(outlierP))
iqr = IQR(P_M.Youtlier$P,na.rm = TRUE)
up = quantile(P_M.Youtlier$P,0.75,na.rm = TRUE)+1.5*iqr
low = quantile(P_M.Youtlier$M.Y,0.25,na.rm = TRUE)-1.5*iqr
P_M.Youtlier = subset(P_M.Youtlier,P_M.Youtlier$P>low & P_M.Youtlier$P<up)

ggplot(P_M.Youtlier,aes(x=M.Y,y=P))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5)

reg = lm(P~M.Y,data=P_M.Youtlier)
summary(reg)

'"
Al eliminar los outliers con la formula "cuantil 75 y 25 +- 1.5*iqr" eliminamos
muchos datos relevantes, por lo que pasaremos a eliminar sólo los datos que están 
muy seperados de la gráfico de dispersión
"'


# ANÁLISIS DE DATOS EXTREMOS
subset(P_M.Y,P<=0.5 & M.Y>=2)
'"
Australia 1976 y 1980; M.Y = 2,21 y 2.43 respectivamente.
Namibia 2002; M.Y = 2.63
"'
subset(P_M.Y,M.Y<=-2)
'"
Australia 1978 M.Y = -2.19
"'
P_M.Yextremo = P_M.Y[-c(821,823,825,10207),]
reg = lm(P~M.Y,data=P_M.Yextremo,na.action = "na.omit")
summary(reg)


################################ REMOVE ########################################
rm(outlier)
rm(outlier_index)
rm(outlier_df)
rm(iqr)
rm(up)
rm(low)
