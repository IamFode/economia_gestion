
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


####### ANÁLISIS DE DATOS (análisis Dwyer and Fisher (2009, Fig. 4)) ###########
# Histograms
hist(P_M.Y40geom$M.Y)
hist(P_M.Y40geom$P)

#Resumen estadística
summary(P_M.Y40geom$M.Y)
summary(P_M.Y40geom$P)

#Boxplots
dev.new(width=32, height=32)
df_list <- list(P_M.Y5geom,P_M.Y10geom,P_M.Y25geom,P_M.Y40geom)
par(mfrow=c(1, 8), mar=c(2, 2, 1, 1))
lapply(df_list, function(df) {
  sapply(seq(3,4), function(j) {
    boxplot(df[,j], main=colnames(df)[j], xlab="", col="yellow", pch=1, cex=.3)
  })
})


# DATOS EXTREMOS
subset(P_M.Y,P<=0.5 & M.Y>=2)
'"
Australia 1976 y 1980; M.Y = 2,21 y 2.43 respectivamente.
Namibia 2002; M.Y = 2.63
"'
subset(P_M.Y,M.Y<=-2)
'"
Australia 1978 M.Y = -2.19
Indonesia 1966 P = -4.34575746
"'
subset(P_M.Y,P<=-4)
'"
Indonesia 1966 P = -4.34575746
"'



'"
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


Al eliminar los outliers con la formula "cuantil 75 y 25 +- 1.5*iqr" eliminamos
muchos datos relevantes, por lo que pasaremos a eliminar sólo los datos que están 
muy seperados de la gráfico de dispersión
"'

################################ REMOVE ########################################
rm(outlier)
rm(outlier_index)
rm(outlier_df)
rm(iqr)
rm(up)
rm(low)
