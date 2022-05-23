########################################################
#
# Máster en Economía. Curso 2021-2022
#
# Advanced Techniques for Data Analysis (1st part)
#
# Juan Carlos Pardo-Fernández (Universidade de Vigo)
#
########################################################



##############################################
#
# Script 2:
#
# Chapter 2.- Methods for dimension reduction
#
# 	2.1.- Principal component analysis
#	  2.2.- Factor analysis
#
##############################################



##############################################
#
# 2.1.- Principal component analysis (PCA)
#
##############################################


# Set the working directory: setwd(...)

# Upload the data set to R
usair <- read.csv("data/data-usair.csv",header=TRUE,sep="")

# Select the variables in the PCA analysis
data.for.PCA <- usair[,3:8]

# Matriz de correlaciones
cor(data.for.PCA)

# Perform the PCA analysis based on the correlation matrix
# calcular la matriz de correlaciones
PCA.usair <- princomp(data.for.PCA,cor=TRUE) # cor=TRUE estandarizada

# Proportion of variability explained by each component
summary(PCA.usair)

# Loadings: unit vectors that form the components
# as combinations of the original variables
# vectores que forman los componentes principales (autovectores)
PCA.usair$loadings

# teneos que buscar números grandes no importa el signo 

# Scores: values of the principal components for each case
PCA.usair$scores
# nuevo conjunto matriz transformado en componentes principales

# Correlations between the original variables and the principal components
cor(data.for.PCA,PCA.usair$scores)

# Scree-plot
screeplot(PCA.usair)

# Scatter-plot of the first 2 PCs
plot(PCA.usair$scores[,1],PCA.usair$scores[,2],xlab="1st PC",ylab="2nd PC",type="n",frame=F)
text(PCA.usair$scores[,1],PCA.usair$scores[,2],usair$City,cex=0.75)

# Pairwise scatter-plot of the first 3 PCs
pairs(PCA.usair$scores[,1:3])

# Biplot
biplot(PCA.usair)   # first two PC with numbers
biplot(PCA.usair,xlabs=usair$City)   # first two PC with city names
biplot(PCA.usair,xlabs=usair$City,cex=0.5)   # first two PC with smaller city names

# to plot other components, use the argument "choices"
biplot(PCA.usair,choices=c(1,2)) 
biplot(PCA.usair,choices=c(2,3))
biplot(PCA.usair,choices=c(1,3))







##############################################
#
# 2.2.- Factor analysis 
#
##############################################


# Set the working directory: setwd(...)


# Example: one-factor model
world2016 <- read.csv("data-world2016.csv",sep=",",header=TRUE)
str(world2016)
data.for.FA <- world2016[,c("life.expectancy","fertility.rate","mortality.rate")]
pairs(data.for.FA)
var(data.for.FA)
cor(data.for.FA)

# solution to the six-equation system based on the variance-covariance matrix
s <- var(data.for.FA)
lambda1 <- -sqrt(s[1,2]*s[1,3]/s[2,3])
lambda2 <- sqrt(s[2,1]*s[2,3]/s[1,3])
lambda3 <- sqrt(s[3,2]*s[3,1]/s[2,1])
psi1 <- s[1,1]-lambda1^2
psi2 <- s[2,2]-lambda2^2
psi3 <- s[3,3]-lambda3^2
( Lambda.var <- c(lambda1,lambda2,lambda3) )
( Psi.var <- c(psi1,psi2,psi3) )

# solution to the six-equation system based on the correlation matrix
s <- cor(data.for.FA)
lambda1 <- -sqrt(s[1,2]*s[1,3]/s[2,3])
lambda2 <- sqrt(s[2,1]*s[2,3]/s[1,3])
lambda3 <- sqrt(s[3,2]*s[3,1]/s[2,1])
psi1 <- s[1,1]-lambda1^2
psi2 <- s[2,2]-lambda2^2
psi3 <- s[3,3]-lambda3^2
( Lambda.cor <- c(lambda1,lambda2,lambda3) )
( Psi.cor <- c(psi1,psi2,psi3) )

# checking the invariance for scale changes
Lambda.var/sqrt(diag(var(data.for.FA)))
Psi.var/diag(var(data.for.FA))


# Factor analysis in R:
factanal(data.for.FA,factors=1)

# scores:
scores <- factanal(data.for.FA,factors=1,scores="regression")$scores

# scores for each country
cbind.data.frame(world2016$country,scores)

# scores for each country ordered
cbind.data.frame(world2016$country[order(scores)],scores[order(scores)])





# Example: Data set "usair"

usair <- read.csv("data/data-usair.csv",header=TRUE,sep="")

# Factor analysis with 1,2 and 3 factors (check the p-values!)
factanal(usair[,3:8],factors=1)
factanal(usair[,3:8],factors=2)
factanal(usair[,3:8],factors=3)

# scores (3 factors) and scatter-plots
scores <- factanal(usair[,3:8],factors=3,scores="regression")$scores

par(mfrow=c(1,3))

plot(scores[,1],scores[,2],xlab="1st factor",ylab="2nd factor",type="n")
text(scores[,1],scores[,2],usair$City,cex=0.5)

plot(scores[,1],scores[,3],xlab="1st factor",ylab="3rd factor",type="n")
text(scores[,1],scores[,3],usair$City,cex=0.5)

plot(scores[,2],scores[,3],xlab="2nd factor",ylab="3rd factor",type="n")
text(scores[,2],scores[,3],usair$City,cex=0.5)

