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
# Script 1:
#
# Chapter 1.- Searching for data patterns
#
##############################################



# Set the working directory: setwd(...)

# Upload a data set to R
world2016 <- read.csv("data/data-world2016.csv",header=TRUE,dec=".",sep=",")

# Quick summary of the data set
str(world2016)

# Extracting a variable
world2016$life.expectancy
world2016[,9]
world2016[,"life.expectancy"]

# This function incorporates the variables to the working session
attach(world2016)
life.expectancy

# The opposite of attach is detach():
# detach(world2016)

# hist
hist(life.expectancy)

# Box-plots
boxplot(fertility.rate~income.level,xlab="income level",ylab="fertility rate")

# Note that the variables "income.level" and "income.level.code" are 
# not correctly ordered. We'd better put them in the correct order so 
# the graphs, tables, etc. are correct.
income.level <- factor(income.level,levels=c("Low","Lower middle","Upper middle","High"))
income.level.code <- factor(income.level.code,levels=c("L","LM","UM","H"))

boxplot(fertility.rate~income.level,xlab="income level",ylab="fertility rate")



# Pairwise scatter-plot of all numerical variables (colums 9-20)
pairs(world2016[,9:20])
pairs(world2016[,9:20],panel=panel.smooth)

pairs(world2016[,9:11])
pairs(world2016[,9:11],panel=panel.smooth)


# Vector of means
colMeans(world2016[,9:20])

# Variance-covariance matrix
var(world2016[,9:20])

# Correlation matrix
cor(world2016[,9:20])



# Mahalanobis distance
# Two variables: "log.GDP.percapita", "life.expectancy"

# scatter-plot
plot(log.GDP.percapita,life.expectancy)

# scatter-plot with the country codes
plot(log.GDP.percapita,life.expectancy,type="n")  # this generates an empty scatter-plot
text(log.GDP.percapita,life.expectancy,code,cex=0.5)
# Add the regression line
abline(lm(life.expectancy~log.GDP.percapita),lty=2)
# Add the vector of means
points(mean(log.GDP.percapita),mean(life.expectancy),pch=3,cex=1.5,lwd=2,col="red")


# Mahalanobis distance of Spain
data.for.mahalanobis=data.frame(log.GDP.percapita,life.expectancy)
M <- colMeans(data.for.mahalanobis)
S <- cov(data.for.mahalanobis)
sqrt(mahalanobis(data.for.mahalanobis[which(country=="Spain"),],center=M,cov=S))

# Mahalanobis distance of Argentina
sqrt(mahalanobis(data.for.mahalanobis[which(country=="Argentina"),],center=M,cov=S))


# Mahalanobis distances for all countries
mahalanobis.distances.2 <- apply(data.for.mahalanobis,1,mahalanobis,center=M,cov=S)
mahalanobis.distances <- sqrt(mahalanobis.distances.2)
cbind.data.frame(country,mahalanobis.distances)

# Scatter-plot with the order of the countries according to their Mahalanobis distance
plot(log.GDP.percapita,life.expectancy,type="n")
ord <- order(mahalanobis.distances)
text(log.GDP.percapita[ord],life.expectancy[ord],1:length(ord),cex=0.5)
# Add the regression line
abline(lm(life.expectancy~log.GDP.percapita),lty=2)
# Add the vector of means
points(mean(log.GDP.percapita),mean(life.expectancy),pch=3,cex=1.5,lwd=2,col="red")


# Euclidean distances for all countries (use the function "mahalanobis" with cov=identity matrix)
euclidean.distances.2 <- apply(data.for.mahalanobis,1,mahalanobis,center=M,cov=diag(c(1,1)))
euclidean.distances <- sqrt(euclidean.distances.2)
cbind.data.frame(country,euclidean.distances)

# Scatter-plot with the order of the countries according to their Euclidean distance 
# with respect to the vector of means. 
# Notice that the horizontal and vertical axes have different scales,
# and hence the graph is somehow distorted
plot(log.GDP.percapita,life.expectancy,type="n")
ord <- order(euclidean.distances)
text(log.GDP.percapita[ord],life.expectancy[ord],1:length(ord),cex=0.5)
# Add the regression line
abline(lm(life.expectancy~log.GDP.percapita),lty=2)
# Add the vector of means
points(mean(log.GDP.percapita),mean(life.expectancy),pch=3,cex=1.5,lwd=2,col="red")



# Box-plot of the Mahalanobis distances by income level
boxplot(mahalanobis.distances~income.level.code,ylab="Mahalanobis distances",xlab="income level",frame=FALSE)

