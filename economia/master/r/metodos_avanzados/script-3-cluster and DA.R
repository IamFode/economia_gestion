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
# Script 3:
#
# Chapter 3.- Methods to create groups
#
#		3.1.- Cluster methods
#		3.2.- Discriminant analysis
# 
##############################################


# Set the working directory: setwd(...)


###################################################
# 3.3. Cluster analysis
###################################################


# 3.3.1 Method of K-means
###################################################

# spainclimate data set
spainclimate <- read.csv("data/data-spainclimate.csv",sep=";",dec=".",header=TRUE)
attach(spainclimate)


# Illustration with only two variables: hsun.annual and prec.annual
data.for.CA <- spainclimate[,c("hsun.annual","prec.annual")]

# scatter-plot of "hsun.annual" and "prec.annual"
plot(hsun.annual,prec.annual,type="n",frame=F)
text(hsun.annual,prec.annual,city.code,cex=0.5)


# K-means clustering with K=2
km2 <- kmeans(data.for.CA,2) # dos grupos
as.character(city)[km2$cluster==1]
as.character(city)[km2$cluster==2]

plot(hsun.annual,prec.annual,type="n",frame=F)
plot(hsun.annual,prec.annual,type="n",frame=F)
text(hsun.annual[km2$cluster==1],prec.annual[km2$cluster==1],city.code[km2$cluster==1],cex=0.5,col="red") 
text(hsun.annual[km2$cluster==2],prec.annual[km2$cluster==2],city.code[km2$cluster==2],cex=0.5,col="blue") 

points(mean(hsun.annual[km2$cluster==1]),mean(prec.annual[km2$cluster==1]),col="red",pch=3,lwd=2)  # centroid 1st group
points(mean(hsun.annual[km2$cluster==2]),mean(prec.annual[km2$cluster==2]),col="blue",pch=3,lwd=2) # centroid 2nd group

# K-means clustering with K=3
km3 <- kmeans(data.for.CA,3)
as.character(city)[km3$cluster==1]
as.character(city)[km3$cluster==2]
as.character(city)[km3$cluster==3]

plot(hsun.annual,prec.annual,type="n")
text(hsun.annual[km3$cluster==1],prec.annual[km3$cluster==1],city.code[km3$cluster==1],cex=0.5,col="red") 
text(hsun.annual[km3$cluster==2],prec.annual[km3$cluster==2],city.code[km3$cluster==2],cex=0.5,col="blue") 
text(hsun.annual[km3$cluster==3],prec.annual[km3$cluster==3],city.code[km3$cluster==3],cex=0.5,col="green")

points(mean(hsun.annual[km3$cluster==1]),mean(prec.annual[km3$cluster==1]),col="red",pch=3,lwd=2)  # centroid 1st group
points(mean(hsun.annual[km3$cluster==2]),mean(prec.annual[km3$cluster==2]),col="blue",pch=3,lwd=2) # centroid 2nd group
points(mean(hsun.annual[km3$cluster==3]),mean(prec.annual[km3$cluster==3]),col="green",pch=3,lwd=2) # centroid 3rd group


# calculation of SSWG and H

sswg <- numeric(6)
H <- numeric(5)
sswg[1] <- kmeans(data.for.CA,1)$tot.withinss
sswg[2] <- kmeans(data.for.CA,2)$tot.withinss
sswg[3] <- kmeans(data.for.CA,3)$tot.withinss
sswg[4] <- kmeans(data.for.CA,4)$tot.withinss
sswg[5] <- kmeans(data.for.CA,5)$tot.withinss
sswg[6] <- kmeans(data.for.CA,6)$tot.withinss
n <- length(hsun.annual)

for (K in 1:5){
	H[K] <- (n-K-1)*(sswg[K]-sswg[K+1])/sswg[K+1]
	}

H

plot(1:5,sswg[1:5])



###################################################
#
#
# Extra material: MAPS
#######################

# install.packages("maps")  
library(maps)

map(regions=c("Spain","Portugal","Morocco","Algeria","France","Canary Islands"),xlim=c(-20,5),ylim=c(27,44))
points(longitude,latitude,cex=0.25,pch=1)
text(longitude,latitude+0.20,city,cex=0.25)

# K-means clustering with K=2
map(regions=c("Spain","Portugal","Morocco","Algeria","France","Canary Islands"),xlim=c(-20,5),ylim=c(27,44))
points(longitude[km2$cluster==1],latitude[km2$cluster==1],cex=0.25,pch=1,col="red")
points(longitude[km2$cluster==2],latitude[km2$cluster==2],cex=0.25,pch=1,col="blue")
text(longitude[km2$cluster==1],latitude[km2$cluster==1]+0.20,city[km2$cluster==1],cex=0.25,col="red")
text(longitude[km2$cluster==2],latitude[km2$cluster==2]+0.20,city[km2$cluster==2],cex=0.25,col="blue")

# K-means clustering with K=3
map(regions=c("Spain","Portugal","Morocco","Algeria","France","Canary Islands"),xlim=c(-20,5),ylim=c(27,44))
points(longitude[km3$cluster==1],latitude[km3$cluster==1],cex=0.25,pch=1,col="red")
points(longitude[km3$cluster==2],latitude[km3$cluster==2],cex=0.25,pch=1,col="blue")
points(longitude[km3$cluster==3],latitude[km3$cluster==3],cex=0.25,pch=1,col="green")
text(longitude[km3$cluster==1],latitude[km3$cluster==1]+0.20,city[km3$cluster==1],cex=0.25,col="red")
text(longitude[km3$cluster==2],latitude[km3$cluster==2]+0.20,city[km3$cluster==2],cex=0.25,col="blue")
text(longitude[km3$cluster==3],latitude[km3$cluster==3]+0.20,city[km3$cluster==3],cex=0.25,col="green")

#
###################################################


# Example with 3 variables

# We can use a 3D scatterplot (interesting to check
# for patterns and groups with three variables)

# install.packages("rgl")  
library(rgl)  

data.for.CA <- cbind(temp.annual,hsun.annual,prec.annual)
km2 <- kmeans(data.for.CA,2)
plot3d(data.for.CA,type="n")
points3d(subset(data.for.CA,km2$cluster==1),col="red")
points3d(subset(data.for.CA,km2$cluster==2),col="blue")

km3 <- kmeans(data.for.CA,3)
plot3d(data.for.CA,type="n")
points3d(subset(data.for.CA,km3$cluster==1),col="red")
points3d(subset(data.for.CA,km3$cluster==2),col="blue")
points3d(subset(data.for.CA,km3$cluster==3),col="green")







# 3.3.1 Hierarchical clustering
###################################################


# Illustration with only two variables: hsun.annual and prec.annual
data.for.CA <- spainclimate[,c("hsun.annual","prec.annual")]

#Single linkage:
hc.single <- hclust(dist(data.for.CA),method="single")

#Complete linkage:
hc.complete <- hclust(dist(data.for.CA),method="complete")

# Centroid method:
hc.centroid=hclust(dist(data.for.CA),method="centroid")

# Dendograms:
plot(hc.single,labels=spainclimate$city.code,cex=0.5,main="")
plot(hc.complete,labels=spainclimate$city.code,cex=0.5,main="")
plot(hc.centroid,labels=spainclimate$city.code,cex=0.5,main="")


# Obtain the clusters at a certain height in the dendogram or for a given number of clusters
cutree(hc.complete,h=1000)
cutree(hc.complete,k=3)



 
 

###################################################
# 3.2 Discriminant analysis
###################################################


# 3.4.1 Fisher's linear discriminant function
###################################################

diabetes <- read.csv("data/data-diabetes.csv",header=TRUE,sep="")
attach(diabetes)

str(diabetes)

# The package MASS is required to use the function lda()
# install.packages("MASS")
library(MASS)

lda.diabetes <- lda(type~glu+bp+skin+bmi)

# histograms of the scores for each group
plot(lda.diabetes)


# prediction
woman1 <- data.frame(101,50,12,21)
names(woman1) <- names(diabetes[,1:4])
predict(lda.diabetes,woman1)

woman2 <- data.frame(160,75,25,30)
names(woman2) <- names(diabetes[,1:4])
predict(lda.diabetes,woman2)

# confusion matrix
predicted.class <- predict(lda.diabetes,diabetes[,1:4])$class
table(predicted.class,type)

# confusion table obtained by cross-validation (preferred)
n <- length(type)
predicted.class <- factor(rep(NA,n),levels=c("No","Yes"))
for (i in 1:n){
lda.cv <- lda(type~glu+bp+skin+bmi,data=diabetes[-i,])
predicted.class[i] <- predict(lda.cv,diabetes[i,1:4])$class
}
table(predicted.class,type)



