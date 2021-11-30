

####################################################################
# QUANTITATIVE METHODS: STATISTICS 
# Chapter 3: Statistical Inference
# Juan Carlos Pardo Fernández - UVigo
####################################################################


####################################################################
# SCRIPT 1: MONTE CARLO STUDIES AND THE BOOTSTRAP
####################################################################


####################################################################
# Monte Carlo simulation to check the performance of the sample mean
####################################################################


# Fix the number of Monte Carlo simulations M
M=10000 # Repetir varias veces la simulación

# Create a vector that will save the values of the estimator
mu.est=numeric(M) # vector m para guardar valores de media
mu.est
# Fix the true value of the parameter
mu=2 

# Fix the sample size (consider several values)
n=25 # tamaño muestral

# Repeat in a loop M times
for (i in 1:M){
	# Step 1.: simulate a sample from a distribution with mean mu	
  	x=rexp(n,rate=1/mu)  
  	# Step 2.: evaluate the estimator in the simulated sample
  	mu.est[i]=mean(x)   # compute the estimator
  }
hist(x)
mean(x)
# The vector "mu.est" contains M observations of the random variable sample mean. 
# Analyse the distribution of this random variable. For example:
( mse.est=mean((mu.est-mu)^2) )
( bias.est=mean(mu.est-mu) )#Sesgo nos debería dar un estimador muy próximo a 0 
mean(mu.est)
( variance.est=var(mu.est) )
" La media de la diferencia de la mu.est - mu al cuadrado debe ser muy parecido
a la varianza de mu.est "

round(c(bias.est,variance.est,mse.est),4)

hist(mu.est,freq=F,xlab="sample mean",main=paste("n=",n,sep=""))

####### Para n = 50 ##########
M = 10000
mu.est <- numeric(M)
mu = 2
n = 50 
for (i in 1:M){
  x <- rexp(n,rate=1/mu)
  mu.est[i] <- mean(x)
}
( mse.est=mean((mu.est-mu)^2) )
( bias.est=mean(mu.est-mu) )
( variance.est=var(mu.est) )

round(c(bias.est,variance.est,mse.est),4)
hist(mu.est,freq=F,xlab="sample mean",main=paste("n=",n,sep=""))

####### Para n = 100 ##########
M = 10000
mu.est <- numeric(M)
mu = 2
n = 100 
for (i in 1:M){
  x <- rexp(n,rate=1/mu)
  mu.est[i] <- mean(x)
}
( mse.est=mean((mu.est-mu)^2) )
( bias.est=mean(mu.est-mu) )
( variance.est=var(mu.est) )

round(c(bias.est,variance.est,mse.est),4)
hist(mu.est,freq=F,xlab="sample mean",main=paste("n=",n,sep=""))

############################ Métodos de los momentos ###########################
x = runif(20,min=0,max=7)
mean(x)*2

########################### Métodos de maximasimilitud #########################
sort(x)


"
Para poder comparar cual de los estimadores es mejor podemos utilizar el método 
de monte carlo, como se vera a continuación.
"
####################################################################
# Example: Monte Carlo simulation to check the performance 
# of two estimators of the parameter of U[0,theta]
####################################################################

# Fix the number of Monte Carlo simulations M
M=10000

# Create a vector that will save the values of the estimator
theta.est.1=numeric(M)
theta.est.2=numeric(M)

# Fix the true value of the parameter
theta=2

# Fix the sample size (consider sereral values)
n=25

# Repeat in a loop M times
for (i in 1:M){
	# Step 1.: simulate a sample from a distribution with mean mu	
  	x=runif(n,min=0,max=theta)  
  	
  	# Step 2.: evaluate the estimator in the simulated sample
  	theta.est.1[i]=2*mean(x)   # method of moments estimator
  	theta.est.2[i]=max(x)   # maximum likelihood estimator
  }

# Bias, variance and MSE:
( bias.est.1=mean(theta.est.1-theta) )
( variance.est.1=var(theta.est.1) )
( mse.est.1=mean((theta.est.1-theta)^2) )
( bias.est.2=mean(theta.est.2-theta) )
( variance.est.2=var(theta.est.2) )
( mse.est.2=mean((theta.est.2-theta)^2) )
# all values in a row:
round(c(bias.est.1,variance.est.1,mse.est.1,bias.est.2,variance.est.2,mse.est.2),4)
"
El método de momentos se comporta mejor en temas de sesgo y el método de monte
carlo se comporta mejor en términos de varianza.
El método de monte Carlo es mejor por tiene menor error cuatrático medio.
Mientas más datos entonces el método monte carlo es mejor que el método de momentos
"
# Eficiencia relativa entre los errores cuadráticos medios de dos estimadores
( mse.est.2=mean((theta.est.2-theta)^2) ) / ( mse.est.1=mean((theta.est.1-theta)^2) )


par(mfrow=c(1,2))
#estimador de momentos
hist(theta.est.1,freq=F,xlab="moments",main=paste("n=",n,sep=""))
abline(v=theta,lty=2,col="red",lwd=2)
#Estimador de maximasimilitud
hist(theta.est.2,freq=F,xlab="MLE",main=paste("n=",n,sep=""))
abline(v=theta,lty=2,col="red",lwd=2)

# Important questions:
# Does theta.est.1 behave like a Normal distribution?
# Does theta.est.2 behave like a Normal distribution?


############################## Ejercicio 1 #####################################
x =  rnorm(20,mean=7,sd=2)
mean(x)
median(x)

M = 10000
mu.est <- numeric(M)
median.est <- numeric(M)
mu = 7 
sigma = 1  
n = 25 
for (i in 1:M){
  x <- rnorm(n,mean=mu,sd=sigma)
  mu.est[i] <- mean(x)
  median.est[i] <- median(x)
}

( bias.est=mean(mu.est-mu) )
( variance.est=var(mu.est) )
( mse.est=mean((mu.est-mu)^2) )

( bias.est=mean(median.est-mu) )
( variance.est=var(median.est) )
( mse.est=mean((median.est-mu)^2) )

# ¿Es la mediana de la muestra un estimador insesgado? ¿Por qué?
## Si, porque su sesgo es pequeño como también su varianza

"Para cada tamaño de muestra, calcule la eficiencia relativa entre los estimadores. 
¿Qué concluirías? ¿Qué estimador tiene un mejor desempeño práctico?"
"El mejor estimador será para mean ya que su error cuadratico medio es menor"

# MSE_mean / MSE_median
( mse.est=mean((mu.est-mu)^2) ) / ( mse.est=mean((median.est-mu)^2) )

hist(x)

################################ Ejercicio 2 ###################################


####################################################################
# Monte Carlo parametric vs. non-parametric estimation
####################################################################


# Fix the number of Monte Carlo simulations M
M=10000

# Create a vector that will save the values of the estimator
tau.est.1=numeric(M)
tau.est.2=numeric(M)

# Fix the sample size (consider sereral values)
n=50

# Repeat in a loop M times
for (i in 1:M){
	# Step 1.: simulate a sample from a distribution with mean mu	
  	# x=rnorm(n); tau=qnorm(0.95)  
  	# x=rt(n,df=5); tau=qt(0.95,df=5)
  	 x=rexp(n,rate=0.5); tau=qexp(0.95,rate=0.5)
  	
  	# Step 2.: evaluate the estimator in the simulated sample
  	tau.est.1[i]=quantile(x,0.95)   # non-parametric estimator
  	tau.est.2[i]=mean(x) + qnorm(0.95)*sd(x)  # parametric estimator
  }

# Bias, variance and MSE of the estimators:
( bias.est.1=mean(tau.est.1-tau) )
( variance.est.1=var(tau.est.1) )
( mse.est.1=mean((tau.est.1-tau)^2) )
( bias.est.2=mean(tau.est.2-tau) )
( variance.est.2=var(tau.est.2) )
( mse.est.2=mean((tau.est.2-tau)^2) )

# all values in a row:
round(c(bias.est.1,variance.est.1,mse.est.1,bias.est.2,variance.est.2,mse.est.2),4)

par(mfrow=c(1,2))
hist(tau.est.1,freq=F,xlab="non-parametric estimator",main=paste("n=",n,sep=""))
abline(v=tau,lty=2,col="red",lwd=2)
hist(tau.est.2,freq=F,xlab="parametric estimator",main=paste("n=",n,sep=""))
abline(v=tau,lty=2,col="red",lwd=2)







###############################################
#   BOOTSTRAP
###############################################
#
# Example: bootstrap estimation of the s.e. 
# of the estimators of the mean
#
##########################################

# data:
x = c(0.05, 0.07, 0.08, 0.15, 0.18, 0.21, 0.43, 0.48, 0.52, 0.62, 0.65, 0.67, 0.70, 0.81, 0.86, 0.89, 0.91, 0.96, 1.02, 1.05, 1.19, 1.23, 1.44, 1.58, 1.65, 1.79, 1.82, 1.84, 1.93, 1.97, 2.26, 2.27, 2.34, 2.59, 2.76, 3.09, 3.39, 3.54, 3.62, 3.70, 4.28, 4.33, 4.60, 4.69, 4.91, 5.19, 5.35, 6.02, 6.38, 6.76)

# sample size
n=length(x)

theta.est=mean(x)

# bootstrap algorithm
B=1000
theta.boot=numeric(B)
for (b in 1:B){
	xboot=sample(x,n,replace=T)  # bootstrap resample X1*,...,X2*
	theta.boot[b]=mean(xboot)    # bootstrap estimation 
	}

# estimation of the bias the estimator or theta:
( bias.boot=mean(theta.boot)-theta.est )

# approximation of the standard error of the estimator or theta
( se.boot=sd(theta.boot) )

hist(theta.boot)




