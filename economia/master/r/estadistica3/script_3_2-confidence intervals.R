
####################################################################
# QUANTITATIVE METHODS: STATISTICS 
# Chapter 3: Statistical Inference
# Juan Carlos Pardo Fernández - UVigo
####################################################################



####################################################################
# SCRIPT 2: CONFIDENCE INTERVALS
####################################################################

##################
# Simulation
##################


# X~N(mu,sigma)
# Confidence interval for mu

n.simulations=1000

# confidence level 1-alpha
alpha=0.05  

#sample size
n=10

#vectors to save the confidence intervals
a=numeric(n.simulations)
b=numeric(n.simulations)

for (i.sim in 1:n.simulations){
	# simulation of the data
	x=rnorm(n,mean=7,sd=2)
	
	# confidence interval (a,b)
	a[i.sim]=mean(x)-qt(1-alpha/2,n-1)*sd(x)/sqrt(n)
	b[i.sim]=mean(x)+qt(1-alpha/2,n-1)*sd(x)/sqrt(n)
	}

# proportion of intervals covering the parameter
mean( (a<=7) & (b>=7) )





###############################################
# Bootstrap confidence intervals for the mean
###############################################

# Example:

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

# bootstrap estimation of the s.e.
se.boot=sd(theta.boot)


# bootstrap 0.95-confidence interval: Normal
a=theta.est-1.96*se.boot
b=theta.est+1.96*se.boot
c(a,b)

# bootstrap 0.95-confidence interval: percentile method
a=quantile(theta.boot,0.025)
b=quantile(theta.boot,0.975)
c(a,b)

# bootstrap 0.95-confidence interval: pivotal method
a=2*theta.est-quantile(theta.boot,0.975)
b=2*theta.est-quantile(theta.boot,0.025)
c(a,b)

