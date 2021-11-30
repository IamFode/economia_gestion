
#####################################################################
# QUANTITATIVE METHODS: STATISTICS 
# Chapter 3: Statistical Inference
# Juan Carlos Pardo Fernández - UVigo
####################################################################



####################################################################
# SCRIPT 3: HYPOTHESIS TESTING
####################################################################

################################################
# Simulation of the behaviour of an exact test
################################################


# X~N(mu,sigma)
# Test
# H_0 : mu = mu0
# H_1 : mu > mu0

n.simulations=10000

# confidence level 1-alpha
alpha=0.05  

#sample size
n=10

mu0=5

# vectors to save the test statistics and the p-values
test.statistic=numeric(n.simulations)
p.value=numeric(n.simulations)

for (i.sim in 1:n.simulations){
  # simulation of the data
  x=rnorm(n,mean=5,sd=2)  # data under the null hypothesis
 # x=rnorm(n,mean=5.5,sd=2)   # data under the alternative hypothesis (several values for the mean can be considered)
  
  # test statistic
  test.statistic[i.sim]=sqrt(n)*(mean(x)-mu0)/sd(x)
  
  # p-value (from the t_{n-1} distribution)
  p.value[i.sim]=1-pt(test.statistic[i.sim],n-1)
   
}

# critical value
  cv=qt(1-alpha,n-1)
  
# proportion of rejections based on the values of the test statistic and the critical value
  mean(test.statistic>cv)
  
# proportion of rejections based on the p-values
  mean(p.value<alpha)
  
# distribution of the test statistic
  hist(test.statistic,freq=F)
  
# distribution of the p-values
  hist(p.value,freq=F)


  
  
################################################
# Simulation of the behaviour of an asymptotic test
################################################
  
  
  # X with mean mu=E(X)
  # Test
  # H_0 : mu = mu0
  # H_1 : mu > mu0
  
  n.simulations=10000
  
  # confidence level 1-alpha
  alpha=0.05  
  
  #sample size
  n=10
  
  mu0=5
  
  # vectors to save the test statistics and the p-values
  test.statistic=numeric(n.simulations)
  p.value=numeric(n.simulations)
  
  for (i.sim in 1:n.simulations){
    # simulation of the data from a Uniform[0,theta]
    x=runif(n,0,10)   # data under the null hypothesis
    # simulation of the data from a Exponential(lambda)
   # x=rexp(n,rate=1/5)  # data under the null hypothesis 
    
    # test statistic
    test.statistic[i.sim]=sqrt(n)*(mean(x)-mu0)/sd(x)
    
    # p-value (from the t_{n-1} distribution)
    p.value[i.sim]=1-pnorm(test.statistic[i.sim])
    
  }
  
  # critical value
  cv=qnorm(1-alpha)
  
  # proportion of rejections based on the values of the test statistic and the critical value
  mean(test.statistic>cv)
  
  # proportion of rejections based on the p-values
  mean(p.value<alpha)
  
  # distribution of the test statistic
  hist(test.statistic,freq=F)
  
  # distribution of the p-values
  hist(p.value,freq=F)
  
  
  
  
  
  ################################################
  # Example: fuel consumption
  ################################################
  
  # X ~ N(mu,sigma)
  
  # H_0 : mu = 7
  # H_1 : mu < 7
  
  # significance level
  alpha=0.05
  
  # data
  x=c(6.61,6.55,6.64,7.02,6.25,6.57,6.82,6.87,6.60,6.94)
  n=length(x)
  
  # test statistic
  ( d=sqrt(n)*(mean(x)-7)/sd(x) )
  
  # critical value
  ( qt(alpha,n-1)  )
  
  # p-value
  ( p.value=pt(d,n-1) )
  
  # R function
  t.test(x,mu=7,alternative="less")
  
  
  
  
  
  ################################################
  # Example: EXPENDITURES DATA
  ################################################
  
  # working directory
  # setwd(...)
  
  expenditures=read.table("data-expenditures.txt",header=T)
  attach(expenditures)
  str(expenditures)
  table(members)
  
  # means of "total" and "food" by values of "members"
  tapply(total,members,mean)
  tapply(food,members,mean)
  
  
  ################################################
  # t-test for "food" (2 members and 3 members)
  ################################################
  
  t.test(food[members==2],food[members==3],alternative="less")
  
  
  ################################################
  # Wilcoxon-Mann-Whitney test
  ################################################
  
  nonsmoker=c(28.6, 25.1, 26.4, 34.9, 29.8, 28.4, 38.5, 30.2, 30.6, 31.8, 41.6, 21.1, 36.0, 37.9, 13.9)
  smoker=c(69.3, 56.0, 22.1, 47.6, 53.2, 48.1, 23.2, 13.8, 52.7, 34.4, 60.2, 43.8)
  
  wilcox.test(smoker,nonsmoker,alternative="greater")
  
  
  
  
  
  ################################################
  # ANOVA
  ################################################
  
  
  boxplot(food~members,frame=F,xlab="members",ylab="food expenditure")
  
  summary(aov(food~as.factor(members)))
  
  TukeyHSD(aov(food~as.factor(members),data=expenditures))
  
  bartlett.test(food~as.factor(members))
  
  
  
  
  ################################################
  # qq-plot for normality
  ################################################
  
  qqnorm(total[members==2])
  
  qqnorm(food[members==2])
  
  
  
  
  ################################################
  # Goodness-of-fit test of Normality
  ################################################
  
  #install.packages("nortest")  # required to use lillie.test()
  library(nortest)
  
  lillie.test(total[members==2])
  shapiro.test(total[members==2])
  lillie.test(food[members==2])
  shapiro.test(food[members==2])
    
  
  