library(ggplot2)
library(foreign)

var <- read.dta("http://www.stata-press.com/data/r9/auto.dta")

cond <- (var$foreign=="Domestic" & var$weight>4000) | (var$foreign=="Domestic" & var$weight<2000)

var1 <- var[cond,]

var1 <- subset(var,cond)

var1$foreign

####### gráficos ############### 

ggplot(var$foreign)+
  geom_bar(stat = "identity",color="white")+
  coord_polar(theta="y")

### gráfico de dispersión
plot(var$price~var$weight, pch = 19)
abline(lm(var$price~var$weight))

      