# Regresión lineal con EES
## Regresión simple
ees <- read.csv2(file = "../Dat/EES/EES_2018_prep.csv",encoding =  "UTF8")

reg_simple <- lm(BASE~SEXO, data = ees)
summary(reg_simple)

table(ees$ESTU)

ees$ESTU <- factor(ees$ESTU)
levels(ees$ESTU) <- c("AN","EP","EP","ES","ES","ES","UN")
table(ees$ESTU)
reg_simple <- lm(BASE~ESTU, data = ees)
summary(reg_simple)

# Regresión multiple
reg_multiple <- lm(BASE~SEXO+ESTU, data = ees)
summary(reg_multiple)
