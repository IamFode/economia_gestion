# En este programa voy a cruzar datos de la EPA y EES

s_epa <- read.csv2(file="../Dat/EPA/TasaParados.csv",encoding="UTF8")
ees <- read.csv2(file = "../Dat/EES/EES_2018_prep.csv",encoding =  "UTF8")

s_ees = aggregate(BASE~SEXO, data = ees, mean)  

merge(s_epa,s_ees)[c(1,3,4,5,6)]
