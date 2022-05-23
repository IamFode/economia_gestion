#librerias
library(ggplot2)
library(tibble)
library("readxl")

################################# DATA #########################################
data_efw <- read_excel("data/economic-freedom-of-the-world-2021.xlsx",
                  sheet = "EFW Data 2021 Report",
                  col_names = TRUE,
                  skip = 4)

pib <- read.csv("data/pib_pc_2017.csv",
                skip = 4,
                header = TRUE)

index_cap_hum <- read.csv("data/Indice_capital_humano.csv",
                          skip=4,
                          header = TRUE
                          )

idh = read.csv("data/idh.csv",header = TRUE)

gini = read.csv("data/gini.csv",header = TRUE)

epi = read.csv("data/epi.csv",header = TRUE)

poblacion = read.csv("data/poblacion.csv",header = TRUE)

desempleo = read.csv("data/desempleo.csv",header = TRUE)
#------------------------------Generico-----------------------------------------
ranking <- data_frame(data_efw$Year,
                      data_efw$ISO_Code_3,
                      data_efw$`Economic Freedom Summary Index`)
ranking <- subset(ranking,data_efw$Year==2017) 
ranking$`data_efw$Year` = NULL
colnames(ranking) <- c("code","indice")
################################################################################


################################################################################
################### LIBERTAD ECONÓMICA EN CONTEXTO #############################
################################################################################

########################## RELACIÓN CON EL PIB ################################# 
pib_2017 <- data_frame(pib$Country.Code,pib$X2017)
colnames(pib_2017) <- c("code","pib_pc")

relacion_pib <- merge(ranking,pib_2017,by="code")

plot=ggplot(relacion_pib, aes(pib_pc,indice)) + 
  geom_point(color="gray") + 
  geom_smooth(method = "lm", color="black",se=FALSE) +
  theme_classic() +
  labs(x="PIB per cápita",y="Indice de Libertdad Económica")
  
##################### RELACIÓN CON EL CAPITAL HUMANO ###########################
index_cap_hum_2017 <- data_frame(index_cap_hum$Country.Code,index_cap_hum$X2017)
colnames(index_cap_hum_2017) = c("code","indice_ch")

relacion_cap_hum = merge(ranking,index_cap_hum_2017,by="code")

plot1=ggplot(relacion_cap_hum, aes(indice_ch,indice)) + 
  geom_point(color="gray") + 
  geom_smooth(method = "lm", color="black",se=FALSE) +
  theme_classic() +
  labs(x="Índice de capital humano",y="Índice de libertad económica")

################# RELACIÓN CON EL INDICE DE DESARROLLO HUMANO ##################
idh_2017 = data_frame(idh$Country,idh$X2017)
edit_pais = gsub(" ","",idh_2017$`idh$Country`)
edit_idh = gsub(".*^","0.",idh_2017$`idh$X2017`)
edit_idh=as.numeric(edit_idh)
idh_2017 = data_frame(edit_pais,edit_idh)
colnames(idh_2017) = c("pais","idh")
pais = data_frame(pib$Country.Name,pib$Country.Code)
colnames(pais) = c("pais","code")
relacion_idh = merge(idh_2017,pais,by="pais")
relacion_idh$pais = NULL
relacion_idh = merge(ranking,relacion_idh,by="code")
head(relacion_idh)

plot2 = ggplot(relacion_idh, aes(idh,indice)) + 
  geom_point(color="gray") + 
  geom_smooth(method = "lm", color="black",se=FALSE) +
  theme_classic() +
  labs(x="IDH",y="Índice de libertad económica")


#################### RELACIÓN CON LA CALIDAD AMBIENTAL ########################
# indice de desempeño ambiental
epi_2020 = data_frame(epi$iso,epi$EPI.new)
colnames(epi_2020) = c("code","epi")
relacion_epi = merge(ranking,epi_2020,by="code")

plot3 = ggplot(relacion_epi, aes(epi,indice)) + 
  geom_point(color="gray") + 
  geom_smooth(method = "lm", color="black",se=FALSE) +
  theme_classic() +
  labs(x="Índice de desempeño ambiental",y="ïndice de libertad económica")

################RELACIÓN CON LA DISTRIBUCIÓN DE LA RENTA #######################
gini_2017 = data_frame(gini$Country.Code,gini$X2017)
colnames(gini_2017) = c("code","gini")
relacion_gini = merge(ranking,gini_2017,by="code")

plot4 = ggplot(relacion_gini, aes(gini,indice)) + 
  geom_point(color="gray") + 
  geom_smooth(method = "lm", color="black",se=FALSE) +
  theme_classic() +
  labs(x="Índice de GINI",y="")

############# RELACIÓN CON EL CRECIMIENTO DE LA POBLACIÓN ###################### 
poblacion_2017 = data_frame(poblacion$Country.Code,poblacion$X2017)
colnames(poblacion_2017) = c("code","poblacion")
relacion_pob = merge(ranking,poblacion_2017,by="code")

plot5 = ggplot(relacion_pob, aes(poblacion,indice)) + 
  geom_point(color="gray") + 
  geom_smooth(method = "lm", color="black",se=FALSE) +
  theme_classic() +
  labs(x="Crecimiento de la población (% Anual)",y="")

############## RELACIÓN TASA ANUAL MEDIA DE INVERSION ##########################
csh_i <- data.frame(data_pwt$countrycode,data_pwt$year,data_pwt$csh_i)
csh_i <- subset(csh_i, data_pwt$year>=año_inicio & data_pwt$year<=año_final)
colnames(csh_i) <- c("code","year","csh_i")
csh_i <- csh_i %>%
  group_by(code) %>%
  summarise(csh_i = sum(csh_i)/año_final)
relacion_inversion = merge(ranking,csh_i, by="code")
relacion_inversion = data_frame(relacion_inversion$code,
                                relacion_inversion$csh_i,
                                relacion_inversion$indice
                                )
colnames(relacion_inversion) = c("code","tasa_inversion","indice")

plot6 = ggplot(relacion_inversion, aes(tasa_inversion,indice)) + 
  geom_point(color="gray") + 
  geom_smooth(method = "lm", color="black",se=FALSE) +
  theme_classic() +
  labs(x="Tasa media de inversión",y="")

############################ Relacipon con Desempleo ###########################
desempleo_2017 = data_frame(desempleo$Country.Code,desempleo$X2017)
colnames(desempleo_2017) = c("code","desempleo")
relacion_desempleo = merge(ranking,desempleo_2017,by="code")

plot7 = ggplot(relacion_desempleo, aes(desempleo,indice)) + 
  geom_point(color="gray") + 
  geom_smooth(method = "lm", color="black",se=FALSE) +
  theme_classic() +
  labs(x="Desempleo total (% población activa)",y="")

################################################################################
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots == 1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
multiplot(plot,plot1,plot2,plot3,plot4,plot5,plot6,plot7,cols=2)
################################################################################

