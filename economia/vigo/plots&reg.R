
#LIBRERIAS
library(readxl)

#DATA
GDPreal = read_excel("new/GDP-WB-IFS.xlsx", 
                         sheet = "GDPreal-IMF+WB")
DPIB = read_excel("new/Prices-WB-IFS.xlsx", 
                            sheet = "GDPDefl(2010)WB+IMF")
M1 <- read_excel("new/SeriesAnuales.xlsx", 
                    sheet = "M1")




