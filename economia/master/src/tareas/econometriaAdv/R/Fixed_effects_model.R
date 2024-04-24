library(tidyverse)
library(tidytext)
library(readxl)
library(e1071)
library(ggplot2)
library(openxlsx)
library(lubridate)
library(plm)
library(robustbase)
library(DescTools)
library(dplyr)
library(stargazer)
library(lmtest)



setwd("C:\\Users\\endur\\Desktop\\progetti")

DB = read_excel("Final_dataset_wins.xlsx")

DB = DB %>% group_by(Name) %>% 
  mutate(LEVERAGE_NATLOG_LAG = dplyr::lag(LEVERAGE_NATLOG))

DB = DB %>% group_by(Name) %>% 
  mutate(MRKT_VALUE_BOOK_RATIO_LAG = dplyr::lag(`MRKT_VALUE_TO_BOOK`))

lower_threshold = 0.1
upper_threshold = 0.90

columns_to_wins = c("PRICE_OR_TRADE","TOTAL_ASSETS","TOT_NATLOG","TOT_GRWTRATE","MRKT_VALUE_TO_BOOK","MARKET_VALUE",
                    "MRKT_VALUE_NATLOG","MKT_VALUE_GRWTRATE","TOT_ASSETS_CMN_EQUITY_RATIO","LEVERAGE_NATLOG","LEVERAGE_GRWTRATE",
                    "LEVERAGE_NATLOG_LAG","MRKT_VALUE_BOOK_RATIO_LAG")

DB[columns_to_wins] = lapply(DB[columns_to_wins], function(x) Winsorize(x, probs = c(lower_threshold, upper_threshold), na.rm = TRUE))




MOD1 = plm(LEVERAGE_GRWTRATE ~ TOT_GRWTRATE + LEVERAGE_NATLOG_LAG, 
            data = DB, model = "within", effect = "time")
summary(MOD1)


MOD2 =plm(LEVERAGE_GRWTRATE ~ TOT_GRWTRATE + MRKT_VALUE_BOOK_RATIO_LAG + LEVERAGE_NATLOG_LAG, 
          data = DB, model = "within", effect = "time")
summary(MOD2)


MOD3 = plm(LEVERAGE_GRWTRATE ~ MKT_VALUE_GRWTRATE + LEVERAGE_NATLOG_LAG, 
           data = DB, model = "within", effect = "time")
summary(MOD3)

MOD4 = plm(LEVERAGE_GRWTRATE ~ MKT_VALUE_GRWTRATE + MRKT_VALUE_BOOK_RATIO_LAG +LEVERAGE_NATLOG_LAG, 
           data = DB, model = "within", effect = "time" )
summary(MOD4)

stargazer(MOD1,MOD2,MOD3,MOD4)


