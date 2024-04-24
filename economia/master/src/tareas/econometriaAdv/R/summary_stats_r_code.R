library(tidyverse)
library(tidytext)
library(e1071)
library(knitr)
library(rmarkdown)
library(stargazer)
library(xtable)
library(ggplot2)
library(gridExtra)
library(grid)
library(tikzDevice)

setwd("C:\\Users\\endur\\Desktop\\laurea magistrale\\Financial\\First_Assignment\\Nuova cartella")

options(timeout = 600)

file = read.csv("DB_wins.csv", sep = ";")
file$PRICE_OR_TRADE = as.numeric(gsub(",", ".", file$PRICE_OR_TRADE))
file$TOTAL_ASSETS = as.numeric(gsub(",", ".", file$TOTAL_ASSETS))
file$TOT_NATLOG = as.numeric(gsub(",", ".", file$TOT_NATLOG))
file$TOT_GRWTRATE = as.numeric(gsub(",", ".", file$TOT_GRWTRATE))
file$MRKT_VALUE_TO_BOOK = as.numeric(gsub(",", ".", file$MRKT_VALUE_TO_BOOK))
file$MARKET_VALUE = as.numeric(gsub(",", ".", file$MARKET_VALUE))
file$MRKT_VALUE_NATLOG = as.numeric(gsub(",", ".", file$MRKT_VALUE_NATLOG))
file$MKT_VALUE_GRWTRATE = as.numeric(gsub(",", ".", file$MKT_VALUE_GRWTRATE))
file$TOT_ASSETS_CMN_EQUITY_RATIO = as.numeric(gsub(",", ".", file$TOT_ASSETS_CMN_EQUITY_RATIO))
file$LEVERAGE_NATLOG = as.numeric(gsub(",", ".", file$LEVERAGE_NATLOG))
file$LEVERAGE_GRWTRATE = as.numeric(gsub(",", ".", file$LEVERAGE_GRWTRATE ))

file$TOT_NATLOG = ifelse(is.na(file$TOT_NATLOG), 0, file$TOT_NATLOG)
file$TOT_GRWTRATE = ifelse(is.na(file$TOT_GRWTRATE), 0, file$TOT_GRWTRATE)
file$MRKT_VALUE_NATLOG = ifelse(is.na(file$MRKT_VALUE_NATLOG), 0, file$MRKT_VALUE_NATLOG)
file$ MKT_VALUE_GRWTRATE = ifelse(is.na(file$ MKT_VALUE_GRWTRATE), 0, file$ MKT_VALUE_GRWTRATE)
file$TOT_ASSETS_CMN_EQUITY_RATIO = ifelse(is.na(file$ TOT_ASSETS_CMN_EQUITY_RATIO), 0, file$TOT_ASSETS_CMN_EQUITY_RATIO)
file$ LEVERAGE_NATLOG = ifelse(is.na(file$LEVERAGE_NATLOG), 0, file$LEVERAGE_NATLOG)
file$ LEVERAGE_GRWTRATE= ifelse(is.na(file$  LEVERAGE_GRWTRATE), 0, file$ LEVERAGE_GRWTRATE)

summary_stats = data.frame(
  XS = c("PRICE_OR_TRADE", "TOTAL_ASSETS", "TOT_NATLOG", "TOT_GRWTRATE", "MRKT_VALUE_TO_BOOK", "MARKET_VALUE", "MRKT_VALUE_NATLOG", "MKT_VALUE_GRWTRATE", "TOT_ASSETS_CMN_EQUITY_RATIO", "LEVERAGE_NATLOG", "LEVERAGE_GRWTRATE"),
  Minimum = c(min(file$PRICE_OR_TRADE), min(file$TOTAL_ASSETS), min(file$TOT_NATLOG), min(file$TOT_GRWTRATE), min(file$MRKT_VALUE_TO_BOOK), min(file$MARKET_VALUE), min(file$MRKT_VALUE_NATLOG), min(file$MKT_VALUE_GRWTRATE), 
              min(file$TOT_ASSETS_CMN_EQUITY_RATIO), min(file$LEVERAGE_NATLOG), min(file$LEVERAGE_GRWTRATE)),
  Mean = c(mean(file$PRICE_OR_TRADE), mean(file$TOTAL_ASSETS), mean(file$TOT_NATLOG), mean(file$TOT_GRWTRATE), mean(file$MRKT_VALUE_TO_BOOK),mean(file$MARKET_VALUE), mean(file$MRKT_VALUE_NATLOG), mean(file$MKT_VALUE_GRWTRATE),
           mean(file$TOT_ASSETS_CMN_EQUITY_RATIO), mean(file$LEVERAGE_NATLOG), mean(file$LEVERAGE_GRWTRATE)),
  Median = c(median(file$PRICE_OR_TRADE), median(file$TOTAL_ASSETS), median(file$TOT_NATLOG), median(file$TOT_GRWTRATE), median(file$MRKT_VALUE_TO_BOOK), median(file$MARKET_VALUE), median(file$MRKT_VALUE_NATLOG), median(file$MKT_VALUE_GRWTRATE),
             median(file$TOT_ASSETS_CMN_EQUITY_RATIO), median(file$LEVERAGE_NATLOG), median(file$LEVERAGE_GRWTRATE)),
  Maximum = c(max(file$PRICE_OR_TRADE), max(file$TOTAL_ASSETS), max(file$TOT_NATLOG), max(file$TOT_GRWTRATE), max(file$MRKT_VALUE_TO_BOOK), max(file$MARKET_VALUE), max(file$MRKT_VALUE_NATLOG), max(file$MKT_VALUE_GRWTRATE),
              max(file$TOT_ASSETS_CMN_EQUITY_RATIO), max(file$LEVERAGE_NATLOG), max(file$LEVERAGE_GRWTRATE)),
  Standard_Deviation = c(sd(file$PRICE_OR_TRADE), sd(file$TOTAL_ASSETS), sd(file$TOT_NATLOG), sd(file$TOT_GRWTRATE), sd(file$MRKT_VALUE_TO_BOOK), sd(file$MARKET_VALUE), sd(file$MRKT_VALUE_NATLOG), sd(file$MKT_VALUE_GRWTRATE),
                         sd(file$TOT_ASSETS_CMN_EQUITY_RATIO), sd(file$LEVERAGE_NATLOG), sd(file$LEVERAGE_GRWTRATE)),
  Skewness = c(skewness(file$PRICE_OR_TRADE), skewness(file$TOTAL_ASSETS), skewness(file$TOT_NATLOG), skewness(file$TOT_GRWTRATE), skewness(file$MRKT_VALUE_TO_BOOK), skewness(file$MARKET_VALUE), skewness(file$MRKT_VALUE_NATLOG),
               skewness(file$MKT_VALUE_GRWTRATE), skewness(file$TOT_ASSETS_CMN_EQUITY_RATIO), skewness(file$LEVERAGE_NATLOG), skewness(file$LEVERAGE_GRWTRATE)),
  kurtosis = c(kurtosis(file$PRICE_OR_TRADE), kurtosis(file$TOTAL_ASSETS), kurtosis(file$TOT_NATLOG), kurtosis(file$TOT_GRWTRATE), kurtosis(file$MRKT_VALUE_TO_BOOK), kurtosis(file$MARKET_VALUE), kurtosis(file$MRKT_VALUE_NATLOG),
               kurtosis(file$MKT_VALUE_GRWTRATE), kurtosis(file$TOT_ASSETS_CMN_EQUITY_RATIO), kurtosis(file$LEVERAGE_NATLOG), kurtosis(file$LEVERAGE_GRWTRATE)))

summary_stats$Mean = as.numeric(summary_stats$Mean)
summary_stats$Median = as.numeric(summary_stats$Median)
summary_stats$Maximum = as.numeric(summary_stats$Maximum)
summary_stats$Standard_Deviation = as.numeric(summary_stats$Standard_Deviation)
summary_stats$Skewness = as.numeric(summary_stats$Skewness)
summary_stats$kurtosis = as.numeric(summary_stats$kurtosis)

summary_stats$Mean = round(summary_stats$Mean, 2)
summary_stats$Median = round(summary_stats$Median, 2)
summary_stats$Maximum = round(summary_stats$Maximum, 2)
summary_stats$Standard_Deviation = round(summary_stats$Standard_Deviation, 2)
summary_stats$ Skewness = round(summary_stats$ Skewness, 2)
summary_stats$ kurtosis = round(summary_stats$ kurtosis, 2)

summary_stats = format(summary_stats, scientific = FALSE)

summary_stats

table1 = xtable(summary_stats, caption = "Summary statistcs complete dataset")
print(table1, include.rownames = FALSE)


banks = file[file$FinancialEntity == "banks", ]
finance_service = file[file$FinancialEntity == "finance service", ]
real_estate = file[file$FinancialEntity == "real estate", ]

#######BANKS

summary_stats_banks <- data.frame(
  XS = c("PRICE_OR_TRADE", "TOTAL_ASSETS", "TOT_NATLOG", "TOT_GRWTRATE", "MRKT_VALUE_TO_BOOK", "MARKET_VALUE", "MRKT_VALUE_NATLOG", "MKT_VALUE_GRWTRATE", "TOT_ASSETS_CMN_EQUITY_RATIO", "LEVERAGE_NATLOG", "LEVERAGE_GRWTRATE"),  # Sostituire con le colonne effettive
  Minimum = c(min(banks$PRICE_OR_TRADE), min(banks$TOTAL_ASSETS), min(banks$TOT_NATLOG), min(banks$TOT_GRWTRATE), min(banks$MRKT_VALUE_TO_BOOK), min(banks$MARKET_VALUE), min(banks$MRKT_VALUE_NATLOG), min(banks$MKT_VALUE_GRWTRATE), 
              min(banks$TOT_ASSETS_CMN_EQUITY_RATIO), min(banks$LEVERAGE_NATLOG), min(banks$LEVERAGE_GRWTRATE)),
  Mean = c(mean(banks$PRICE_OR_TRADE), mean(banks$TOTAL_ASSETS), mean(banks$TOT_NATLOG), mean(banks$TOT_GRWTRATE), mean(banks$MRKT_VALUE_TO_BOOK), mean(banks$MARKET_VALUE), mean(banks$MRKT_VALUE_NATLOG), mean(banks$MKT_VALUE_GRWTRATE),
           mean(banks$TOT_ASSETS_CMN_EQUITY_RATIO), mean(banks$LEVERAGE_NATLOG), mean(banks$LEVERAGE_GRWTRATE)),
  Median = c(median(banks$PRICE_OR_TRADE), median(banks$TOTAL_ASSETS), median(banks$TOT_NATLOG), median(banks$TOT_GRWTRATE), median(banks$MRKT_VALUE_TO_BOOK), median(banks$MARKET_VALUE), median(banks$MRKT_VALUE_NATLOG), median(banks$MKT_VALUE_GRWTRATE),
             median(banks$TOT_ASSETS_CMN_EQUITY_RATIO), median(banks$LEVERAGE_NATLOG), median(banks$LEVERAGE_GRWTRATE)),
  Maximum = c(max(banks$PRICE_OR_TRADE), max(banks$TOTAL_ASSETS), max(banks$TOT_NATLOG), max(banks$TOT_GRWTRATE), max(banks$MRKT_VALUE_TO_BOOK), max(banks$MARKET_VALUE), max(banks$MRKT_VALUE_NATLOG), max(banks$MKT_VALUE_GRWTRATE),
              max(banks$TOT_ASSETS_CMN_EQUITY_RATIO), max(banks$LEVERAGE_NATLOG), max(banks$LEVERAGE_GRWTRATE)),
  Standard_Deviation = c(sd(banks$PRICE_OR_TRADE), sd(banks$TOTAL_ASSETS), sd(banks$TOT_NATLOG), sd(banks$TOT_GRWTRATE), sd(banks$MRKT_VALUE_TO_BOOK), sd(banks$MARKET_VALUE), sd(banks$MRKT_VALUE_NATLOG), sd(banks$MKT_VALUE_GRWTRATE),
                         sd(banks$TOT_ASSETS_CMN_EQUITY_RATIO), sd(banks$LEVERAGE_NATLOG), sd(banks$LEVERAGE_GRWTRATE)),
  Skewness = c(skewness(banks$PRICE_OR_TRADE), skewness(banks$TOTAL_ASSETS), skewness(banks$TOT_NATLOG), skewness(banks$TOT_GRWTRATE), skewness(banks$MRKT_VALUE_TO_BOOK), skewness(banks$MARKET_VALUE), skewness(banks$MRKT_VALUE_NATLOG),
               skewness(banks$MKT_VALUE_GRWTRATE), skewness(banks$TOT_ASSETS_CMN_EQUITY_RATIO), skewness(banks$LEVERAGE_NATLOG), skewness(banks$LEVERAGE_GRWTRATE)),
  kurtosis = c(kurtosis(banks$PRICE_OR_TRADE), kurtosis(banks$TOTAL_ASSETS), kurtosis(banks$TOT_NATLOG), kurtosis(banks$TOT_GRWTRATE), kurtosis(banks$MRKT_VALUE_TO_BOOK), kurtosis(banks$MARKET_VALUE), kurtosis(banks$MRKT_VALUE_NATLOG),
               kurtosis(banks$MKT_VALUE_GRWTRATE), kurtosis(banks$TOT_ASSETS_CMN_EQUITY_RATIO), kurtosis(banks$LEVERAGE_NATLOG), kurtosis(banks$LEVERAGE_GRWTRATE))
)

summary_stats_banks$Mean = as.numeric(summary_stats_banks$Mean)
summary_stats_banks$Median = as.numeric(summary_stats_banks$Median)
summary_stats_banks$Maximum = as.numeric(summary_stats_banks$Maximum)
summary_stats_banks$Standard_Deviation = as.numeric(summary_stats_banks$Standard_Deviation)
summary_stats_banks$Skewness = as.numeric(summary_stats_banks$Skewness)
summary_stats_banks$kurtosis = as.numeric(summary_stats_banks$kurtosis)

summary_stats_banks$Mean = round(summary_stats_banks$Mean, 2)
summary_stats_banks$Median = round(summary_stats_banks$Median, 2)
summary_stats_banks$Maximum = round(summary_stats_banks$Maximum, 2)
summary_stats_banks$Standard_Deviation = round(summary_stats_banks$Standard_Deviation, 2)
summary_stats_banks$ Skewness = round(summary_stats_banks$ Skewness, 2)
summary_stats_banks$ kurtosis = round(summary_stats_banks$ kurtosis, 2)

summary_stats_banks = format(summary_stats_banks, scientific = FALSE)

summary_stats_banks

table2 = xtable(summary_stats_banks, caption = "Summary statistics for banks")

print(table2, include.colnames = FALSE)



#######FINANCE SERVICE

summary_stats_fs = data.frame(
  XS = c("PRICE_OR_TRADE", "TOTAL_ASSETS", "TOT_NATLOG", "TOT_GRWTRATE", "MRKT_VALUE_TO_BOOK", "MARKET_VALUE", "MRKT_VALUE_NATLOG", "MKT_VALUE_GRWTRATE", "TOT_ASSETS_CMN_EQUITY_RATIO", "LEVERAGE_NATLOG", "LEVERAGE_GRWTRATE"),
  Minimum = c(min(finance_service$PRICE_OR_TRADE), min(finance_service$TOTAL_ASSETS), min(finance_service$TOT_NATLOG), min(finance_service$TOT_GRWTRATE), min(finance_service$MRKT_VALUE_TO_BOOK), min(finance_service$MARKET_VALUE), min(finance_service$MRKT_VALUE_NATLOG), min(finance_service$MKT_VALUE_GRWTRATE), 
              min(finance_service$TOT_ASSETS_CMN_EQUITY_RATIO), min(finance_service$LEVERAGE_NATLOG), min(finance_service$LEVERAGE_GRWTRATE)),
  Mean = c(mean(finance_service$PRICE_OR_TRADE), mean(finance_service$TOTAL_ASSETS), mean(finance_service$TOT_NATLOG), mean(finance_service$TOT_GRWTRATE), mean(finance_service$MRKT_VALUE_TO_BOOK), mean(finance_service$MARKET_VALUE), mean(finance_service$MRKT_VALUE_NATLOG), mean(finance_service$MKT_VALUE_GRWTRATE),
           mean(finance_service$TOT_ASSETS_CMN_EQUITY_RATIO), mean(finance_service$LEVERAGE_NATLOG), mean(finance_service$LEVERAGE_GRWTRATE)),
  Median = c(median(finance_service$PRICE_OR_TRADE), median(finance_service$TOTAL_ASSETS), median(finance_service$TOT_NATLOG), median(finance_service$TOT_GRWTRATE), median(finance_service$MRKT_VALUE_TO_BOOK), median(finance_service$MARKET_VALUE), median(finance_service$MRKT_VALUE_NATLOG), median(finance_service$MKT_VALUE_GRWTRATE),
             median(finance_service$TOT_ASSETS_CMN_EQUITY_RATIO), median(finance_service$LEVERAGE_NATLOG), median(finance_service$LEVERAGE_GRWTRATE)),
  Maximum = c(max(finance_service$PRICE_OR_TRADE), max(finance_service$TOTAL_ASSETS), max(finance_service$TOT_NATLOG), max(finance_service$TOT_GRWTRATE), max(finance_service$MRKT_VALUE_TO_BOOK), max(finance_service$MARKET_VALUE), max(finance_service$MRKT_VALUE_NATLOG), max(finance_service$MKT_VALUE_GRWTRATE),
              max(finance_service$TOT_ASSETS_CMN_EQUITY_RATIO), max(finance_service$LEVERAGE_NATLOG), max(finance_service$LEVERAGE_GRWTRATE)),
  Standard_Deviation = c(sd(finance_service$PRICE_OR_TRADE), sd(finance_service$TOTAL_ASSETS), sd(finance_service$TOT_NATLOG), sd(finance_service$TOT_GRWTRATE), sd(finance_service$MRKT_VALUE_TO_BOOK), sd(finance_service$MARKET_VALUE), sd(finance_service$MRKT_VALUE_NATLOG), sd(finance_service$MKT_VALUE_GRWTRATE),
                         sd(finance_service$TOT_ASSETS_CMN_EQUITY_RATIO), sd(finance_service$LEVERAGE_NATLOG), sd(finance_service$LEVERAGE_GRWTRATE)),
  Skewness = c(skewness(finance_service$PRICE_OR_TRADE), skewness(finance_service$TOTAL_ASSETS), skewness(finance_service$TOT_NATLOG), skewness(finance_service$TOT_GRWTRATE), skewness(finance_service$MRKT_VALUE_TO_BOOK), skewness(finance_service$MARKET_VALUE), skewness(finance_service$MRKT_VALUE_NATLOG),
               skewness(finance_service$MKT_VALUE_GRWTRATE), skewness(finance_service$TOT_ASSETS_CMN_EQUITY_RATIO), skewness(finance_service$LEVERAGE_NATLOG), skewness(finance_service$LEVERAGE_GRWTRATE)),
  kurtosis = c(kurtosis(finance_service$PRICE_OR_TRADE), kurtosis(finance_service$TOTAL_ASSETS), kurtosis(finance_service$TOT_NATLOG), kurtosis(finance_service$TOT_GRWTRATE), kurtosis(finance_service$MRKT_VALUE_TO_BOOK), kurtosis(finance_service$MARKET_VALUE), kurtosis(finance_service$MRKT_VALUE_NATLOG),
               kurtosis(finance_service$MKT_VALUE_GRWTRATE), kurtosis(finance_service$TOT_ASSETS_CMN_EQUITY_RATIO), kurtosis(finance_service$LEVERAGE_NATLOG), kurtosis(finance_service$LEVERAGE_GRWTRATE)))


summary_stats_fs$Mean = as.numeric(summary_stats_fs$Mean)
summary_stats_fs$Median = as.numeric(summary_stats_fs$Median)
summary_stats_fs$Maximum = as.numeric(summary_stats_fs$Maximum)
summary_stats_fs$Standard_Deviation = as.numeric(summary_stats_fs$Standard_Deviation)
summary_stats_fs$Skewness = as.numeric(summary_stats_fs$Skewness)
summary_stats_fs$kurtosis = as.numeric(summary_stats_fs$kurtosis)

summary_stats_fs$Mean = round(summary_stats_fs$Mean, 2)
summary_stats_fs$Median = round(summary_stats_fs$Median, 2)
summary_stats_fs$Maximum = round(summary_stats_fs$Maximum, 2)
summary_stats_fs$Standard_Deviation = round(summary_stats_fs$Standard_Deviation, 2)
summary_stats_fs$ Skewness = round(summary_stats_fs$ Skewness, 2)
summary_stats_fs$ kurtosis = round(summary_stats_fs$ kurtosis, 2)

summary_stats_fs = format(summary_stats_fs, scientific = FALSE)

summary_stats_fs

table3 = xtable(summary_stats_fs, caption = "Summary statistics for Finance Services")

print(table3, include.rownames = FALSE)

summary_stats_re = data.frame(
  XS = c("PRICE_OR_TRADE", "TOTAL_ASSETS", "TOT_NATLOG", "TOT_GRWTRATE", "MRKT_VALUE_TO_BOOK", "MARKET_VALUE", "MRKT_VALUE_NATLOG", "MKT_VALUE_GRWTRATE", "TOT_ASSETS_CMN_EQUITY_RATIO", "LEVERAGE_NATLOG", "LEVERAGE_GRWTRATE"),
  Minimum = c(min(real_estate$PRICE_OR_TRADE), min(real_estate$TOTAL_ASSETS), min(real_estate$TOT_NATLOG), min(real_estate$TOT_GRWTRATE), min(real_estate$MRKT_VALUE_TO_BOOK), min(real_estate$MARKET_VALUE), min(real_estate$MRKT_VALUE_NATLOG), min(real_estate$MKT_VALUE_GRWTRATE), 
              min(real_estate$TOT_ASSETS_CMN_EQUITY_RATIO), min(real_estate$LEVERAGE_NATLOG), min(real_estate$LEVERAGE_GRWTRATE)),
  Mean = c(mean(real_estate$PRICE_OR_TRADE), mean(real_estate$TOTAL_ASSETS), mean(real_estate$TOT_NATLOG), mean(real_estate$TOT_GRWTRATE), mean(real_estate$MRKT_VALUE_TO_BOOK), mean(real_estate$MARKET_VALUE), mean(real_estate$MRKT_VALUE_NATLOG), mean(real_estate$MKT_VALUE_GRWTRATE),
           mean(real_estate$TOT_ASSETS_CMN_EQUITY_RATIO), mean(real_estate$LEVERAGE_NATLOG), mean(real_estate$LEVERAGE_GRWTRATE)),
  Median = c(median(real_estate$PRICE_OR_TRADE), median(real_estate$TOTAL_ASSETS), median(real_estate$TOT_NATLOG), median(real_estate$TOT_GRWTRATE), median(real_estate$MRKT_VALUE_TO_BOOK), median(real_estate$MARKET_VALUE), median(real_estate$MRKT_VALUE_NATLOG), median(real_estate$MKT_VALUE_GRWTRATE),
             median(real_estate$TOT_ASSETS_CMN_EQUITY_RATIO), median(real_estate$LEVERAGE_NATLOG), median(real_estate$LEVERAGE_GRWTRATE)),
  Maximum = c(max(real_estate$PRICE_OR_TRADE), max(real_estate$TOTAL_ASSETS), max(real_estate$TOT_NATLOG), max(real_estate$TOT_GRWTRATE), max(real_estate$MRKT_VALUE_TO_BOOK), max(real_estate$MARKET_VALUE), max(real_estate$MRKT_VALUE_NATLOG), max(real_estate$MKT_VALUE_GRWTRATE),
              max(real_estate$TOT_ASSETS_CMN_EQUITY_RATIO), max(real_estate$LEVERAGE_NATLOG), max(real_estate$LEVERAGE_GRWTRATE)),
  Standard_Deviation = c(sd(real_estate$PRICE_OR_TRADE), sd(real_estate$TOTAL_ASSETS), sd(real_estate$TOT_NATLOG), sd(real_estate$TOT_GRWTRATE), sd(real_estate$MRKT_VALUE_TO_BOOK), sd(real_estate$MARKET_VALUE), sd(real_estate$MRKT_VALUE_NATLOG), sd(real_estate$MKT_VALUE_GRWTRATE),
                         sd(real_estate$TOT_ASSETS_CMN_EQUITY_RATIO), sd(real_estate$LEVERAGE_NATLOG), sd(real_estate$LEVERAGE_GRWTRATE)),
  Skewness = c(skewness(real_estate$PRICE_OR_TRADE), skewness(real_estate$TOTAL_ASSETS), skewness(real_estate$TOT_NATLOG), skewness(real_estate$TOT_GRWTRATE), skewness(real_estate$MRKT_VALUE_TO_BOOK), skewness(real_estate$MARKET_VALUE), skewness(real_estate$MRKT_VALUE_NATLOG),
               skewness(real_estate$MKT_VALUE_GRWTRATE), skewness(real_estate$TOT_ASSETS_CMN_EQUITY_RATIO), skewness(real_estate$LEVERAGE_NATLOG), skewness(real_estate$LEVERAGE_GRWTRATE)),
  kurtosis = c(kurtosis(real_estate$PRICE_OR_TRADE), kurtosis(real_estate$TOTAL_ASSETS), kurtosis(real_estate$TOT_NATLOG), kurtosis(real_estate$TOT_GRWTRATE), kurtosis(real_estate$MRKT_VALUE_TO_BOOK), kurtosis(real_estate$MARKET_VALUE), kurtosis(real_estate$MRKT_VALUE_NATLOG),
               kurtosis(real_estate$MKT_VALUE_GRWTRATE), kurtosis(real_estate$TOT_ASSETS_CMN_EQUITY_RATIO), kurtosis(real_estate$LEVERAGE_NATLOG), kurtosis(real_estate$LEVERAGE_GRWTRATE)))

summary_stats_re = data.frame(
  XS = c("PRICE_OR_TRADE", "TOTAL_ASSETS", "TOT_NATLOG", "TOT_GRWTRATE", "MRKT_VALUE_TO_BOOK", "MARKET_VALUE", "MRKT_VALUE_NATLOG", "MKT_VALUE_GRWTRATE", "TOT_ASSETS_CMN_EQUITY_RATIO", "LEVERAGE_NATLOG", "LEVERAGE_GRWTRATE"),
  Minimum = c(min(real_estate$PRICE_OR_TRADE), min(real_estate$TOTAL_ASSETS), min(real_estate$TOT_NATLOG), min(real_estate$TOT_GRWTRATE), min(real_estate$MRKT_VALUE_TO_BOOK), min(real_estate$MARKET_VALUE), min(real_estate$MRKT_VALUE_NATLOG), min(real_estate$MKT_VALUE_GRWTRATE), 
              min(real_estate$TOT_ASSETS_CMN_EQUITY_RATIO), min(real_estate$LEVERAGE_NATLOG), min(real_estate$LEVERAGE_GRWTRATE)),
  Mean = c(mean(real_estate$PRICE_OR_TRADE), mean(real_estate$TOTAL_ASSETS), mean(real_estate$TOT_NATLOG), mean(real_estate$TOT_GRWTRATE), mean(real_estate$MRKT_VALUE_TO_BOOK), mean(real_estate$MARKET_VALUE), mean(real_estate$MRKT_VALUE_NATLOG), mean(real_estate$MKT_VALUE_GRWTRATE),
           mean(real_estate$TOT_ASSETS_CMN_EQUITY_RATIO), mean(real_estate$LEVERAGE_NATLOG), mean(real_estate$LEVERAGE_GRWTRATE)),
  Median = c(median(real_estate$PRICE_OR_TRADE), median(real_estate$TOTAL_ASSETS), median(real_estate$TOT_NATLOG), median(real_estate$TOT_GRWTRATE), median(real_estate$MRKT_VALUE_TO_BOOK), median(real_estate$MARKET_VALUE), median(real_estate$MRKT_VALUE_NATLOG), median(real_estate$MKT_VALUE_GRWTRATE),
             median(real_estate$TOT_ASSETS_CMN_EQUITY_RATIO), median(real_estate$LEVERAGE_NATLOG), median(real_estate$LEVERAGE_GRWTRATE)),
  Maximum = c(max(real_estate$PRICE_OR_TRADE), max(real_estate$TOTAL_ASSETS), max(real_estate$TOT_NATLOG), max(real_estate$TOT_GRWTRATE), max(real_estate$MRKT_VALUE_TO_BOOK), max(real_estate$MARKET_VALUE), max(real_estate$MRKT_VALUE_NATLOG), max(real_estate$MKT_VALUE_GRWTRATE),
              max(real_estate$TOT_ASSETS_CMN_EQUITY_RATIO), max(real_estate$LEVERAGE_NATLOG), max(real_estate$LEVERAGE_GRWTRATE)),
  Standard_Deviation = c(sd(real_estate$PRICE_OR_TRADE), sd(real_estate$TOTAL_ASSETS), sd(real_estate$TOT_NATLOG), sd(real_estate$TOT_GRWTRATE), sd(real_estate$MRKT_VALUE_TO_BOOK), sd(real_estate$MARKET_VALUE), sd(real_estate$MRKT_VALUE_NATLOG), sd(real_estate$MKT_VALUE_GRWTRATE),
                         sd(real_estate$TOT_ASSETS_CMN_EQUITY_RATIO), sd(real_estate$LEVERAGE_NATLOG), sd(real_estate$LEVERAGE_GRWTRATE)),
  Skewness = c(skewness(real_estate$PRICE_OR_TRADE), skewness(real_estate$TOTAL_ASSETS), skewness(real_estate$TOT_NATLOG), skewness(real_estate$TOT_GRWTRATE), skewness(real_estate$MRKT_VALUE_TO_BOOK), skewness(real_estate$MARKET_VALUE), skewness(real_estate$MRKT_VALUE_NATLOG),
               skewness(real_estate$MKT_VALUE_GRWTRATE), skewness(real_estate$TOT_ASSETS_CMN_EQUITY_RATIO), skewness(real_estate$LEVERAGE_NATLOG), skewness(real_estate$LEVERAGE_GRWTRATE)),
  kurtosis = c(kurtosis(real_estate$PRICE_OR_TRADE), kurtosis(real_estate$TOTAL_ASSETS), kurtosis(real_estate$TOT_NATLOG), kurtosis(real_estate$TOT_GRWTRATE), kurtosis(real_estate$MRKT_VALUE_TO_BOOK), kurtosis(real_estate$MARKET_VALUE), kurtosis(real_estate$MRKT_VALUE_NATLOG),
               kurtosis(real_estate$MKT_VALUE_GRWTRATE), kurtosis(real_estate$TOT_ASSETS_CMN_EQUITY_RATIO), kurtosis(real_estate$LEVERAGE_NATLOG), kurtosis(real_estate$LEVERAGE_GRWTRATE)))
summary_stats_re$Mean = as.numeric(summary_stats_re$Mean)
summary_stats_re$Median = as.numeric(summary_stats_re$Median)
summary_stats_re$Maximum = as.numeric(summary_stats_re$Maximum)
summary_stats_re$Standard_Deviation = as.numeric(summary_stats_re$Standard_Deviation)
summary_stats_re$Skewness = as.numeric(summary_stats_re$Skewness)
summary_stats_re$kurtosis = as.numeric(summary_stats_re$kurtosis)

summary_stats_re$Mean = round(summary_stats_re$Mean, 2)
summary_stats_re$Median = round(summary_stats_re$Median, 2)
summary_stats_re$Maximum = round(summary_stats_re$Maximum, 2)
summary_stats_re$Standard_Deviation = round(summary_stats_re$Standard_Deviation, 2)
summary_stats_re$ Skewness = round(summary_stats_re$ Skewness, 2)
summary_stats_re$ kurtosis = round(summary_stats_re$ kurtosis, 2)

summary_stats_re = format(summary_stats_re, scientific = FALSE)

summary_stats_re

table4 = xtable(summary_stats_re, caption = "Summary statistics for Real Estate Industries")

print(table4, include.rownames = FALSE)



Banks_leverage = banks %>% 
  ggplot(aes(TOT_ASSETS_CMN_EQUITY_RATIO))+
  geom_boxplot(color = "skyblue")+
  ggtitle("Bank")+
  theme(axis.title.x = element_blank())

Finance_service_leverage = finance_service %>% 
  ggplot(aes(TOT_ASSETS_CMN_EQUITY_RATIO))+
  geom_boxplot(color = "skyblue")+
  ggtitle("financial services")+
  theme(axis.title.x = element_blank())

Real_estate_leverage = real_estate %>% 
  ggplot(aes(TOT_ASSETS_CMN_EQUITY_RATIO))+
  geom_boxplot(color = "skyblue")+
  ggtitle("real estate")+
  theme(axis.title.x = element_blank())



plot1 <- grid.arrange(Banks_leverage, Finance_service_leverage, Real_estate_leverage, ncol = 3,
                      top = textGrob("Leverage distribution", gp = gpar(fontface = "bold", fontsize = 16)))

Banks_total_assets = banks %>% 
  ggplot(aes(TOTAL_ASSETS))+
  geom_boxplot(color = "royalblue")+
  ggtitle("banks")+
  theme(axis.title.x = element_blank())

Finance_service_total_assets = finance_service %>% 
  ggplot(aes(TOTAL_ASSETS))+
  geom_boxplot(color = "royalblue")+
  ggtitle("financial services")+
  theme(axis.title.x = element_blank())

Real_estate_total_assets = real_estate %>% 
  ggplot(aes(TOTAL_ASSETS))+
  geom_boxplot(color = "royalblue")+
  ggtitle("real estate")+
  theme(axis.title.x = element_blank())


plot2 = grid.arrange(Banks_total_assets, Finance_service_total_assets, Real_estate_total_assets, ncol = 3,
                      top = textGrob("Total Asset distribution", gp = gpar(fontface = "bold", fontsize = 16)))


Banks_market_to_value_book = banks %>% 
  ggplot(aes(MRKT_VALUE_TO_BOOK))+
  geom_boxplot(color = "blue")+
  ggtitle("banks")+
  theme(axis.title.x = element_blank())

Finance_market_to_value_book = finance_service %>% 
  ggplot(aes(MRKT_VALUE_TO_BOOK))+
  geom_boxplot(color = "blue")+
  ggtitle("financial services")+
  theme(axis.title.x = element_blank())

Real_estate_market_to_value_book = real_estate %>% 
  ggplot(aes(MRKT_VALUE_TO_BOOK))+
  geom_boxplot(color = "blue")+
  ggtitle("real estate")+
  theme(axis.title.x = element_blank())


plot3 = grid.arrange(Banks_market_to_value_book, Finance_market_to_value_book, Real_estate_market_to_value_book, ncol = 3,
                      top = textGrob("Market to value book distribution", gp = gpar(fontface = "bold", fontsize = 16)))


Banks_market_value = banks %>% 
  ggplot(aes(MARKET_VALUE))+
  geom_boxplot(color = "navyblue")+
  ggtitle("banks")+
  theme(axis.title.x = element_blank())

Finance_market_value = finance_service %>% 
  ggplot(aes(MARKET_VALUE))+
  geom_boxplot(color = "navyblue")+
  ggtitle("financial services")+
  theme(axis.title.x = element_blank())

Real_estate_market_value = real_estate %>% 
  ggplot(aes(MARKET_VALUE))+
  geom_boxplot(color = "navyblue")+
  ggtitle("real estate")+
  theme(axis.title.x = element_blank())

plot4 = grid.arrange(Banks_market_value, Finance_market_value, Real_estate_market_value, ncol = 3,
                     top = textGrob("Market value distribution", gp = gpar(fontface = "bold", fontsize = 16)))
           

final_plot = grid.arrange(plot1,plot2, plot3, plot4)

