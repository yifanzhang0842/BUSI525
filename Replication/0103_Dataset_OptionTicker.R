## This file is used to generate the available firm-week
## Date created: 20220418

rm(list = ls())
wd <- "C:/Dropbox/Courses/BUSI525/Replication/Data"
wd <- "D:/Dropbox/Courses/BUSI525/Replication/Data"
upd <- 20220418
setwd(wd)

library(tictoc)
library(tidyverse)



#### 0. Load ####


## CRSP
load("Intermediate/CRSP/CRSP_1995_2021.Rdata")

## Compustat
load("Intermediate/Compustat/Compustat_1995_2021.Rdata")



#### 1. Tickers ####

vec_ticker_crsp      <- sort(unique(data_stock$ticker))
vec_ticker_compustat <- sort(unique(data_compustat$ticker))


vec_ticker <- vec_ticker_crsp %>% 
  intersect(vec_ticker_compustat)


#### 99. Save ####
write.table(vec_ticker,
            file = "Intermediate/OptionMetrics/TickerList.txt",
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE)


