## This file is used to process OptionMatrics data
## Date created: 20220331


rm(list = ls())
wd <- "C:/Dropbox/Courses/BUSI525/Replication/Data"
wd <- "D:/Dropbox/Courses/BUSI525/Replication/Data"
setwd(wd)
upd <- 20220413


library(tictoc)
library(lubridate)
library(tidyverse)



#### 0. CSV to Rdata ####

## OptionMetrics
# read_option <- read.csv("Source/OptionMetrics/OptionMetrics_1996_2010.csv",
#                         stringsAsFactors = FALSE)
# save(read_option,
#      file = "Source/OptionMetrics/OptionMetrics_1996_2010.Rdata")



#### 1. Load ####
## Trading days
load("Intermediate/CRSP/Date_Prices.Rdata")
rm(data_stock)


## Option
# 140 sec
tic()
load("Source/OptionMetrics/OptionMetrics_1996_2010.Rdata")
toc()



#### 2. Data Preparation ####
T2M_range <- c(1, 30) + 5

data_days_settle <- data_days %>% 
  select(date, id_tradingday) %>% 
  rename(maturity            = date,
         maturity_tradingday = id_tradingday)




#### 3. Option Tidy ####

# 1 min
tic()
data_option_raw <- read_option %>% 
  select(ticker, date, cp_flag, 
         exdate, strike_price, 
         volume, open_interest,
         symbol) %>% 
  
  filter(volume > 0) %>% 
  
  mutate(date         = ymd(date),
         maturity     = ymd(exdate),
         strike_price = strike_price / 1000) %>% 
  
  select(ticker, date, cp_flag, 
         maturity, strike_price, 
         volume, open_interest,
         symbol)
toc()

rm(read_option)


## 25 calls and puts
# 30 sec
tic()
data_option_subset <- data_option_raw %>% 
  
  left_join(data_days, by = c("date")) %>% 
  
  group_by(ticker, ith_week, cp_flag) %>% 
  summarise(ncontracts_traded = sum(volume, na.rm = TRUE)) %>% 
  ungroup()  %>% 
  
  filter(ncontracts_traded >= 25) %>% 
  
  group_by(ticker, ith_week) %>% 
  summarise(CallPut_traded = n()) %>% 
  ungroup() %>% 
  
  filter(CallPut_traded == 2)
toc()




## next 30 trading days
# 20 sec
tic()
data_option_volume <- data_option_raw %>% 
  
  left_join(data_days,        by = c("date")) %>% 
  left_join(data_days_settle, by = c("maturity")) %>% 
  
  mutate(T2M_trading  = maturity_tradingday - id_tradingday,) %>% 
  
  filter(T2M_trading >= T2M_range[1],
         T2M_trading <= T2M_range[2]) %>% 
  
  group_by(ticker, ith_week, cp_flag) %>% 
  summarise(volume_weekly = sum(volume, na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  pivot_wider(names_from = "cp_flag",
              values_from = "volume_weekly",
              values_fill = 0) %>% 
  
  rename(Call_volume_weekly = C,
         Put_volume_weekly  = P) %>% 
  mutate(Option_volume_weekly = Call_volume_weekly + Put_volume_weekly)

toc()




#### 4. Variable Construction ####

data_option <- data_option_subset %>% 
  select(ticker, ith_week) %>% 
  
  left_join(data_option_volume, by = c("ticker", "ith_week"))



#### 99. save ####

save(data_option,
     file = "Intermediate/OptionMetrics/OptionVolume_1996_2010.Rdata")
