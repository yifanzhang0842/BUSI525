## This file is used to process CRSP data
## Date created: 20220331


rm(list = ls())
wd <- "C:/Dropbox/Courses/BUSI525/Replication/Data"
# wd <- "D:/Dropbox/Courses/BUSI525/Replication/Data"
setwd(wd)
upd <- 20220419


library(tictoc)
library(lubridate)
library(tidyverse)



#### 0. Load ####

## Stock price
# read_stock <- read.csv("D:/Dropbox/CommonData/CRSP/CRSP_Daily_1995_2021.csv",
#                        stringsAsFactors = FALSE)
# save(read_stock,
#      file = "D:/Dropbox/CommonData/CRSP/CRSP_Daily_1995_2021.Rdata")
load("C:/Dropbox/CommonData/CRSP/CRSP_Daily_1995_2021.Rdata")
# load("D:/Dropbox/CommonData/CRSP/CRSP_Daily_1995_2021.Rdata")


#### 1. Dates ####
data_tradingday <- read_stock %>% 
  filter(is.element(TICKER, c("MSFT"))) %>% 
  select(date) %>% 
  arrange(date) %>% 
  unique() %>% 
  mutate(date          = ymd(date),
         id_tradingday = 1:n())

data_days <- data.frame(year = 1995:2021) %>% 
  crossing(data.frame(month = 1:12)) %>% 
  crossing(data.frame(day   = 1:31)) %>% 
  mutate(date        = year * 10000 + month * 100 + day) %>% 
  mutate(date        = ymd(date),
         ith_weekday = wday(date, week_start=1)) %>% 
  drop_na(date) %>% 
  
  mutate(id_calendarday = 1:n()) %>% 
  
  left_join(data_tradingday, by = "date") %>% 
  
  arrange(date) %>% 
  fill(id_tradingday) %>% 
  mutate(id_tradingday = replace_na(id_tradingday, 0)) %>% 
  
  mutate(signal_weekend = ith_weekday == 1,
         ith_week       = cumsum(signal_weekend)) %>% 
  select(date, ith_week, ith_weekday, 
         id_calendarday, id_tradingday)


rm(data_tradingday)



#### 2. Stock prices ####
# 30 sec
tic()
data_stock_filter <- read_stock %>% 
  rename(ticker = TICKER) %>% 
  
  mutate(year = floor(date/10000)) %>% 
  # filter(year <= 2010) %>% 

  # filter(is.element(ticker, vec_ticker)) %>% 
  filter(is.element(SHRCLS, c("", "A")),
         TRDSTAT == "A",
         is.element(SHRCD,  c(11)), # 11 for common stocks
         PRC > 1,
         is.na(DLSTCD) | DLSTCD == 100,
         # need exchange code
         ) %>%
  
  mutate(date = ymd(date))
toc()


rm(read_stock)



data_stock_raw <- data_stock_filter %>% 
  rename(permno = PERMNO) %>% 
  mutate(price  = as.numeric(PRC),
         return = as.numeric(RET),
         volume = as.numeric(VOL)) %>% 
  
  group_by(ticker) %>% 
  fill(price, return, volume) %>% 
  ungroup() %>% 
  drop_na(price) %>% 
  
  select(ticker, permno, date, 
         price, return, volume) %>% 
  
  left_join(data_days, by = "date") %>% 
  
  select(ticker, permno, date, 
         price, return, volume,
         ith_week, ith_weekday, 
         id_calendarday, id_tradingday)


rm(data_stock_filter)


# 400 sec
tic()
data_crsp_current <- data_stock_raw %>% 
  arrange(ticker, ith_week) %>% 
  
  group_by(ticker, ith_week) %>% 
  summarise(ndays_weekly = n(),
            stock_return_weekly = prod(1 + return, na.rm = FALSE) - 1,
            stock_volume_weekly = sum(volume) / 100,
            stock_amihud_weekly = 1 / ndays_weekly * sum(abs(return)) / sum(volume * price)) %>% 
  ungroup()
toc()


# 300 sec
tic()
data_crsp_future <- data_stock_raw %>% 
  mutate(return = replace_na(return, 0)) %>% 
  arrange(ticker, ith_week) %>% 
  
  group_by(ticker, ith_week) %>% 
  mutate(signal_firstday = id_tradingday == min(id_tradingday, na.rm = TRUE),
         signal_lastday  = id_tradingday == max(id_tradingday, na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  group_by(ticker) %>% 
  mutate(return_7days_raw = RcppRoll::roll_prod(1+return, n = 7, align = "left", fill = NA, na.rm = TRUE) - 1
         ) %>% 
  ungroup()
toc()


# 100 sec
tic()
data_crsp_next5day <- data_crsp_future %>% 
  mutate(return_next6days_total = (1 + return_7days_raw) / (1 + return) - 1,
         
         return_next6day_raw = replace(return_next6days_total, !signal_lastday,  NA),
         return_firstday_raw = replace(return,                 !signal_firstday, NA),
         ) %>% 
  
  group_by(ticker, ith_week) %>% 
  summarise(return_firstday = mean(return_firstday_raw, na.rm = TRUE),
            return_next6day = mean(return_next6day_raw, na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  mutate(return_firstday = replace(return_firstday, is.infinite(return_firstday), NA),
         return_next6day = replace(return_next6day, is.infinite(return_next6day), NA)) %>% 
  
  group_by(ticker) %>% 
  mutate(f1_return_firstday = lead(return_firstday, 1)) %>% 
  ungroup() %>% 
  
  mutate(return_next5days = (1 + return_next6day) / (1 + f1_return_firstday) - 1)
toc()



data_crsp_next <- data_crsp_next5day %>% 
  select(ticker, ith_week, return_next5days)



data_crsp <- data_crsp_current %>% 
  left_join(data_crsp_next, by = c("ticker", "ith_week"))



### 99. save ####
save(data_days, 
     data_crsp,
     
     file = "Intermediate/CRSP/CRSP_1995_2021.Rdata")
