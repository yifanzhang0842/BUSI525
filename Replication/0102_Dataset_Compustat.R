## This file is used to process Compustat data
## Date created: 20220331


rm(list = ls())
wd <- "C:/Dropbox/Courses/BUSI525/Replication/Data"
# wd <- "D:/Dropbox/Courses/BUSI525/Replication/Data"
setwd(wd)
upd <- 20220418


library(tictoc)
library(lubridate)
library(tidyverse)



#### 0. Load ####

## Compustat
read_firmq <- read.csv("Source/Compustat/Compustat_Quarterly_1995_2021.csv",
                       stringsAsFactors = FALSE)

## Date
load("Intermediate/CRSP/CRSP_1995_2021.Rdata")


#### 1. Tidy ####


data_compustat_raw <- read_firmq %>% 
  select(datadate, tic, gvkey, 
         atq, ltq, cshoq, prccq) %>% 
  rename(ticker = tic) %>% 
  
  mutate(date = ymd(datadate)) %>% 
  drop_na(date) %>% 
  
  left_join(data_days, by = "date") %>% 
  
  select(ticker, date, gvkey,
         atq, ltq, cshoq, prccq,
         
         ith_week, ith_weekday, 
         id_calendarday, id_tradingday)


data_compustat_base <- data_compustat_raw %>% 
  mutate(Book   = atq - ltq,
         Market = cshoq * prccq,
         
         B2M = Book / Market) %>% 
  
  rename(size = Market) %>% 
  select(ticker, ith_week,
         B2M, size)


data_compustat <- data_compustat_base %>% 
  group_by(ticker, ith_week) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count == 1)


#### 99. save ####
save(data_compustat,
     
     file = "Intermediate/Compustat/Compustat_1995_2021.Rdata")
