## This file is used to integrate different datasets
## Date created: 20220408


rm(list = ls())
wd <- "C:/Dropbox/Courses/BUSI525/Replication/Data"
# wd <- "D:/Dropbox/Courses/BUSI525/Replication/Data"
setwd(wd)
upd <- 20220419


library(tictoc)
library(lubridate)
library(tidyverse)


#### 0. Load ####

## CRSP
load("Intermediate/CRSP/CRSP_1995_2021.Rdata")

## Compustat
load("Intermediate/Compustat/Compustat_1995_2021.Rdata")

## OptionMetrics
load("Intermediate/OptionMetrics/OptionVolume_1996_2010.Rdata")

## Factors
load("Intermediate/General/Factors.Rdata")



#### 0. Mics. Preparation ####
data_year <- data_days %>% 
  mutate(year = year(date)) %>% 
  
  select(year, ith_week) %>% 
  unique() %>% 
  
  group_by(ith_week) %>% 
  mutate(year_max = max(year, na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  filter(year == year_max) %>% 
  select(year, ith_week)



#### 1. Integrate datasets ####


data_base_raw <- data_option %>% 
  
  left_join(data_crsp,      by = c("ticker", "ith_week")) %>% 
  left_join(data_compustat, by = c("ticker", "ith_week")) %>% 
  left_join(data_factor,    by = c(          "ith_week")) %>% 
  left_join(data_year,      by = c(          "ith_week"))




#### 2. Variable construction ####

data_base <- data_base_raw %>% 
  mutate(O2S  = Option_volume_weekly / stock_volume_weekly,
         C2P  = Call_volume_weekly / Put_volume_weekly,
         
         O2S  = replace(O2S,  is.infinite(O2S),  NA),
         C2P  = replace(C2P,  is.infinite(C2P),  NA)
         ) %>% 
  
  group_by(ticker) %>% 
  fill(B2M, size) %>% 
  mutate(MOMEN       = RcppRoll::roll_prod(1 + stock_return_weekly, n = 24, fill = NA, align = "right", na.rm = TRUE) - 1,
         avg6m_O2S   = RcppRoll::roll_mean(O2S,                     n = 24, fill = NA, align = "right", na.rm = TRUE),
         avg6m_OPVOL = RcppRoll::roll_mean(Option_volume_weekly,    n = 24, fill = NA, align = "right", na.rm = TRUE),
         avg6m_EQVOL = RcppRoll::roll_mean(stock_volume_weekly,     n = 24, fill = NA, align = "right", na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  mutate(LBM  = log(B2M),
         SIZE = log(size),
         
         LBM  = replace(LBM,  is.infinite(LBM),  NA),
         SIZE = replace(SIZE, is.infinite(SIZE), NA)) %>% 
  
  mutate(MOMEN = MOMEN * 100) %>% 
  
  select(ticker, ith_week, year,
         O2S, C2P, 
         Call_volume_weekly, Put_volume_weekly, Option_volume_weekly, stock_volume_weekly,
         SIZE, LBM, MOMEN, stock_amihud_weekly, stock_return_weekly, return_next5days,
         MktRF, SMB, HML, RF, Mom,
         avg6m_O2S, avg6m_OPVOL, avg6m_EQVOL)

  




#### 99. Save ####


save(data_base,
     file = "Intermediate/General/Dataset_Analysis.Rdata")
