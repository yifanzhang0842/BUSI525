## This file is used to process misc datasets
## Date created: 20220418

rm(list = ls())
wd <- "C:/Dropbox/Courses/BUSI525/Replication/Data"
# wd <- "D:/Dropbox/Courses/BUSI525/Replication/Data"
upd <- 20220418
setwd(wd)

library(tictoc)
library(lubridate)
library(tidyverse)



#### 0. Load ####


## CRSP
load("Intermediate/CRSP/CRSP_1995_2021.Rdata")

## FF3
read_FF3 <- read.csv(file = "Source/FamaFrench/F-F_Research_Data_Factors_weekly.csv",
                     stringsAsFactors = FALSE,
                     skip = 4)

read_Mom <- read.csv(file = "Source/FamaFrench/F-F_Momentum_Factor_daily.csv",
                     stringsAsFactors = FALSE,
                     skip = 13)



#### 1. Factors ####

data_FF3 <- read_FF3 %>% 
  rename(date = X,
         MktRF = Mkt.RF) %>% 
  
  mutate(date = ymd(date)) %>% 
  drop_na(date)


data_Mom <- read_Mom %>% 
  rename(date = X) %>% 
  
  mutate(date = ymd(date)) %>% 
  drop_na(date)
  

data_factor <- data_days %>% 
  left_join(data_FF3, by = "date") %>% 
  left_join(data_Mom, by = "date") %>% 
  
  group_by(ith_week) %>% 
  summarise(MktRF = mean(MktRF, na.rm = TRUE),
            SMB   = mean(SMB, na.rm = TRUE),
            HML   = mean(HML, na.rm = TRUE),
            RF    = mean(RF, na.rm = TRUE),
            # This momentum factor is incorrect
            Mom   = mean(Mom, na.rm = TRUE)) %>% 
  ungroup()

#### 99. Save ####
save(data_factor,
     file = "Intermediate/General/Factors.Rdata")


