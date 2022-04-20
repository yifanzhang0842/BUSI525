## This file is used to integrate different datasets
## Date created: 20220408


rm(list = ls())
wd <- "C:/Dropbox/Courses/BUSI525/Replication/Data"
wd <- "D:/Dropbox/Courses/BUSI525/Replication/Data"
setwd(wd)
upd <- 20220419


library(tictoc)
library(lubridate)
library(tidyverse)


#### 0. Load ####

# ## CRSP
# load("Intermediate/CRSP/CRSP_1995_2021.Rdata")
# 
# ## Compustat
# load("Intermediate/Compustat/Compustat_1995_2021.Rdata")
# 
# ## OptionMetrics
# load("Intermediate/OptionMetrics/OptionVolume_1996_2010.Rdata")
# 
## Factors
load("Intermediate/General/Factors.Rdata")

## Analysis
load("Intermediate/General/Dataset_Analysis.Rdata")


#### 1. Summary Statistics ####
Table01A <- data_base %>% 
  group_by(year) %>% 
  mutate(O2S_w = DescTools::Winsorize(O2S, probs = c(0.01, 0.99), na.rm = TRUE)) %>% 
  summarise(count_firm     = length(unique(ticker)),
            count_firmweek = n(),
            
            avg_O2S  = mean(    O2S_w, na.rm = TRUE),
            p25      = quantile(O2S_w, probs = 0.25, na.rm = TRUE),
            p50      = quantile(O2S_w, probs = 0.50, na.rm = TRUE),
            p75      = quantile(O2S_w, probs = 0.65, na.rm = TRUE),
            skewness = rockchalk::skewness(O2S_w, na.rm = TRUE)) %>% 
  ungroup()


Table01B <- data_base %>% 
  drop_na(O2S) %>% 
  
  group_by(ith_week) %>% 
  mutate(decile = ntile(O2S, 10)) %>% 
  ungroup() %>% 
  
  group_by(decile) %>% 
  summarise(VLC   = mean(Call_volume_weekly,   na.rm = TRUE),
            VLP   = mean(Put_volume_weekly,    na.rm = TRUE),
            OPVOL = mean(Option_volume_weekly, na.rm = TRUE),
            EQVOL = mean(stock_volume_weekly,  na.rm = TRUE),
            SIZE  = mean(SIZE,                 na.rm = TRUE),
            LBM   = mean(LBM,                  na.rm = TRUE),
            MOMEN = mean(MOMEN,                na.rm = TRUE)) %>% 
  ungroup()
  

#### 2. Factor Regression ####


Table02_base <- data_base %>% 
  drop_na(O2S) %>% 
  
  mutate(delta_O2S   = (O2S - avg6m_O2S) / avg6m_O2S,
         delta_OPVOL = (Option_volume_weekly - avg6m_OPVOL) / avg6m_OPVOL,
         delta_EQVOL = (stock_volume_weekly  - avg6m_EQVOL) / avg6m_EQVOL) %>% 
  
  group_by(ticker) %>% 
  mutate(Delta_O2S = ntile(O2S, 100)) %>% 
  
  mutate(RET1 = 100 * lead(stock_return_weekly, 1)) %>% 
  mutate(RET1 = 100 * return_next5days) %>% 
  ungroup()  %>% 
  
  group_by(ith_week) %>% 
  mutate(decile_O2S       = ntile(O2S, 10),
         decile_delta_O2S = ntile(delta_O2S, 10),
         decile_Delta_O2S = ntile(Delta_O2S, 10)) %>% 
  ungroup() 


Table02A_base <- Table02_base %>% 
  rename(decile = decile_delta_O2S) %>% 
  drop_na(decile) %>% 
  
  group_by(ith_week, decile) %>% 
  mutate(decile_ret = mean(RET1, na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  select(ticker, ith_week, decile, decile_ret) %>% 
  left_join(data_factor %>% mutate(ith_week = ith_week - 1), by = "ith_week")


Table02A1 <- Table02A_base %>% 
  group_by(decile) %>% 
  do(broom::tidy(lm(decile_ret ~ MktRF, data = .))) %>%
  ungroup() %>% 
  
  filter(term == "(Intercept)") 
  # select(decile, term, estimate, statistic) %>% 
  # pivot_longer(col = c("estimate", "statistic"),
  #              names_to = "test",
  #              values_to = "value")



Table02A2 <- Table02A_base %>% 
  group_by(decile) %>% 
  do(broom::tidy(lm(decile_ret ~ MktRF + SMB + HML, data = .))) %>%
  ungroup() %>% 
  
  filter(term == "(Intercept)")


Table02A3 <- Table02A_base %>% 
  group_by(decile) %>% 
  do(broom::tidy(lm(decile_ret ~ MktRF + SMB + HML + Mom, data = .))) %>%
  ungroup() %>% 
  
  filter(term == "(Intercept)")

Table02A <- Table02A1 %>% mutate(Model1 = "CAPM")       %>% select(Model1, decile, estimate, statistic) %>% rename(coef1 = estimate, tstat1 = statistic) %>% 
  bind_cols(Table02A2 %>% mutate(Model2 = "FF3")        %>% select(Model2, decile, estimate, statistic) %>% rename(coef2 = estimate, tstat2 = statistic)) %>% 
  bind_cols(Table02A3 %>% mutate(Model3 = "FourFactor") %>% select(Model3, decile, estimate, statistic) %>% rename(coef3 = estimate, tstat3 = statistic)) 



#### 3. Fama MacBeth ####


Table03_base <- data_base %>% 
  drop_na(O2S) %>% 
  
  mutate(delta_O2S = (O2S - avg6m_O2S) / avg6m_O2S,
         delta_OPVOL = (Option_volume_weekly - avg6m_OPVOL) / avg6m_OPVOL,
         delta_EQVOL = (stock_volume_weekly  - avg6m_EQVOL) / avg6m_EQVOL) %>% 
  
  group_by(ticker) %>% 
  mutate(Delta_O2S = ntile(O2S, 100)) %>% 
  
  mutate(RET1 = 100 * lead(stock_return_weekly, 1)) %>% 
  mutate(RET1 = 100 * return_next5days) %>% 
  ungroup() %>% 
  
  mutate(RET0 = stock_return_weekly)


Table03A_base <- Table03_base %>% 
  group_by(ith_week) %>% 
  mutate(decile_O2S         = ntile(O2S,         10),
         decile_delta_O2S   = ntile(delta_O2S,   10),
         decile_Delta_O2S   = ntile(Delta_O2S,   10),
         decile_delta_OPVOL = ntile(delta_OPVOL, 10),
         decile_delta_EQVOL = ntile(delta_EQVOL, 10)) %>% 
  ungroup() %>% 
  
  select(ticker, ith_week, 
         RET1, RET0,
         O2S, delta_O2S, Delta_O2S,
         decile_O2S        ,
         decile_delta_O2S  ,
         decile_Delta_O2S  ,
         decile_delta_OPVOL,
         decile_delta_EQVOL,
         MOMEN, SIZE, LBM, stock_amihud_weekly) %>% 
  drop_na(decile_O2S, decile_delta_O2S, decile_Delta_O2S, 
          decile_delta_OPVOL, decile_delta_EQVOL, 
          MOMEN, SIZE, LBM, stock_amihud_weekly) %>% 
  mutate(AMIHUD = stock_amihud_weekly / 1000000)



Table03A1 <- pmg(RET1 ~ decile_Delta_O2S                                                  + MOMEN + SIZE + LBM, index=c("ith_week","ticker"), data = Table03A_base)
# Table03A2 <- pmg(RET1 ~ decile_Delta_O2S                                                  + MOMEN + SIZE + LBM + AMIHUD, index=c("ith_week","ticker"), data = Table03A_base)
Table03A3 <- pmg(RET1 ~ decile_Delta_O2S                                           + RET0 + MOMEN + SIZE + LBM, index=c("ith_week","ticker"), data = Table03A_base)
Table03A4 <- pmg(RET1 ~              decile_delta_OPVOL + decile_delta_EQVOL + RET0 + MOMEN + SIZE + LBM, index=c("ith_week","ticker"), data = Table03A_base)
Table03A5 <- pmg(RET1 ~ decile_Delta_O2S + decile_delta_OPVOL                      + RET0 + MOMEN + SIZE + LBM, index=c("ith_week","ticker"), data = Table03A_base)
Table03A6 <- pmg(RET1 ~ decile_Delta_O2S + decile_delta_EQVOL                      + RET0 + MOMEN + SIZE + LBM, index=c("ith_week","ticker"), data = Table03A_base)
Table03A7 <- pmg(RET1 ~ decile_Delta_O2S + decile_delta_EQVOL                             + MOMEN + SIZE + LBM, index=c("ith_week","ticker"), data = Table03A_base)


stargazer::stargazer(Table03A1,
                     # Table03A2,
                     Table03A3,
                     Table03A4,
                     Table03A5,
                     Table03A6,
                     Table03A7,
                     type = "text")


broom::tidy(Table03A7)

#### 6. Skewness ####
Table06_base <- data_base %>% 
  drop_na(C2P) %>% 
  
  group_by(ticker) %>% 
  mutate(RET1 = 100 * lead(stock_return_weekly, 1)) %>% 
  # mutate(RET1 = 100 * return_next5days) %>% 
  ungroup() %>% 
  mutate(RET0 = 100 * stock_return_weekly) %>% 
  
  group_by(ith_week) %>% 
  mutate(decile_C2P = ntile(C2P,10)) %>% 
  ungroup() %>% 
  
  group_by(ith_week, year, decile_C2P) %>% 
  summarise(skew    = rockchalk::skewness(RET1, na.rm = TRUE),
            l1_skew = rockchalk::skewness(RET0, na.rm = TRUE)) %>% 
  ungroup()
  
  
Table06_c1 <- lfe::felm(skew ~ decile_C2P           | year | 0 | ith_week, data = Table06_base)
Table06_c2 <- lfe::felm(skew ~ decile_C2P + l1_skew | year | 0 | ith_week, data = Table06_base)

stargazer::stargazer(Table06_c1,
                     Table06_c2,
                     type = "text")
#### 99. Save ####


