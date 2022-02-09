## This file is used to prepare HW01
## Date created: 20210208


rm(list = ls())
wd = "D:/Dropbox/Courses/BUSI525/Week01"
setwd(wd)
set.seed(20220208)
upd = 20220208



library(stargazer)
library(tidyverse)


#### 0. Generate data ####
data_market <- data.frame(Month   = 1:120,
                          Mkt_ret = rnorm(120, 0.05/12, 0.2/sqrt(12)))


data_funds <- data.frame(fund = 1:1000) %>% 
  crossing(data.frame(Month   = c(1:120))) %>% 
  left_join(data_market, by = "Month") %>% 
  
  mutate(alpha   = 0,
         beta    = 1,
         epsilon = rnorm(120 * 1000, 0, 0.1/sqrt(12))) %>% 
  
  mutate(fund_ret = alpha + beta * Mkt_ret + epsilon)


#### Part 1 ####
data_est01 <- data_funds %>% 
  group_by(fund) %>% 
  do(broom::tidy(lm(fund_ret ~ Mkt_ret, data = .))) %>% 
  ungroup()
  
data_est01_alpha <- data_est01 %>% 
  filter(term == "(Intercept)") %>% 
  rename(p_value = "p.value")

sum(data_est01_alpha$p_value <= 0.05)

plot_tstats <- ggplot(data = data_est01_alpha) +
  aes(x = statistic) +
  geom_histogram(bins = 50) + 
  labs(title = "Panel A: t-statistics",
       y = "",
       # x = "Date"
       x = "t-statistics") +
  theme_bw() +
  theme(plot.title       = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        # panel.background = element_blank(),
        axis.line   = element_line(colour = "black")) 

plot_pvalue <- ggplot(data = data_est01_alpha) +
  aes(x = p_value) +
  geom_histogram(bins = 50) + 
  labs(title = "Panel B: p-values",
       y = "",
       # x = "Date"
       x = "p-values") +
  theme_bw() +
  theme(plot.title       = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        # panel.background = element_blank(),
        axis.line   = element_line(colour = "black")) 


plot_facts <- egg::ggarrange(plot_tstats, plot_pvalue,
                             ncol = 1, nrow = 2)

ggpubr::ggexport(plot_facts,
                 filename = paste0("Part01_", upd, ".png"),
                 width = 16 * 35, height = 20 * 35, units = "mm")

#### Part 2 ####

func_part02 <- function(data_fund = data_fund, lambda = 0.1) {
  id_skilled   <- c(1:(1000*lambda))
  id_unskilled <- setdiff(c(1:1000), id_skilled)
  
  n_skilled   <- length(id_skilled)
  n_unskilled <- length(id_unskilled)
  
  data_funds02 <- data_funds %>% 
    mutate(signal_skill = is.element(fund, c(1:(1000*lambda))),
           alpha        = replace(alpha, signal_skill, 0.025 / 12)) %>% 
    
    mutate(fund_ret = alpha + beta * Mkt_ret + epsilon)
  
  data_est02 <- data_funds02 %>% 
    group_by(fund) %>% 
    do(broom::tidy(lm(fund_ret ~ Mkt_ret, data = .))) %>% 
    ungroup()
  
  data_est02_alpha <- data_est02 %>% 
    filter(term == "(Intercept)") %>% 
    rename(p_value = "p.value")
  
  plot_alpha <- ggplot(data = data_est02_alpha) +
    aes(x = estimate) +
    geom_histogram(bins = 50) + 
    labs(title = "Panel A: Estimated Alpha",
         y = "",
         # x = "Date"
         x = "Estimated Alpha") +
    theme_bw() +
    theme(plot.title       = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          # panel.background = element_blank(),
          axis.line   = element_line(colour = "black"))
  
  plot_tstats <- ggplot(data = data_est02_alpha) +
    aes(x = statistic) +
    geom_histogram(bins = 50) + 
    labs(title = "Panel B: t-statistics",
         y = "",
         # x = "Date"
         x = "t-statistics") +
    theme_bw() +
    theme(plot.title       = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          # panel.background = element_blank(),
          axis.line   = element_line(colour = "black")) 
  
  plot_pvalue <- ggplot(data = data_est02_alpha) +
    aes(x = p_value) +
    geom_histogram(bins = 50) + 
    labs(title = "Panel C: p-values",
         y = "",
         # x = "Date"
         x = "p-values") +
    theme_bw() +
    theme(plot.title       = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          # panel.background = element_blank(),
          axis.line   = element_line(colour = "black")) 
  
  
  plot_facts <- egg::ggarrange(plot_alpha, plot_tstats, plot_pvalue,
                               ncol = 1, nrow = 3)
  
  ggpubr::ggexport(plot_facts,
                   filename = paste0("Part02_lambda", lambda*100, "_", upd, ".png"),
                   width = 16 * 35, height = 20 * 35, units = "mm")
  
  
  vec_matrix <- c(
    sum(data_est02_alpha %>% filter(fund %in% id_skilled)  %>% .$p_value <= 0.05) / n_skilled  ,
    sum(data_est02_alpha %>% filter(fund %in% id_skilled)  %>% .$p_value >  0.05) / n_skilled  ,
    sum(data_est02_alpha %>% filter(fund %in% id_unskilled)%>% .$p_value <= 0.05) / n_unskilled,
    sum(data_est02_alpha %>% filter(fund %in% id_unskilled)%>% .$p_value >  0.05) / n_unskilled
  ) * 100 %>% round(2)
  
  return(vec_matrix)
  
}

vec_matrix_10 <- func_part02(data_funds, lambda = 0.10)
vec_matrix_25 <- func_part02(data_funds, lambda = 0.25)
vec_matrix_50 <- func_part02(data_funds, lambda = 0.50)
vec_matrix_75 <- func_part02(data_funds, lambda = 0.75)




#### save ####

