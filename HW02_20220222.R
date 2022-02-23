## This file is used to prepare HW02
## Date created: 20210222


rm(list = ls())
wd = "D:/Dropbox/Courses/BUSI525/Week02"
setwd(wd)
set.seed(20220222)
upd = 20220222


library(tictoc)
library(stargazer)
library(tidyverse)


#### 0. Generate data ####
data_market <- data.frame(Month   = 1:120,
                          Mkt_ret = rnorm(120, 0.05/12, 0.2/sqrt(12)))


data_funds_base <- data.frame(fund = 1:1000) %>% 
  crossing(data.frame(Month   = c(1:120))) %>% 
  left_join(data_market, by = "Month") %>% 
  
  mutate(alpha   = 0,
         beta    = 1,
         epsilon = rnorm(120 * 1000, 0, 0.1/sqrt(12))) %>% 
  
  mutate(fund_ret = alpha + beta * Mkt_ret + epsilon)


#### 0. Estimation ####

data_est01_raw <- NULL

for (i in c(1:1000)) {
  loop_fund <- data_funds_base %>% filter(fund == i)
  
  loop_lm  <- lm(fund_ret ~ Mkt_ret, data = loop_fund)
  loop_sum <- summary(loop_lm)
  
  loop_est <- data.frame(residual = loop_lm$residuals) %>% 
    mutate(fund        = i,
           Month       = 1:n(),
           alpha_est   = loop_lm$coefficients[1],
           beta_est    = loop_lm$coefficients[2],
           alpha_tstat = loop_sum$coefficients[1,3])
  
  data_est01_raw <- data_est01_raw %>% 
    bind_rows(loop_est)
}

data_est01 <- data_est01_raw %>% 
  select(fund, Month,
         alpha_est, beta_est, alpha_tstat, residual)

data_funds <- data_funds_base %>% 
  left_join(data_est01, by = c("fund", "Month"))



#### Part 1 ####

data_result01 <- NULL


tic()
for (b in c(1:100)) {
  message("Now running loop No.", b)
  
  ## bootstrap 120 months
  loop_b_months <- sample(1:120, 120, replace=T)
  
  loop_b_ind_matrix <- outer(loop_b_months, c(0:999) * 120, FUN = "+")
  loop_b_ind <- as.vector(loop_b_ind_matrix)
  
  
  ## construct zero-skill returns
  loop_boot_raw <- data_funds[loop_b_ind,]
  row.names(loop_boot_raw) <- c(1:nrow(loop_boot_raw))
  
  loop_boot <- loop_boot_raw %>% 
    transmute(Month_b    = Month,
              Mkt_ret_b  = Mkt_ret,
              residual_b = residual)
  
  loop_data <- data_funds %>% 
    bind_cols(loop_boot) %>% 
    
    mutate(fund_ret_b = beta_est * Mkt_ret_b + residual_b)
  
  
  ## bootstrapped estimates
  loop_est <- loop_data %>% 
    group_by(fund) %>% 
    do(broom::tidy(lm(fund_ret_b ~ Mkt_ret_b, data = .))) %>% 
    ungroup()
  
  loop_result <- loop_est %>% 
    filter(term == "(Intercept)") %>% 
    rename(alpha_hat_b  = estimate,
           t_stat_hat_b = statistic) %>% 
    
    mutate(ind_bootstrap = b) %>% 
    
    select(ind_bootstrap, fund, alpha_hat_b, t_stat_hat_b)
  
  
  ## save
  data_result01 <- data_result01 %>% 
    bind_rows(loop_result)
  
}
toc()

## plot data
{
  data_analysis_base01 <- data_result01 %>% 
    group_by(ind_bootstrap) %>% 
    arrange(t_stat_hat_b) %>% 
    mutate(rank = 1:n()) %>% 
    ungroup() %>% 
    
    arrange(ind_bootstrap)
  
  data_analysis01_05th <- data_analysis_base01 %>% filter(rank ==  50)
  data_analysis01_95th <- data_analysis_base01 %>% filter(rank == 950)
  
  data_analysis01_simu <- data_analysis_base01 %>% 
    group_by(rank) %>% 
    summarise(tstat = mean(t_stat_hat_b)) %>% 
    ungroup() %>% 
    
    mutate(groups = "Bootstrapped")
  
  data_analysis01_actu <- data_est01 %>% 
    group_by(fund) %>% 
    summarise(tstat = mean(alpha_tstat)) %>% 
    ungroup() %>% 
    
    mutate(groups = "Actual")
  
  
  data_plot_ecdf <- data_analysis01_simu %>% 
    bind_rows(data_analysis01_actu)
  
  data_plot_pct <- data_analysis01_05th %>% mutate(percentile = "5th") %>% 
    bind_rows(data_analysis01_95th %>% mutate(percentile = "95th") )
}

## plot
{
  plot_ecdf <- ggplot(data = data_plot_ecdf) + 
    aes(x = tstat, colour = groups) + 
    stat_ecdf() +
    labs(title = "Panel A: CDF of the bootstrapped and actual t-statistics",
         y = "",
         x = "t-statistics") +
    theme_bw() +
    theme(plot.title       = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          # panel.background = element_blank(),
          axis.line   = element_line(colour = "black"),
          legend.title      = element_blank(),
          legend.text       = element_text(size = 8),
          # legend.key        = element_blank(),
          # legend.key.size   = unit(0.2, "cm"),
          legend.key.height = unit(0.18, "cm"),
          legend.key.width  = unit(1, "cm"),
          legend.background = element_rect(colour = 'black', fill = NA, linetype='solid'),
          legend.position   = c(0.2,0.9)) +
    guides(linetype = guide_legend(nrow = 1))
  
  
  
  
  plot_tstats <- ggplot(data = data_plot_pct) +
    aes(x = t_stat_hat_b, colour = percentile) +
    geom_density() + 
    scale_x_continuous(limits = c(-3,3)) +
    labs(title = "Panel B: PDF of the 5th and 95th percentiles",
         y = "",
         # x = "Date"
         x = "t-statistics") +
    theme_bw() +
    theme(plot.title       = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          # panel.background = element_blank(),
          axis.line   = element_line(colour = "black"),
          legend.title      = element_blank(),
          legend.text       = element_text(size = 8),
          # legend.key        = element_blank(),
          # legend.key.size   = unit(0.2, "cm"),
          legend.key.height = unit(0.18, "cm"),
          legend.key.width  = unit(1, "cm"),
          legend.background = element_rect(colour = 'black', fill = NA, linetype='solid'),
          legend.position   = c(0.5,0.9)) +
    guides(linetype = guide_legend(nrow = 1))
  
  
  plot_facts <- egg::ggarrange(plot_ecdf, plot_tstats,
                               ncol = 1, nrow = 2)
  
  ggpubr::ggexport(plot_facts,
                   filename = paste0("Part01_", upd, ".png"),
                   width = 16 * 35, height = 20 * 35, units = "mm")
}






#### Part 2 ####

func_part02 <- function(data_funds_base02 = data_funds_base, 
                        alpha_true        = 0.01, 
                        lambda            = 0.1) {
  
  id_skilled   <- c(1:(1000*lambda))
  id_unskilled <- setdiff(c(1:1000), id_skilled)
  
  data_funds_base02 <- data_funds_base02 %>% 
    mutate(signal_skill = is.element(fund, c(1:(1000*lambda))),
           alpha        = replace(alpha, signal_skill, alpha_true / 12)) %>% 
    
    mutate(fund_ret = alpha + beta * Mkt_ret + epsilon)
  
  
  ## 
  data_est02_raw <- NULL
  
  for (i in c(1:1000)) {
    loop_fund <- data_funds_base02 %>% filter(fund == i)
    
    loop_lm  <- lm(fund_ret ~ Mkt_ret, data = loop_fund)
    loop_sum <- summary(loop_lm)
    
    loop_est <- data.frame(residual = loop_lm$residuals) %>% 
      mutate(fund        = i,
             Month       = 1:n(),
             alpha_est   = loop_lm$coefficients[1],
             beta_est    = loop_lm$coefficients[2],
             alpha_tstat = loop_sum$coefficients[1,3])
    
    data_est02_raw <- data_est02_raw %>% 
      bind_rows(loop_est)
  }
  
  data_est02 <- data_est02_raw %>% 
    select(fund, Month,
           alpha_est, beta_est, alpha_tstat, residual)
  
  data_funds02 <- data_funds_base02 %>% 
    left_join(data_est02, by = c("fund", "Month"))
  
  return(data_funds02)
  
}




data_funds <- func_part02(data_funds_base02 = data_funds_base, 
                          alpha_true        = 0.050, 
                          lambda            = 0.75)

## standard


data_result01 <- NULL


tic()
for (b in c(1:100)) {
  message("Now running loop No.", b)
  
  ## bootstrap 120 months
  loop_b_months <- sample(1:120, 120, replace=T)
  
  loop_b_ind_matrix <- outer(loop_b_months, c(0:999) * 120, FUN = "+")
  loop_b_ind <- as.vector(loop_b_ind_matrix)
  
  
  ## construct zero-skill returns
  loop_boot_raw <- data_funds[loop_b_ind,]
  row.names(loop_boot_raw) <- c(1:nrow(loop_boot_raw))
  
  loop_boot <- loop_boot_raw %>% 
    transmute(Month_b    = Month,
              Mkt_ret_b  = Mkt_ret,
              residual_b = residual)
  
  loop_data <- data_funds %>% 
    bind_cols(loop_boot) %>% 
    
    mutate(fund_ret_b = beta_est * Mkt_ret_b + residual_b)
  
  
  ## bootstrapped estimates
  loop_est <- loop_data %>% 
    group_by(fund) %>% 
    do(broom::tidy(lm(fund_ret_b ~ Mkt_ret_b, data = .))) %>% 
    ungroup()
  
  loop_result <- loop_est %>% 
    filter(term == "(Intercept)") %>% 
    rename(alpha_hat_b  = estimate,
           t_stat_hat_b = statistic) %>% 
    
    mutate(ind_bootstrap = b) %>% 
    
    select(ind_bootstrap, fund, alpha_hat_b, t_stat_hat_b)
  
  
  ## save
  data_result01 <- data_result01 %>% 
    bind_rows(loop_result)
  
}
toc()

## plot data
{
  data_analysis_base01 <- data_result01 %>% 
    group_by(ind_bootstrap) %>% 
    arrange(t_stat_hat_b) %>% 
    mutate(rank = 1:n()) %>% 
    ungroup() %>% 
    
    arrange(ind_bootstrap)
  
  data_analysis01_05th <- data_analysis_base01 %>% filter(rank ==  50)
  data_analysis01_95th <- data_analysis_base01 %>% filter(rank == 950)
  
  data_analysis01_simu <- data_analysis_base01 %>% 
    group_by(rank) %>% 
    summarise(tstat = mean(t_stat_hat_b)) %>% 
    ungroup() %>% 
    
    mutate(groups = "Bootstrapped")
  
  data_analysis01_actu <- data_est01 %>% 
    group_by(fund) %>% 
    summarise(tstat = mean(alpha_tstat)) %>% 
    ungroup() %>% 
    
    mutate(groups = "Actual")
  
  
  data_plot_ecdf <- data_analysis01_simu %>% 
    bind_rows(data_analysis01_actu)
  
  data_plot_pct <- data_analysis01_05th %>% mutate(percentile = "5th") %>% 
    bind_rows(data_analysis01_95th %>% mutate(percentile = "95th") )
}

## plot
{
  plot_ecdf <- ggplot(data = data_plot_ecdf) + 
    aes(x = tstat, colour = groups) + 
    stat_ecdf() +
    labs(title = "Panel A: CDF of the bootstrapped and actual t-statistics",
         y = "",
         x = "t-statistics") +
    theme_bw() +
    theme(plot.title       = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          # panel.background = element_blank(),
          axis.line   = element_line(colour = "black"),
          legend.title      = element_blank(),
          legend.text       = element_text(size = 8),
          # legend.key        = element_blank(),
          # legend.key.size   = unit(0.2, "cm"),
          legend.key.height = unit(0.18, "cm"),
          legend.key.width  = unit(1, "cm"),
          legend.background = element_rect(colour = 'black', fill = NA, linetype='solid'),
          legend.position   = c(0.2,0.9)) +
    guides(linetype = guide_legend(nrow = 1))
  
  
  
  
  plot_tstats <- ggplot(data = data_plot_pct) +
    aes(x = t_stat_hat_b, colour = percentile) +
    geom_density() + 
    scale_x_continuous(limits = c(-3,3)) +
    labs(title = "Panel B: PDF of the 5th and 95th percentiles",
         y = "",
         # x = "Date"
         x = "t-statistics") +
    theme_bw() +
    theme(plot.title       = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          # panel.background = element_blank(),
          axis.line   = element_line(colour = "black"),
          legend.title      = element_blank(),
          legend.text       = element_text(size = 8),
          # legend.key        = element_blank(),
          # legend.key.size   = unit(0.2, "cm"),
          legend.key.height = unit(0.18, "cm"),
          legend.key.width  = unit(1, "cm"),
          legend.background = element_rect(colour = 'black', fill = NA, linetype='solid'),
          legend.position   = c(0.5,0.9)) +
    guides(linetype = guide_legend(nrow = 1))
  
  
  plot_facts <- egg::ggarrange(plot_ecdf, plot_tstats,
                               ncol = 1, nrow = 2)
  
  
}

ggpubr::ggexport(plot_facts,
                 filename = paste0("Part02_Alpha050_lambda75", upd, ".png"),
                 width = 16 * 35, height = 20 * 35, units = "mm")

#### save ####

