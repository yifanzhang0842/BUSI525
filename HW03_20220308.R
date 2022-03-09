## This file is used to prepare HW03
## Date created: 20210308


rm(list = ls())
wd = "D:/Dropbox/Courses/BUSI525/Week03"
setwd(wd)
set.seed(20220308)
upd = 20220308


library(tictoc)
library(stargazer)
library(tidyverse)


#### 0. Generate data ####


## Basic para

alpha   <- 0
beta    <- 0.015
sigma_u <- 0.053
theta   <- 0
sigma_v <- 0.044
rho     <- 0.98
# rho_uv  <- -0.8

x_0   <- 0

vec_T <- seq(120, 1200, 120)
B     <- 250

# matrix_uv <- MASS::mvrnorm(n = B, mu = Mu, Sigma = Sigma)


#### 1. Part01 ####

#### 1.1. Simulation ####
rho_uv  <- -0.8

Mu    <- c(0, 0)
Sigma <- matrix(c(sigma_u^2, 
                  rho_uv * sigma_u * sigma_v,  
                  rho_uv * sigma_v * sigma_u,
                  sigma_v^2), 
                ncol = 2)

data_simulation <- NULL

for (i in vec_T) {
  loop_T <- i
  
  loop_B_dataframe <- NULL
  
  for (b in c(1:B)) {
    message("T =", loop_T, ", B = ", b)
    
    loop_uv <- MASS::mvrnorm(n = loop_T, mu = Mu, Sigma = Sigma)
    u_t     <- loop_uv[,1]
    v_t     <- loop_uv[,2]
    x_t_1   <- x_0
    
    loop_B <- data.frame()[1:loop_T, ] %>% 
      mutate(r_t = NA,
             x_t = NA)
    
    for (t in c(1:loop_T)) { 
      
      r_t <- alpha + beta * x_t_1 + u_t[t]
      x_t <- theta + rho  * x_t_1 + v_t[t]
      
      loop_B$r_t[t] <- r_t
      loop_B$x_t[t] <- x_t
      
      x_t_1 <- x_t
      
    }
    
    loop_B <- loop_B %>% 
      mutate(B = b)
    
    
    loop_B_dataframe <- loop_B_dataframe %>% 
      bind_rows(loop_B)
  }
  
  loop_B_dataframe <- loop_B_dataframe %>% 
    mutate(nMonth = loop_T)
  
  data_simulation <- data_simulation %>% 
    bind_rows(loop_B_dataframe)
}


#### 1.2. Estimation ####
data_est08 <- data_simulation %>% 
  group_by(nMonth, B) %>% 
  do(broom::tidy(lm(r_t ~ x_t, data = .))) %>% 
  ungroup()

data_est08_beta <- data_est08 %>% 
  filter(term == "x_t") %>% 
  
  group_by(nMonth) %>% 
  summarise(pct_05  = quantile(estimate, probs = 0.05, na.rm = TRUE),
            average = mean(    estimate,               na.rm = TRUE),
            pct_95  = quantile(estimate, probs = 0.95, na.rm = TRUE)) %>% 
  ungroup()


## plot data
{
  data_plot08 <- data_est08_beta %>% 
    reshape2::melt(id.var = "nMonth")
    
    
  plot08 <- ggplot(data = data_plot08) + 
    aes(x = nMonth, linetype = variable) + 
    geom_line(aes(y = value)) +
    labs(title = "Panel C: correlation of -0.8",
         y = "",
         x = "Sample Size") +
    scale_linetype_manual(values = c("dotdash", "solid", "dotdash"),
                          labels = c("5% percentile", "average", "95% percentile")) + 
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
          legend.position   = c(0.9,0.2))
  
  plot08
  
}






#### 2. Part 2 ####

#### 2.1.1. 0.2 Simulation ####
rho_uv  <- -0.2

Mu    <- c(0, 0)
Sigma <- matrix(c(sigma_u^2, 
                  rho_uv * sigma_u * sigma_v,  
                  rho_uv * sigma_v * sigma_u,
                  sigma_v^2), 
                ncol = 2)


data_simulation <- NULL

for (i in vec_T) {
  loop_T <- i
  
  loop_B_dataframe <- NULL
  
  for (b in c(1:B)) {
    message("T =", loop_T, ", B = ", b)
    
    loop_uv <- MASS::mvrnorm(n = loop_T, mu = Mu, Sigma = Sigma)
    u_t     <- loop_uv[,1]
    v_t     <- loop_uv[,2]
    x_t_1   <- x_0
    
    loop_B <- data.frame()[1:loop_T, ] %>% 
      mutate(r_t = NA,
             x_t = NA)
    
    for (t in c(1:loop_T)) { 
      
      r_t <- alpha + beta * x_t_1 + u_t[t]
      x_t <- theta + rho  * x_t_1 + v_t[t]
      
      loop_B$r_t[t] <- r_t
      loop_B$x_t[t] <- x_t
      
      x_t_1 <- x_t
      
    }
    
    loop_B <- loop_B %>% 
      mutate(B = b)
    
    
    loop_B_dataframe <- loop_B_dataframe %>% 
      bind_rows(loop_B)
  }
  
  loop_B_dataframe <- loop_B_dataframe %>% 
    mutate(nMonth = loop_T)
  
  data_simulation <- data_simulation %>% 
    bind_rows(loop_B_dataframe)
}


#### 2.1.2. 0.2 Estimation ####
data_est02 <- data_simulation %>% 
  group_by(nMonth, B) %>% 
  do(broom::tidy(lm(r_t ~ x_t, data = .))) %>% 
  ungroup()

data_est02_beta <- data_est02 %>% 
  filter(term == "x_t") %>% 
  
  group_by(nMonth) %>% 
  summarise(pct_05  = quantile(estimate, probs = 0.05, na.rm = TRUE),
            average = mean(    estimate,               na.rm = TRUE),
            pct_95  = quantile(estimate, probs = 0.95, na.rm = TRUE)) %>% 
  ungroup()


## plot data
{
  data_plot02 <- data_est02_beta %>% 
    reshape2::melt(id.var = "nMonth")
  
  
  plot02 <- ggplot(data = data_plot02) + 
    aes(x = nMonth, linetype = variable) + 
    geom_line(aes(y = value)) +
    labs(title = "Panel A: correlation of -0.2",
         y = "",
         x = "Sample Size") +
    scale_linetype_manual(values = c("dotdash", "solid", "dotdash"),
                          labels = c("5% percentile", "average", "95% percentile")) + 
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
          legend.position   = c(0.9,0.2))
  
  plot02
  }





#### 2.2.1. 0.5 Simulation ####
rho_uv  <- -0.5

Mu    <- c(0, 0)
Sigma <- matrix(c(sigma_u^2, 
                  rho_uv * sigma_u * sigma_v,  
                  rho_uv * sigma_v * sigma_u,
                  sigma_v^2), 
                ncol = 2)


data_simulation <- NULL

for (i in vec_T) {
  loop_T <- i
  
  loop_B_dataframe <- NULL
  
  for (b in c(1:B)) {
    message("T =", loop_T, ", B = ", b)
    
    loop_uv <- MASS::mvrnorm(n = loop_T, mu = Mu, Sigma = Sigma)
    u_t     <- loop_uv[,1]
    v_t     <- loop_uv[,2]
    x_t_1   <- x_0
    
    loop_B <- data.frame()[1:loop_T, ] %>% 
      mutate(r_t = NA,
             x_t = NA)
    
    for (t in c(1:loop_T)) { 
      
      r_t <- alpha + beta * x_t_1 + u_t[t]
      x_t <- theta + rho  * x_t_1 + v_t[t]
      
      loop_B$r_t[t] <- r_t
      loop_B$x_t[t] <- x_t
      
      x_t_1 <- x_t
      
    }
    
    loop_B <- loop_B %>% 
      mutate(B = b)
    
    
    loop_B_dataframe <- loop_B_dataframe %>% 
      bind_rows(loop_B)
  }
  
  loop_B_dataframe <- loop_B_dataframe %>% 
    mutate(nMonth = loop_T)
  
  data_simulation <- data_simulation %>% 
    bind_rows(loop_B_dataframe)
}


#### 2.2.2. 0.5 Estimation ####
data_est05 <- data_simulation %>% 
  group_by(nMonth, B) %>% 
  do(broom::tidy(lm(r_t ~ x_t, data = .))) %>% 
  ungroup()

data_est05_beta <- data_est05 %>% 
  filter(term == "x_t") %>% 
  
  group_by(nMonth) %>% 
  summarise(pct_05  = quantile(estimate, probs = 0.05, na.rm = TRUE),
            average = mean(    estimate,               na.rm = TRUE),
            pct_95  = quantile(estimate, probs = 0.95, na.rm = TRUE)) %>% 
  ungroup()


## plot data
{
  data_plot05 <- data_est05_beta %>% 
    reshape2::melt(id.var = "nMonth")
  
  
  plot05 <- ggplot(data = data_plot05) + 
    aes(x = nMonth, colour = variable, linetype = variable) + 
    geom_line(aes(y = value)) +
    labs(title = "Panel B: correlation of -0.5",
         y = "",
         x = "Sample Size") +
    scale_linetype_manual(values = c("dotdash", "solid", "dotdash"),
                          labels = c("5% percentile", "average", "95% percentile")) + 
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
          legend.position   = c(0.9,0.2))
  
  plot05
  }








#### 3. Save #### 
plot_facts <- egg::ggarrange(plot02, 
                             plot05,
                             plot08,
                             
                             ncol = 1, nrow = 3)

ggpubr::ggexport(plot_facts,
                 filename = paste0("HW03", upd, ".png"),
                 width = 16 * 35, height = 20 * 35, units = "mm")



