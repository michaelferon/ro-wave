rm(list = ls())

library(tidyverse)
library(ggplot2)
library(ggridges)
library(viridis)
library(glmnet)

getwd()

load("../data/data.Rdata")


data %>%
  ggplot(aes(x = water_flux_lmh, y = mode, fill = experiment)) +
  geom_density_ridges() +
  theme_ridges() +
  labs(x = "Water Flux") + 
  theme(legend.position = "none")


data %>%
  ggplot(aes(x = feed_pressure_psi, y = mode, fill = experiment)) +
  geom_density_ridges() +
  theme_ridges() +
  labs(x = "Feed Pressure") + 
  theme(legend.position = "none")


# Lasso
test <- data %>%
  filter(experiment == 1) %>%
  select(-mode, -status, -ac_current_a, -rej_valve_open, -rej_flow_lm, -time, -experiment)

predict_col <- which(colnames((test)) == "perm_cond_low_us")

y <- as.matrix(test[,predict_col])
x <- as.matrix(test[,-predict_col])

mod_ridge <- cv.glmnet(x, y, alpha = 0)

coef(mod_ridge, s = mod_ridge$lambda.min)[ , 1]
weights <- 1/abs(matrix(coef(mod_ridge, s = mod_ridge$lambda.min)[, 1][-1]))^1

mod_adaptive <- cv.glmnet(x, y,  alpha = 1, penalty.factor = weights)

plot(mod_adaptive)



## for loop for adaptive lasso 
N <- 8
OUTPUT <- TRUE

for(i in 1:N){
  test <- data %>%
    filter(experiment == i) %>%
    select(-mode, -status, -ac_current_a, -rej_valve_open, -rej_flow_lm, -time, -experiment)
  
  predict_col <- which(colnames((test)) == "perm_cond_low_us")
  
  x <- as.matrix(test[,-predict_col])
  y <- as.matrix(test[,predict_col])
  
  mod_ridge <- cv.glmnet(x, y, alpha = 0)
  
  coef(mod_ridge, s = mod_ridge$lambda.min)[, 1]
  weights <- 1/abs(matrix(coef(mod_ridge, s = mod_ridge$lambda.min)[, 1][-1]))^1
  
  mod_adaptive <- cv.glmnet(x, y, alpha = 1, penalty.factor = weights)
  
  if(OUTPUT){
    png(file = paste('../plots/adaptive_lasso/adaptivelasso', i, '.png', sep = ''), height = 800, width = 2167.5)
  }
  
  plot(mod_adaptive)
  
  if(OUTPUT){
    dev.off()
  }
  
  
  if(OUTPUT){
    png(file = paste('../plots/adaptive_lasso/adaptivelasso', i, '.png', sep = ''), height = 800, width = 2167.5)
  }
  
  rain <- plasma(ncol(x))
  plot(mod_adaptive$glmnet.fit, xvar = "lambda", label = TRUE, col = rain, cex.lab = 1.5)
  abline(v = log(mod_adaptive$lambda.min), col = "black")
  abline(v = log(mod_adaptive$lambda.1se), col = "blue")
  abline(v = log(6*mod_adaptive$lambda.1se),col= "red")
  abline(v = 0,col="green")
  
  
  if(OUTPUT){
    dev.off()
  }
  
  s.list <- c(as.numeric(mod_adaptive$lambda.min), as.numeric(mod_adaptive$lambda.1se), as.numeric(6*mod_adaptive$lambda.1se), 1)

}







