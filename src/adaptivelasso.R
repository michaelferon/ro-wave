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



## for loop for adaptive lasso 
N <- 8
OUTPUT <- TRUE


for(i in 1:N){
  test <- data %>%
    filter(experiment == i) %>%
    select(-mode, -status, -ac_current_a, -rej_valve_open, -rej_flow_lm, -time, -experiment, -perm_cond_high_us)
  
  predict_col <- which(colnames((test)) == "perm_cond_low_us")
  
  x <- as.matrix(test[,-predict_col])
  y <- as.matrix(test[,predict_col])
  
  mod_ridge <- cv.glmnet(x, y, alpha = 0)
  
  coef(mod_ridge, s = mod_ridge$lambda.min)[, 1]
  weights <- 1/abs(matrix(coef(mod_ridge, s = mod_ridge$lambda.min)[, 1][-1]))^1
  
  mod_adaptive <- cv.glmnet(x, y, alpha = 1, penalty.factor = weights)
  
  if(OUTPUT){
    png(file = paste('../plots/adaptive_lasso/root_mean/adaptivelassoroot', i, '.png', sep = ''), height = 800, width = 2167.5)
  }
  
  plot(mod_adaptive)
  
  if(OUTPUT){
    dev.off()
  }
  
  
  if(OUTPUT){
    png(file = paste('../plots/adaptive_lasso/coefficients/adaptivelassocoeff', i, '.png', sep = ''), height = 800, width = 2167.5)
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

  for(s in s.list){
    coef <- coef(mod_adaptive, s = s)
    
    #Identify selected variables
    selected_attributes <- (coef@i[-1]) 
    colnames(x)[selected_attributes]
    
    #Compute R-squared
    predictions <- predict(mod_adaptive, newx = x, s = s)
    SSE = mean((y - predictions)^2); print(SSE)
    SST = mean((y - mean(y))^2); print(SST)
    Rsqu = 1 - SSE/SST; print(Rsqu)
    
    #Plot prediction
    data1 <- y
    data2 <- predictions
    max.val <- max(c(data1, data2))
    min.val <- min(c(data1, data2))
    data2plot <- cbind(data1, data2)
    
    if(OUTPUT){
      png(file = paste('../plots/adaptive_lasso/residual/residuals', i, '.png', sep = ''), height = 800, width = 2167.5)
    }
    
      plot(x = data2plot[,1], y = data2plot[,2]
          , xlim=c(min.val,max.val), ylim=c(min.val,max.val)
          , xlab="Actual", ylab="Predicted"
          , pch=20
          , main = paste("Lambda = ",round(s,3),", Variables = ", length(selected_attributes), collapse="")
      )
      abline(a=0,b=1,col="blue", lwd=2)
    
      legend("bottomright", 
            inset = c(-.01,-.01),
            legend = c("Observation", "Perfect Fit"
                        , paste0("R-sq = ",round(Rsqu,2))),
            col = c("black", "blue", NA),
            pch = c(20,NA, NA),
            lwd = c(NA,2, NA),
            bty = "n",
            cex=2)
  }
  
  if(OUTPUT){
    dev.off()
  }
  
  if(OUTPUT){
    png(file = paste('../plots/adaptive_lasso/ts/time_series', i, '.png', sep = ''), height = 800, width = 2167.5)
  }
  
  par(mfrow=c(1,1))
  plot(data$time[data$experiment == i], as.numeric(test$perm_cond_low_us), type = "l", xlab = "", ylab = "")
  lines(data$time[data$experiment == i],  predict(mod_adaptive, newx = x, s = mod_adaptive$lambda.1se), col=2)
  
  if(OUTPUT){
    dev.off()
  }
}
