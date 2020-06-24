rm(list = ls())

## Load libraries.
library(lubridate)
library(viridis)
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)

## Load data.
load('../data/data.Rdata')
load('../data/bigdfs.Rdata')

N <- 8
vars <- c('water_perm_coef', 'water_flux_lmh', 'feed_pressure_psi',
          'feed_pump_pow', 'feed_volume_l', 'feed_flow_lm', 'perm_flow_lm',
          'ac_current_a')

test <- big.dfs[[5]]


df <- tibble(
  time = test$water_perm_coef$time,
  trend.water_perm_coef = test$water_perm_coef$trend,
  resid.water_perm_coef = test$water_perm_coef$resid
)

for (i in 2:8) {
  df[[2*i]] = test[[i]]$trend
  df[[2*i + 1]] = test[[i]]$resid
}


names(df) <- c('time', paste(rep(c('trend.', 'resid.'), times = 8),
                             rep(vars, each = 2), sep=''))

df$perm_cond_low_us <- data$perm_cond_low_us[data$experiment == 5]

  
df <- df %>%
  select(-time)




predict_col <- which(colnames((df)) == "perm_cond_low_us")

x <- as.matrix(df[,-predict_col])
y <- as.matrix(df[,predict_col])

mod_ridge <- cv.glmnet(x, y, alpha = 0)

coef(mod_ridge, s = mod_ridge$lambda.min)[, 1]
weights <- 1/abs(matrix(coef(mod_ridge, s = mod_ridge$lambda.min)[, 1][-1]))^1

mod_adaptive <- cv.glmnet(x, y, alpha = 1, penalty.factor = weights)

plot(mod_adaptive)

rain <- plasma(ncol(x))
plot(mod_adaptive$glmnet.fit, xvar = "lambda", label = TRUE, col = rain, cex.lab = 1.5)
abline(v = log(mod_adaptive$lambda.min), col = "black")
abline(v = log(mod_adaptive$lambda.1se), col = "blue")
abline(v = log(6*mod_adaptive$lambda.1se),col= "red")
abline(v = 0,col="green")



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
  
  
  
  par(mfrow=c(1,1))
  plot(data$time[data$experiment == i], as.numeric(test$perm_cond_low_us), type = "l", xlab = "", ylab = "")
  lines(data$time[data$experiment == i],  predict(mod_adaptive, newx = x, s = mod_adaptive$lambda.1se), col=2)
}
