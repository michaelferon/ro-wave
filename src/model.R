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

## Number of experiments = 8.
N <- 8
OUTPUT <- TRUE
DISPLAY <- FALSE



# Test: feed pressure ts plots for each experiment.
for (i in 1:N) {
  df <- data %>%
    filter(experiment == i)
  plot(df$time, df$feed_pressure_psi, type = 'l', lwd = 0.1,
       main = paste('Experiment', i))
}
rm(df)




### Data de-trending.
exp.names <- c('10A', '10B', '10C', '11A', '11B', '11C', '12A', '13')
ts.detrend <- function(data, var, sp, name, q) {
  time <- data$time
  response <- data[[var]]
  N <- length(response)
  expno <- data$experiment[1]
  expname <- exp.names[expno]
  ft <- fft(response)
  
  # Plot time-series and frequency spectrum.
  x.axis <- seq(0, 1/sp, length = N) * 60
  ts.title <- paste('Experiment ', expname, '\n', name, ' Time Series', sep='')
  if (DISPLAY) {
    par(mfrow = c(2, 1))
    invisible(readline(prompt = 'Hit <Return> to see next plot: '))
    plot(time, response, type = 'l', main = ts.title, xlab = 'Time',
         ylab = name, lwd = 0.1, cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
    plot(x.axis[-1], Mod(ft)[-1], type = 'h', main = 'Frequency Spectrum',
         xlab = expression(paste('Frequency (minutes'^-1, ')')),
         ylab = 'Magnitude, |z|', cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
  }
  
  
  # Remove low-power frequencies.
  strongFreq <- ft
  strongFreq[Mod(strongFreq) < quantile(Mod(strongFreq), 1 - q)] <- 0
  colors <- rep('black', nrow(data))
  colors[strongFreq == 0] <- 'red'
  
  # Plot full frequency spectrum and high-power frequency spectrum.
  if (DISPLAY) {
    par(mfrow = c(2, 1))
    invisible(readline(prompt = 'Hit <Return> to see next plot: '))
    plot(x.axis[-1], Mod(ft)[-1], type = 'h',
         main = paste('Experiment ', expname, '\nFull Frequency Spectrum', sep=''),
         xlab = expression(paste('Frequency (minutes'^-1, ')')), col = colors,
         ylab = 'Magnitude, |z|', cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
    legend('top', legend = 'Low Power',
           col = 'red', lty = 1, cex = 1.25)
    plot(x.axis[-1], Mod(strongFreq)[-1], type = 'h',
         main = 'High-Power Frequency Spectrum',
         xlab = expression(paste('Frequency (minutes'^-1, ')')),
         ylab = 'Magnitude, |z|', cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
  }
  
  
  # Re-construct signal using only high-power frequencies.
  trend <- Re(fft(strongFreq, inverse = TRUE)) / N
  
  # Plot glimpse of time-series with reconstruction overlaid.
  if (DISPLAY) {
    par(mfrow = c(1, 1))
    invisible(readline(prompt = 'Hit <Return> to see next plot: '))
    plot(time[5000:5099], response[5000:5099], type = 'l', xlab = 'Time',
         ylab = name, main = paste(ts.title, 'with Reconstruction Overlaid'),
         cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
    lines(time[5000:5099], trend[5000:5099], col = 'red', lty = 2, lwd = 1.5)
    legend('topleft', legend = c('Original', 'Re-construction'),
           col = c('black', 'red'), lty = 1:2, cex = 1.5)
  }
  
  
  # Compute residuals (response - reconstruction)
  res <- response - trend
  
  # Plot original time-series and residuals.
  if (DISPLAY) {
    par(mfrow = c(2, 1))
    invisible(readline(prompt = 'Hit <Return> to see next plot: '))
    plot(time, response, type = 'l', lwd = 0.1, main = ts.title, xlab = 'Time',
         ylab = name, cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
    plot(time, res, type = 'l', lwd = 0.1, main = 'Residuals', xlab = 'Time',
         ylab = name, cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
  }
  
  
  # Plot ACF for original time-series and for residuals.
  if (DISPLAY) {
    par(mfrow = c(2, 1))
    invisible(readline(prompt = 'Hit <Return> to see next plot: '))
    acf(response, cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5,
        main = paste('Experiment ', expname, '\n', name,
                     ' Auto-Correlation Function', sep=''))
    acf(res, main = 'Residual ACF', cex.axis = 1.3, cex.lab = 1.3,
        cex.main = 1.5)
    par(mfrow = c(1, 1))
  }
  
  
  ## Output plots.
  if (OUTPUT) {
    outdir <- paste('../plots/trend/exp', expno, '/', sep='')
    
    png(file = paste(outdir, 'ts-freq.png', sep=''),
        height = 675, width = 850)
    par(mfrow = c(2, 1))
    plot(time, response, type = 'l', main = ts.title, xlab = 'Time',
         ylab = name, lwd = 0.1, cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
    plot(x.axis[-1], Mod(ft)[-1], type = 'h', main = 'Frequency Spectrum',
         xlab = expression(paste('Frequency (minutes'^-1, ')')),
         ylab = 'Magnitude, |z|', cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
    dev.off()
    
    png(file = paste(outdir, 'freq-comp.png', sep=''),
        height = 675, width = 850)
    par(mfrow = c(2, 1))
    plot(x.axis[-1], Mod(ft)[-1], type = 'h',
         main = paste('Experiment ', expname, '\nFull Frequency Spectrum', sep=''),
         xlab = expression(paste('Frequency (minutes'^-1, ')')), col = colors,
         ylab = 'Magnitude, |z|', cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
    legend('top', legend = 'Low Power',
           col = 'red', lty = 1, cex = 1.25)
    plot(x.axis[-1], Mod(strongFreq)[-1], type = 'h',
         main = 'High-Power Frequency Spectrum',
         xlab = expression(paste('Frequency (minutes'^-1, ')')),
         ylab = 'Magnitude, |z|', cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
    dev.off()
    
    png(file = paste(outdir, 'ts-trend.png', sep=''),
        height = 500, width = 1000)
    par(mfrow = c(1, 1))
    plot(time[5000:5099], response[5000:5099], type = 'l', xlab = 'Time',
         ylab = name, main = paste(ts.title, 'with Reconstruction Overlaid'),
         cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
    lines(time[5000:5099], trend[5000:5099], col = 'red', lty = 2, lwd = 1.5)
    legend('topleft', legend = c('Original', 'Re-construction'),
           col = c('black', 'red'), lty = 1:2, cex = 1.5)
    dev.off()
    
    png(file = paste(outdir, 'ts-resid.png', sep=''),
        height = 675, width = 850)
    par(mfrow = c(2, 1))
    plot(time, response, type = 'l', lwd = 0.1, main = ts.title, xlab = 'Time',
         ylab = name, cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
    plot(time, res, type = 'l', lwd = 0.1, main = 'Residuals', xlab = 'Time',
         ylab = name, cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
    dev.off()
    
    png(file = paste(outdir, 'acf.png', sep=''),
        height = 675, width = 850)
    par(mfrow = c(2, 1))
    acf(response, cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5,
        main = paste('Experiment ', expname, '\n', name,
                     ' Auto-Correlation Function', sep=''))
    acf(res, main = 'Residual ACF', cex.axis = 1.3, cex.lab = 1.3,
        cex.main = 1.5)
    par(mfrow = c(1, 1))
    dev.off()
  }
  
  # Reset mfrow.
  par(mfrow = c(1, 1))
  
  df <- tibble(
    time = time,
    response = response,
    ft = ft,
    high_ft = strongFreq,
    trend = trend,
    resid = res
  )
  return(df)
}


## Experiment 1.
dfs <- list()
dfs[[1]] <- data %>%
  filter(experiment == 1) %>%
  ts.detrend(
    var = 'feed_pressure_psi',
    sp = 1,
    name = 'Feed Pressure',
    q = 0.01
  )
## Experiment 2.
dfs[[2]] <- data %>%
  filter(experiment == 2) %>%
  ts.detrend(
    var = 'feed_pressure_psi',
    sp = 1,
    name = 'Feed Pressure',
    q = 0.01
  )
## Experiment 3.
dfs[[3]] <- data %>%
  filter(experiment == 3) %>%
  ts.detrend(
    var = 'feed_pressure_psi',
    sp = 1,
    name = 'Feed Pressure',
    q = 0.01
  )
## Experiment 4.
dfs[[4]] <- data %>%
  filter(experiment == 4) %>%
  ts.detrend(
    var = 'feed_pressure_psi',
    sp = 1,
    name = 'Feed Pressure',
    q = 0.01
  )
## Experiment 5.
dfs[[5]] <- data %>%
  filter(experiment == 5) %>%
  ts.detrend(
    var = 'feed_pressure_psi',
    sp = 1,
    name = 'Feed Pressure',
    q = 0.01
  )
## Experiment 6.
dfs[[6]] <- data %>%
  filter(experiment == 6) %>%
  ts.detrend(
    var = 'feed_pressure_psi',
    sp = 1,
    name = 'Feed Pressure',
    q = 0.01
  )
## Experiment 7.
dfs[[7]] <- data %>%
  filter(experiment == 7) %>%
  ts.detrend(
    var = 'feed_pressure_psi',
    sp = 20,
    name = 'Feed Pressure',
    q = 0.01
  )
## Experiment 8.
dfs[[8]] <- data %>%
  filter(experiment == 8) %>%
  ts.detrend(
    var = 'feed_pressure_psi',
    sp = 15,
    name = 'Feed Pressure',
    q = 0.01
  )


## TEST
test <- dfs[[1]]$resid
x <- seq(min(test), max(test), length = 1000)
y <- dnorm(x, mean = mean(test), sd = sd(test))
par(mfrow = c(1, 2))
plot(density(test),
     main = 'Feed Pressure Density for Experiment 1\nwith Normal Density Overlaid')
lines(x, y, col = 'red', lty = 2)
qqnorm(test)
qqline(test)



### LUKE'S METHOD.
df <- data %>%
  filter(experiment == 1) %>%
  slice(500:600)
time <- df$time
pressure <- df$feed_pressure_psi
M <- nrow(df)

T <- 8
rad <- 2 * pi * (1:M) / T
par(mfrow = c(1, 1))

mod1 <- lm(pressure ~ cos(rad) + sin(rad))
plot(time, pressure, type = 'l', xlab = 'Time',
     main = 'Feed Pressure Time Series', ylab = 'Pressure')
lines(time, fitted(mod1), col = 'red', lty = 'dashed')
summary(mod1)
rm(mod1)




### Linear Models.
df <- data %>%
  filter(experiment == 1) %>%
  select(-experiment, -mode, -status, -rej_valve_open, -rej_flow_lm)
model <- df %>%
  select(-time) %>%
  lm(perm_cond_low_us ~ ., data = .)
plot(df$time[500:1000], df$perm_cond_low_us[500:1000], type = 'l')
lines(df$time[500:1000], fitted(model)[500:1000], col = 'red')

df2 <- data %>%
  filter(experiment == 2) %>%
  select(-experiment, -mode, -status, -rej_valve_open, -rej_flow_lm)
fit <- predict(model, df2 %>% select(-time))
plot(df2$time[500:1000], df2$perm_cond_low_us[500:1000], type = 'l')
lines(df2$time[500:1000], fit[500:1000], col = 'red', lty = 2)







