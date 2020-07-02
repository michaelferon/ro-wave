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
sps <- c(rep(1, 6), 20, 15)
exp.names <- c('10A', '10B', '10C', '11A', '11B', '11C', '12A', '13')



# Test: feed pressure ts plots for each experiment.
if (DISPLAY) {
  for (i in 1:N) {
    df <- data %>%
      filter(experiment == i)
    plot(df$time, df$feed_pressure_psi, type = 'l', lwd = 0.1,
         main = paste('Experiment', i))
  }
  rm(df)
}




### Data de-trending.
ts.detrend <- function(data, var, sp, name, q) {
  if (nrow(data) %% 2) {
    cut <- (nrow(data) + 1) / 2
  } else {
    cut <- nrow(data) / 2 + 1
  }
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
    
    
    a <- 5000
    b <- 5099
    png(file = paste(outdir, 'test.png', sep=''),
        height = 675, width = 850)
    par(mfrow = c(2, 1))
    plot(x.axis[-1][1:cut], Mod(ft)[-1][1:cut], type = 'h',
         main = paste('Experiment ', expname, '\nFrequency Spectrum', sep=''),
         xlab = expression(paste('Frequency (minutes'^-1, ')')), col = colors[1:cut],
         ylab = 'Magnitude, |z|', cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
    legend('top', legend = 'Low Power',
           col = 'red', lty = 1, cex = 1.25)
    plot(time[a:b], response[a:b], type = 'l', xlab = 'Time',
         ylab = name, main = paste(ts.title, 'with Reconstruction Overlaid'),
         cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
    lines(time[a:b], trend[a:b], col = 'red', lty = 2, lwd = 1.5)
    legend('topleft', legend = c('Original', 'Re-construction'),
           col = c('black', 'red'), lty = 1:2, cex = 1.5)
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



## Principal Components.
pc.dfs <- list()
for (i in 1:N) {
  df <- data %>%
    filter(experiment == i) %>%
    select(water_perm_coef, water_flux_lmh) %>%
    scale
  
  eig <- eigen(t(df) %*% df)
  
  pc <- df %*% eig$vectors[, 1] %>%
    as_tibble
  pc$experiment <- i
  pc$time <- data$time[data$experiment == i]
  
  pc.dfs[[i]] <- pc %>%
    ts.detrend(
      var = 'V1',
      sp = sps[i],
      name = expression(paste(1^st, ' Principal Component')),
      q = 0.01
    )
}

save(pc.dfs, file = '../data/pcdfs.Rdata')





OUTPUT <- FALSE
DISPLAY <- FALSE

big.dfs <- list()
vars <- c('water_perm_coef', 'water_flux_lmh', 'feed_pressure_psi',
          'feed_pump_pow', 'feed_volume_l', 'feed_flow_lm', 'perm_flow_lm',
          'ac_current_a')
names <- c('Water Permeability', 'Water Flux', 'Feed Pressure',
           'Feed Pump Power', 'Feed Volume', 'Feed Flowrate',
           'Permeate Flowrate', 'AC Current')

for (i in 1:N) {
  exp.df <- data %>%
    filter(experiment == i)
  temp.list <- list()
  
  for (j in 1:length(vars)) {
    temp.df <- exp.df %>%
      ts.detrend(
        var = vars[j],
        sp = sps[i],
        name = names[j],
        q = 0.01
      )
    names(temp.df)[names(temp.df) == 'response'] <- vars[j]
    temp.list[[j]] <- temp.df
  }
  
  big.dfs[[i]] <- temp.list
}
names(big.dfs) <- paste('exp', 1:N, sep='')
for (i in 1:N) {
  names(big.dfs[[i]]) <- vars
}

save(big.dfs, file = '../data/bigdfs.Rdata')












