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
OUTPUT <- FALSE




### Data removal.
# df <- data %>%
#   filter(experiment == 1)
# df %>%
#   ggplot(aes(time, feed_pressure_psi, color = mode)) +
#   geom_line()
# a <- which(diff(which(df$mode == 'SYSTEM MODE: Auto Water Flux (Fixed)')) > 1)
# which(df$mode == 'SYSTEM MODE: Auto Water Flux (Fixed)')[(a - 1):(a + 2)]

ns <- cumsum(tapply(data$experiment, data$experiment, length)[-N])
keep <- c(
  4908:84132,
  4238:81182 + ns[1],
  5323:80322 + ns[2],
  5585:84648 + ns[3],
  2991:75638 + ns[4],
  5558:90965 + ns[5],
  235:36045 + ns[6],
  624:79433 + ns[7]
)
data <- data %>%
  slice(keep)

# Test: feed pressure ts plots for each experiment.
for (i in 1:N) {
  df <- data %>%
    filter(experiment == i)
  plot(df$time, df$feed_pressure_psi, type = 'l', lwd = 0.1,
       main = paste('Experiment', i))
}
rm(ns, keep, df)




### Data de-trending.
ts.detrend <- function(data, var, sp, name, q) {
  time <- data$time
  response <- data[[var]]
  N <- length(response)
  expno <- data$experiment[1]
  start <- 500
  ft <- fft(response)
  
  # Plot time-series and frequency spectrum.
  par(mfrow = c(2, 1))
  x.axis <- seq(0, 1/sp, length = N) * 60
  ts.title <- paste('Experiment ', expno, '\n', name, ' Time Series', sep='')
  invisible(readline(prompt = 'Hit <Return> to see next plot: '))
  plot(time, response, type = 'l', main = ts.title, xlab = 'Time', ylab = name,
       lwd = 0.1)
  plot(x.axis[-1], Mod(ft)[-1], type = 'h', main = 'Frequency Spectrum',
       xlab = expression(paste('Frequency (minutes'^-1, ')')),
       ylab = 'Magnitude, |z|')
  
  
  # Remove low-power frequencies.
  strongFreq <- ft
  strongFreq[Mod(strongFreq) < quantile(Mod(strongFreq), 1 - q)] <- 0
  
  # Plot full frequency spectrum and high-power frequency spectrum.
  par(mfrow = c(2, 1))
  invisible(readline(prompt = 'Hit <Return> to see next plot: '))
  plot(x.axis[-1], Mod(ft)[-1], type = 'h',
       main = paste('Experiment ', expno, '\nFull Frequency Spectrum', sep=''),
       xlab = expression(paste('Frequency (minutes'^-1, ')')),
       ylab = 'Magnitude, |z|')
  plot(x.axis[-1], Mod(strongFreq)[-1], type = 'h', main = 'High-Power Frequency Spectrum',
       xlab = expression(paste('Frequency (minutes'^-1, ')')),
       ylab = 'Magnitude, |z|')
  
  
  # Re-construct signal using only high-power frequencies.
  trend <- Re(fft(strongFreq, inverse = TRUE)) / N
  
  # Plot glimpse of time-series with reconstruction overlaid.
  par(mfrow = c(1, 1))
  invisible(readline(prompt = 'Hit <Return> to see next plot: '))
  plot(time[500:599], response[500:599], type = 'l', xlab = 'Time', ylab = name,
       main = paste(ts.title, 'with Reconstruction Overlaid'))
  lines(time[500:599], trend[500:599], col = 'red', lty = 2)
  
  
  # Compute residuals (response - reconstruction)
  res <- response - trend
  
  # Plot original time-series and residuals.
  par(mfrow = c(2, 1))
  invisible(readline(prompt = 'Hit <Return> to see next plot: '))
  plot(time, response, type = 'l', lwd = 0.1, main = ts.title, xlab = 'Time',
       ylab = name)
  plot(time, res, type = 'l', lwd = 0.1, main = 'Residuals', xlab = 'Time',
       ylab = name)
  
  
  # Plot ACF for original time-series and for residuals.
  par(mfrow = c(2, 1))
  invisible(readline(prompt = 'Hit <Return> to see next plot: '))
  response.acf <- acf(response, main = paste(name, 'ACF'))
  resid.acf <- acf(res, main = 'Residual ACF')
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
data %>%
  filter(experiment == 2) %>%
  ts.detrend(
    var = 'feed_pressure_psi',
    sp = 1,
    name = 'Feed Pressure',
    q = 0.01
  )
## Experiment 3.
data %>%
  filter(experiment == 3) %>%
  ts.detrend(
    var = 'feed_pressure_psi',
    sp = 1,
    name = 'Feed Pressure',
    q = 0.01
  )
## Experiment 4.
data %>%
  filter(experiment == 4) %>%
  ts.detrend(
    var = 'feed_pressure_psi',
    sp = 1,
    name = 'Feed Pressure',
    q = 0.01
  )
## Experiment 5.
data %>%
  filter(experiment == 5) %>%
  ts.detrend(
    var = 'feed_pressure_psi',
    sp = 1,
    name = 'Feed Pressure',
    q = 0.01
  )
## Experiment 6.
data %>%
  filter(experiment == 6) %>%
  ts.detrend(
    var = 'feed_pressure_psi',
    sp = 1,
    name = 'Feed Pressure',
    q = 0.01
  )
## Experiment 7.
data %>%
  filter(experiment == 7) %>%
  ts.detrend(
    var = 'feed_pressure_psi',
    sp = 20,
    name = 'Feed Pressure',
    q = 0.01
  )
## Experiment 8.
data %>%
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
phi <- 0

mod1 <- lm(pressure ~ cos(rad + phi) + sin(rad + phi))
plot(time, pressure, type = 'l', xlab = 'Time',
     main = 'Feed Pressure Time Series', ylab = 'Pressure')
lines(time, fitted(mod1), col = 'red', lty = 'dashed')
summary(mod1)
rm(mod1)


mod2 <- lm(pressure ~ cos(rad) + sin(rad) + cos(2*rad) + sin(2*rad))
plot(time, pressure, type = 'l', xlab = 'Time',
     main = 'Feed Pressure Time Series', ylab = 'Pressure')
lines(time, fitted(mod2), col = 'red', lty = 'dashed')


mod3 <- lm(pressure ~ cos(rad) + sin(rad) + cos(2*rad) + sin(2*rad) +
             cos(3*rad) + sin(3*rad))
plot(time, pressure, type = 'l', xlab = 'Time',
     main = 'Feed Pressure Time Series', ylab = 'Pressure')
lines(time, fitted(mod3), col = 'red', lty = 'dashed')


mod4 <- lm(pressure ~ cos(rad) + sin(rad) + cos(2*rad) + sin(2*rad) +
             cos(3*rad) + sin(3*rad) + cos(4*rad) + sin(4*rad))
plot(time, pressure, type = 'l', xlab = 'Time',
     main = 'Feed Pressure Time Series', ylab = 'Pressure')
lines(time, fitted(mod4), col = 'red', lty = 'dashed')


mod5 <- lm(pressure ~ cos(rad) + sin(rad) + cos(2*rad) + sin(2*rad) +
             cos(3*rad) + sin(3*rad) + cos(4*rad) + sin(4*rad) +
             cos(5*rad) + sin(5*rad) + cos(6*rad) + sin(6*rad) +
             cos(7*rad) + sin(7*rad) + cos(8*rad) + sin(8*rad))
plot(time, pressure, type = 'l', xlab = 'Time',
     main = 'Feed Pressure Time Series', ylab = 'Pressure')
lines(time, fitted(mod5), col = 'red', lty = 'dashed')




df <- data %>%
  filter(experiment == 1)
model <- df %>%
  select(-time, -experiment, -mode, -status) %>%
  lm(perm_cond_low_us ~ ., data = .)
plot(df$time[500:1000], df$perm_cond_low_us[500:1000], type = 'l')
lines(df$time[500:1000], fitted(model)[500:1000], col = 'red')


plot(df$time, df$perm_cond_low_us, type = 'l', lwd = 0.1)
plot(df$time, log(df$perm_cond_low_us), type = 'l', lwd = 0.1)
plot(df$time, 1/df$perm_cond_low_us, type = 'l', lwd = 0.1)





