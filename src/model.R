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

ns <- cumsum(tapply(data$experiment, data$experiment, length)[-8])
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

# Test.
for (i in 1:N) {
  df <- data %>%
    filter(experiment == i)
  plot(df$time, df$feed_pressure_psi, type = 'l', lwd = 0.1,
       main = paste('Experiment', i))
}




### Data de-trending.
ts.detrend <- function(data, var, sp, name, q) {
  time <- data$time
  response <- data[[var]]
  N <- length(response)
  ft <- fft(response)
  expno <- data$experiment[1]
  
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
  plot(x.axis[-1], Mod(ft)[-1], type = 'h', main = 'Full Frequency Spectrum',
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
       main = paste(ts.title, 'with reconstruction overlaid'))
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
  acf(response, main = paste(name, 'ACF'))
  acf(res, main = 'Residual ACF')
  par(mfrow = c(1, 1))
}


## Experiment 1.
data %>%
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




### LUKE'S METHOD.
T <- 8 # Period.
rad <- 2 * pi * (1:M) / T
par(mfrow = c(1, 1))
phi <- 0

mod1 <- lm(pressure ~ cos(rad + phi) + sin(rad + phi))
plot(time[500:600], pressure[500:600], type = 'l', xlab = 'Time',
     main = 'Feed Pressure Time Series', ylab = 'Pressure')
lines(time[500:600], fitted(mod1)[500:600], col = 'red', lty = 'dashed')
summary(mod1)
rm(mod1)


mod2 <- lm(pressure ~ cos(rad) + sin(rad) + cos(2*rad) + sin(2*rad))
plot(time[500:600], pressure[500:600], type = 'l', xlab = 'Time',
     main = 'Feed Pressure Time Series', ylab = 'Pressure')
lines(time[500:600], fitted(mod2)[500:600], col = 'red', lty = 'dashed')


mod3 <- lm(pressure ~ cos(rad) + sin(rad) + cos(2*rad) + sin(2*rad) +
             cos(3*rad) + sin(3*rad))
plot(time[500:600], pressure[500:600], type = 'l', xlab = 'Time',
     main = 'Feed Pressure Time Series', ylab = 'Pressure')
lines(time[500:600], fitted(mod3)[500:600], col = 'red', lty = 'dashed')


mod4 <- lm(pressure ~ cos(rad) + sin(rad) + cos(2*rad) + sin(2*rad) +
             cos(3*rad) + sin(3*rad) + cos(4*rad) + sin(4*rad))
plot(time[500:600], pressure[500:600], type = 'l', xlab = 'Time',
     main = 'Feed Pressure Time Series', ylab = 'Pressure')
lines(time[500:600], fitted(mod4)[500:600], col = 'red', lty = 'dashed')


mod5 <- lm(pressure ~ cos(rad) + sin(rad) + cos(2*rad) + sin(2*rad) +
             cos(3*rad) + sin(3*rad) + cos(4*rad) + sin(4*rad) +
             cos(5*rad) + sin(5*rad))
plot(time[500:600], pressure[500:600], type = 'l', xlab = 'Time',
     main = 'Feed Pressure Time Series', ylab = 'Pressure')
lines(time[500:600], fitted(mod4)[500:600], col = 'red', lty = 'dashed')





