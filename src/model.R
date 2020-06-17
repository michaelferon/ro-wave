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



## Grab experiment 1.
df <- data %>%
  filter(experiment == 1) %>%
  slice(4908:84132)
time <- df$time
pressure <- df$feed_pressure_psi
M <- length(pressure)

## Plot a small segment of our pressure wave.
plot(time[500:600], pressure[500:600], type = 'l')


## Compute the Fourier transform.
ft <- fft(pressure)

# Plot Fourier transform and time series.
t <- seq(0, 1, length = M)*60
par(mfrow = c(2, 1))
plot(time, pressure, type = 'l', lwd = 0.1, main = 'Feed Pressure Time Series',
     xlab = 'Time', ylab = 'Pressure')
plot(t[-1], Mod(ft)[2:M], type = 'h', main = 'Feed Pressure in Frequency Domain',
     ylab = 'Power', xlab = 'Frequency (1/minute)')


## Grab just high-power frequencies.
strongFreq <- ft
strongFreq[Mod(strongFreq) < 100000] <- 0
# Plot fft and high-power fft.
plot(t[-1], Mod(ft)[2:M], type = 'h', main = 'Feed Pressure in Frequency Domain',
     ylab = 'Power', xlab = 'Frequency (1/minute)')
plot(Mod(strongFreq)[2:M], type = 'h', main = 'High-Power Frequencies Only',
     ylab = 'Power', xlab = 'Frequency (1/minute)')



## Rebuild trend signal.
trend <- Re(fft(strongFreq, inverse = TRUE) / M)

# Plot glimpse of original time-series vs. reconstructed trend.
plot(time[500:600], pressure[500:600], type = 'l', xlab = 'Time',
     main = 'Feed Pressure Time Series', ylab = 'Pressure')
plot(time[500:600], trend[500:600], type = 'l', xlab = 'Time',
     ylab = 'Pressure', main = 'Reconstructed Time Series')

# Plot trend and original time-series on top of each other.
par(mfrow = c(1, 1))
plot(time[500:600], pressure[500:600], type = 'l', xlab = 'Time',
     main = 'Feed Pressure Time Series', ylab = 'Pressure')
lines(time[500:600], trend[500:600], col = 'red', lty = 'dashed')

# Plot whole time-series and trend.
par(mfrow = c(2, 1))
plot(time, pressure, type = 'l', lwd = 0.1,
     main = 'Feed Pressure Time Series', xlab = 'Time', ylab = 'Pressure')
plot(time, trend, lwd = 0.1, type = 'l')


## Compute residuals.
res <- pressure - trend

# Plot residuals.
par(mfrow = c(1, 1))
plot(time, res, type = 'l', lwd = 0.1, main = 'Residuals',
     xlab = 'Time', ylab = 'Pressure')


# Plot original time-series and residuals.
par(mfrow = c(2, 1))
plot(time, pressure, type = 'l', lwd = 0.1,
     main = 'Feed Pressure Time Series', xlab = 'Time', ylab = 'Pressure')
plot(time, res, type = 'l', lwd = 0.1, main = 'Residuals', xlab = 'Time',
     ylab = 'Pressure')


## Plot correlograms.
acf(pressure, main = 'Feed Pressure ACF')
acf(res, main = 'Residual ACF')




### LUKE'S METHOD.




