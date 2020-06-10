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

## Number of experiments = 9.
N <- 8
OUTPUT <- FALSE


## Experiments summarries.
tapply(data$experiment, data$experiment, length)
(data.mean <- data %>%
    select(-time, -mode, -status) %>%
    group_by(experiment) %>%
    summarise_all(mean, na.rm = TRUE))
(data.sd <- data %>%
    select(-time, -mode, -status) %>%
    group_by(experiment) %>%
    summarise_all(sd, na.rm = TRUE))
(data.iqr <- data %>%
    select(-time, -mode, -status) %>%
    group_by(experiment) %>%
    summarise_all(IQR, na.rm = TRUE))
(data.mad <- data %>%
    select(-time, -mode, -status) %>%
    group_by(experiment) %>%
    summarise_all(mad, na.rm = TRUE))

if (OUTPUT) {
  data %>%
    select(-time, -mode, -status) %>%
    group_by(experiment) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    write_csv(path = '../data/mean.csv')
}




## Scatterplot matrices.
for (i in 1:N) {
  if (OUTPUT) {
    pdf(file = paste('../plots/scatter_matrix/scatter_matrix', i,
                     '.pdf', sep = ''), height = 10.0, width = 10.0)
  }
  data %>%
    filter(experiment == i) %>%
    select(water_flux_lmh, feed_pressure_psi, feed_volume_l, feed_flow_lm,
           feed_pump_pow, perm_flow_lm, perm_cond_low_us, rej_cond_ms) %>%
    rename(
      flux = water_flux_lmh,
      feed_psi = feed_pressure_psi,
      feed_vol = feed_volume_l,
      feed_flow = feed_flow_lm,
      feed_pow = feed_pump_pow,
      perm_flow = perm_flow_lm,
      perm_cond = perm_cond_low_us,
      rej_cond = rej_cond_ms,
    ) %>%
    pairs(cex = 0.10) #, col = colors[i])
  if (OUTPUT) {
    dev.off()
  }
}





