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
OUTPUT <- TRUE


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
    pdf(file = paste('../plots/scatter_matrix/corr/corr', i,
                     '.pdf', sep = ''), height = 10.0, width = 10.0)
    set.seed(1)
    data %>%
      filter(experiment == i) %>%
      slice(sample(1:nrow(.), 10000, replace = FALSE)) %>%
      select(water_flux_lmh, feed_pressure_psi, feed_pump_pow, feed_flow_lm,
             perm_flow_lm, rej_flow_lm, ac_current_a) %>%
      rename(
        flux = water_flux_lmh,
        feed_psi = feed_pressure_psi,
        feed_pow = feed_pump_pow,
        feed_flow = feed_flow_lm,
        perm_flow = perm_flow_lm,
        rej_flow = rej_flow_lm,
        ac_curr = ac_current_a
      ) %>%
      pairs(cex = 0.01)
    dev.off()
    
    pdf(file = paste('../plots/scatter_matrix/other/other', i,
                     '.pdf', sep = ''), height = 10.0, width = 10.0)
    set.seed(1)
    data %>%
      filter(experiment == i) %>%
      slice(sample(1:nrow(.), 10000, replace = FALSE)) %>%
      select(water_perm_coef, feed_pressure_psi, feed_volume_l, feed_temp_c,
             perm_cond_low_us, rej_cond_ms) %>%
      rename(
        perm_coef = water_perm_coef,
        feed_psi = feed_pressure_psi,
        feed_vol = feed_volume_l,
        feed_temp = feed_temp_c,
        perm_cond = perm_cond_low_us,
        rej_cond = rej_cond_ms
      ) %>%
      pairs(cex = 0.01)
    dev.off()
  }
}





