rm(list = ls())

## Load libraries.
library(tidyverse)
library(ggplot2)
library(lubridate)
library(viridis)

## Load data.
load('../data/data.Rdata')

## Number of experiments = 8.
N <- 8
OUTPUT <- TRUE




exp.names <- c('10A', '10B', '10C', '11A', '11B', '11C', '12A', '13')
exp.salt <- c('35 g/L NaCl', '35 g/L NaCl', '35 g/L NaCl', 'Instant Ocean',
              'Instant Ocean', 'Instant Ocean', '1.5x Instant Ocean',
              'Instant Ocean')
exp.wave <- c('7.5 wave/min', '1.25 wave/min', '12 wave/min', '7.5 wave/min',
              '1.25 wave/min', '12 wave/min', '7.5 wave/min', 'NREL wave')
plot.ts <- function(data, var, title, ylab, ylim) {
  df <- data %>%
    select(time, experiment, value = var)

  for (i in 1:N) {
    g <- df %>%
      filter(experiment == i) %>%
      ggplot(aes(time, value)) +
      geom_line(size = 0.1) +
      xlab('Time') + ylab(ylab) + ylim(ylim) +
      theme_minimal(base_size = 35)
    
    if (OUTPUT) {
      # pdf(file = paste('../plots/ts/', var, '/', var, i, '.pdf', sep=''),
      #     height = 4.0, width = 8.67)
      png(file = paste('../plots/ts/', var, '/', var, i, '.png', sep=''),
          height = 800, width = 2167.5)
      print(
        g + ggtitle(paste('Experiment', exp.names[i])) +
          labs(subtitle = paste(exp.salt[i], ', ', exp.wave[i], sep=''))
      )
      dev.off()
      
      # pdf(file = paste('../plots/ts/experiments/exp', i, '/', var, '.pdf', sep=''),
      #     height = 4.0, width = 8.67)
      png(file = paste('../plots/ts/experiments/exp', i, '/', var, '.png', sep=''),
          height = 800, width = 2167.5)
      print(
        g + ggtitle(title)
      )
      dev.off()
    }
  }
}


## Water permeability.
plot.ts(
  data = data,
  var = 'water_perm_coef',
  title = 'Water Permeability',
  ylab = 'Water Permeability (lmh/psi)',
  ylim = range(data$water_perm_coef)
)


## Water flux.
plot.ts(
  data = data,
  var = 'water_flux_lmh',
  title = 'Water Flux',
  ylab = 'Water Flux (lmh)',
  ylim = range(data$water_flux_lmh)
)


## Feed pressure.
plot.ts(
  data = data,
  var = 'feed_pressure_psi',
  title = 'Feed Pressure',
  ylab = 'Feed Pressure (psi)',
  ylim = range(data$feed_pressure_psi)
)


## Feed pump power.
plot.ts(
  data = data,
  var = 'feed_pump_pow',
  title = 'Feed Pump Power',
  ylab = 'Feed Pump Power (%)',
  ylim = range(data$feed_pump_pow)
)


## Feed volume.
plot.ts(
  data = data,
  var = 'feed_volume_l',
  title = 'Feed Volume',
  ylab = 'Feed Volume (L)',
  ylim = range(data$feed_volume_l)
)


## Feed temperature.
plot.ts(
  data = data,
  var = 'feed_temp_c',
  title = 'Feed Temperature',
  ylab = 'Feed Temperature (C)',
  ylim = range(data$feed_temp_c)
)


## Feed flowrate.
plot.ts(
  data = data,
  var = 'feed_flow_lm',
  title = 'Feed Flowrate',
  ylab = 'Feed Flowrate (L/m)',
  ylim = range(data$feed_flow_lm)
)


## Permeate conductivity.
plot.ts(
  data = data,
  var = 'perm_cond_low_us',
  title = 'Permeate Conductivity',
  ylab = 'Permeate Conductivity (uS)',
  ylim = range(data$perm_cond_low_us)# c(600, 900)
)


## Permeate flowrate.
plot.ts(
  data = data,
  var = 'perm_flow_lm',
  title = 'Permeate Flowrate',
  ylab = 'Permeate FLowrate (L/m)',
  ylim = range(data$perm_flow_lm)
)


## Reject conductivity.
plot.ts(
  data = data,
  var = 'rej_cond_ms',
  title = 'Reject Conductivity',
  ylab = 'Reject Conductivity (mS)',
  ylim = range(data$rej_cond_ms)
)


## Reject flowrate.
plot.ts(
  data = data,
  var = 'rej_flow_lm',
  title = 'Reject Flowrate',
  ylab = 'Reject Flowrate (L/m)',
  ylim = range(data$rej_flow_lm)
)


## AC current.
plot.ts(
  data = data,
  var = 'ac_current_a',
  title = 'AC Current',
  ylab = 'AC Current (A)',
  ylim = range(data$ac_current_a)
)




## Scatterplot matrices.
for (i in 1:N) {
  if (OUTPUT) {
    png(file = paste('../plots/scatter_matrix/corr/corr', i,
                     '.png', sep = ''), height = 1000, width = 1000)
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
      pairs(cex = 0.01, main = paste('Experiment', exp.names[i]))
    dev.off()
    
    png(file = paste('../plots/scatter_matrix/other/other', i,
                     '.png', sep = ''), height = 1000, width = 1000)
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
      pairs(cex = 0.01, main = paste('Experiment', exp.names[i]))
    dev.off()
  }
}


