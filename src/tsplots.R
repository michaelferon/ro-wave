rm(list = ls())

library(tidyverse)
library(ggplot2)
library(lubridate)
library(viridis)

#Loading the Data
load('../data/data.Rdata')

# Number of experiments
N <- 8
OUTPUT <- FALSE



plot.ts <- function(data, var, title, ylab, ylim, DUAL = FALSE) {
  if (DUAL) {
    df <- data %>%
      select(time, experiment, measure, value = var)
  } else {
    df <- data %>%
      select(time, experiment, value = var)
  }

  for (i in 1:N) {
    g <- df %>%
      filter(experiment == i)
    if (DUAL) {
      g <- g %>% ggplot(aes(time, value, color = measure))
    } else {
      g <- g %>% ggplot(aes(time, value))
    }
    
    g <- g +
      geom_line(size = 0.025) +
      ggtitle(title) +
      labs(subtitle = paste('Experiment', i)) +
      xlab('Time') + ylab(ylab) +
      theme_minimal()
    
    if (DUAL) {
      g <- g +
        scale_color_manual(values = c('SteelBlue', 'Orange3'),
                           labels = c('High', 'Low')) +
        labs(color = 'Measure')
    }
    
    if (OUTPUT) {
      pdf(file = paste('../plots/ts/', var, '/', var, i, '.pdf', sep=''),
          height = 4.0, width = 8.67)
      print(g)
      dev.off()
      
      pdf(file = paste('../plots/ts/experiments/exp', i, '/', var, '.pdf', sep=''),
          height = 4.0, width = 8.67)
      print(g)
      dev.off()
    }
  }
}


## Water permeability.
plot.ts(
  data = data,
  var = 'water_perm_coef',
  title = 'Water Permeability',
  ylab = expression(
    paste('Water Permeability (L m'^2, ' hour'^-1, ' psi'^-1, ')'))
)


## Water flux.
plot.ts(
  data = data,
  var = 'water_flux_lmh',
  title = 'Water Flux',
  ylab = 'Water Flux (lmh)'
)


## Feed pressure.
plot.ts(
  data = data,
  var = 'feed_pressure_psi',
  title = 'Feed Pressure',
  ylab = 'Feed Pressure (psi)'
)


## Feed pump power.
plot.ts(
  data = data,
  var = 'feed_pump_pow',
  title = 'Feed Pump Power',
  ylab = 'Feed Pump Power (%)'
)


## Feed volume.
plot.ts(
  data = data,
  var = 'feed_volume_l',
  title = 'Feed Volume',
  ylab = 'Feed Volume (L)'
)


## Feed temperature.
plot.ts(
  data = data,
  var = 'feed_temp_c',
  title = 'Feed Temperature',
  ylab = 'Feed Temperature (C)'
)


## Feed flowrate.
plot.ts(
  data = data,
  var = 'feed_flow_lm',
  title = 'Feed Flowrate',
  ylab = 'Feed Flowrate (L/m)'
)


## Permeate conductivity.
data %>%
  select(time, experiment, perm_cond_low_us, perm_cond_high_us) %>%
  gather(key = measure, value = permeate_conductivity,
         c('perm_cond_high_us', 'perm_cond_low_us')) %>%
  plot.ts(
    var = 'permeate_conductivity',
    title = 'Permeate Conductivity',
    ylab = 'Permeate Conductivity (uS)',
    DUAL = TRUE
  )


## Permeate flowrate.
plot.ts(
  data = data,
  var = 'perm_flow_lm',
  title = 'Permeate Flowrate',
  ylab = 'Permeate FLowrate (L/m)'
)


## Reject conductivity.
plot.ts(
  data = data,
  var = 'rej_cond_ms',
  title = 'Reject Conductivity',
  ylab = 'Reject Conductivity (mS)'
)


## Reject flowrate.
plot.ts(
  data = data,
  var = 'rej_flow_lm',
  title = 'Reject Flowrate',
  ylab = 'Reject Flowrate (L/m)'
)


## AC current.
plot.ts(
  data = data,
  var = 'ac_current_a',
  title = 'AC Current',
  ylab = 'AC Current (A)'
)


