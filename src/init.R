rm(list = ls())

## Load libraries.
library(lubridate)
library(viridis)
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)

# List of .csv files.
files <- list.files('../data/raw', full.names = TRUE)

# Read in data.
data <- read_csv(files[1], skip = 1, col_types = cols(Time = col_character()))
data$experiment <- 1
for (i in 2:8) {
  temp <- read_csv(files[i], skip = 1, col_types = cols(Time = col_character()))
  temp$experiment <- i
  data <- data %>%
    rbind(temp)
}

# Rename variables.
data <- data %>%
  rename(
    date = Date,
    time = Time,
    runtime_h = `Runtime (h)`,
    iter = Iteration,
    perm_cond_high_us = `Permeate Conductivity [High] (uS)`,
    perm_cond_low_us = `Permeate Conductivity [Low] (uS)`,
    rej_cond_ms = `Reject Conductivity (mS)`,
    feed_pressure_psi = `Feed Pressure (psi)`,
    feed_volume_l = `Feed Volume (L)`,
    feed_temp_c = `Feed Temperature (C)`,
    perm_flow_lm = `Permeate Flowrate (L/min)`,
    rej_flow_lm = `Reject Flowrate (L/min)`,
    feed_flow_lm = `Feed Flowrate (L/min)`,
    feed_pump_pow = `Feed Pump Power (%)`,
    temp_valve_open = `Temperatre Valve Open (%)`,
    rej_valve_open = `Reject Valve Open (%)`,
    water_flux_lmh = `Water Flux (LMH)`,
    total_perm_l = `Total Permeate  (L)`,
    ac_current_a = `AC Current (A)`,
    perm_tank_level = `Permeate Tank Level`,
    feed_clean_valve_state = `Feed Cleaning Valve State`,
    perm_dis_valve_state = `Permeate Discharge Valve State`,
    mode = `System Mode`,
    status = Status,
    code = Code,
    source = Source,
    lj_string = `LJ String`,
    experiment = experiment
  )


# Convert date and time to date-time.
data$time <- paste(mdy(data$date), data$time) %>%
  parse_date_time('%Y-%m-%d %H:%M:%S %p')

# Remove unnecessary variables.
del <- c('date', 'runtime_h', 'iter', 'temp_valve_open', 'perm_tank_level',
         'feed_clean_valve_state', 'perm_dis_valve_state', 'code', 'source',
         'lj_string')
data <- data %>%
  select(-all_of(del)) %>%
  mutate_at(c('mode', 'status', 'experiment'), as.factor)


## Save data.
save(data, file = '../data/data.Rdata')




