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

# Number of experiments.
N <- 8

# Read in data.
data <- read_csv(files[1], skip = 1, col_types = cols(Time = col_character()))
data$experiment <- 1
for (i in 2:N) {
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
    perm_total_l = `Total Permeate  (L)`,
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
del <- c('date', 'runtime_h', 'iter', 'temp_valve_open', 'perm_total_l',
         'perm_tank_level', 'feed_clean_valve_state', 'perm_dis_valve_state',
         'code', 'source', 'lj_string')
data <- data %>%
  select(-all_of(del)) %>%
  mutate_at(c('mode', 'status', 'experiment'), as.factor)


# Water permeability coefficient function.
water_perm_coef <- function(flux, feed_pressure, rej_cond) {
  rej_in_psi <- rej_cond * 0.5 * 10
  water_coef <- flux / (feed_pressure - rej_in_psi)
  return(water_coef)
}
# Add water permeability coefficient and rearrange variables.
data <- data %>%
  mutate(
    water_perm_coef = water_perm_coef(water_flux_lmh, feed_pressure_psi, rej_cond_ms)
  ) %>%
  select(time, experiment, water_perm_coef, water_flux_lmh, feed_pressure_psi,
         feed_pump_pow, feed_volume_l, feed_temp_c, feed_flow_lm,
         perm_cond_low_us, perm_cond_high_us, perm_flow_lm, rej_cond_ms,
         rej_flow_lm, rej_valve_open, ac_current_a, mode, status)



## Removing outliers.
first <- c(400, 500, 300, 500, 360, 400, 21, 36)
last <- c(100, 90, 50, 60, 30, 290, 0, 22)
temp <- data %>%
  filter(experiment == 1) %>%
  slice(first[1]:(n() - last[1]))
for (i in 2:N) {
  temp <- data %>%
    filter(experiment == i) %>%
    slice(first[i]:(n() - last[i])) %>%
    rbind(temp, .)
}

fullData <- data
data <- temp

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
rm(temp, ns, keep)


## Save data.
save(fullData, file = '../data/fullData.Rdata')
save(data, file = '../data/data.Rdata')




