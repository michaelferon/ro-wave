###############################################################################
# This source file is for our initial data input and cleaning.
###############################################################################

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



## Removing outliers by chunks of indices.
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

# Keep old data stored in `fullData`, rename new data to `data`.
fullData <- data
data <- temp

# More outlier removal. The indices in `keep` are ranges of indices within
# each experiment which we want to keep in our data. `ns` is the
# cumulative sum of the number of observations in each experiment, which allows
# us to get index locations in the original, full data frame.
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





###############################################################################
# This source file performs the Fast Fouier Transform.
###############################################################################

rm(list = ls())

## Load libraries.
library(lubridate)
library(viridis)
library(ggplot2)
library(tidyr)
library(dplyr)

## Load data.
load('../data/data.Rdata')

## Number of experiments = 8.
N <- 8
# `OUTPUT` controls whether or not output plots are produced.
OUTPUT <- FALSE


# List of parameters for each experiment.
exp.names <- c('10A', '10B', '10C', '11A', '11B', '11C', '12A', '13')
exp.salt <- c('35 g/L NaCl', '35 g/L NaCl', '35 g/L NaCl', 'Instant Ocean',
              'Instant Ocean', 'Instant Ocean', '1.5x Instant Ocean',
              'Instant Ocean')
exp.wave <- c('7.5 wave/min', '1.25 wave/min', '12 wave/min', '7.5 wave/min',
              '1.25 wave/min', '12 wave/min', '7.5 wave/min', 'NREL wave')
# This is the de facto sampling period for each experiment (in seconds).
end <- c(rep(1, 6), 20, 15)


dfs <- list() # Empty list to hold the results of the FFT for each experiment.
# Loop through each experiment.
for (i in 1:N) {
  # Filter by experiment.
  temp <- data %>%
    filter(experiment == i)
  # `cut` is the Nyquist frequency (varies for even or odd vector lengths).
  if (nrow(temp) %% 2) {
    cut <- (nrow(temp) + 1) / 2
  } else {
    cut <- nrow(temp) / 2 + 1
  }
  
  # Construct dataframe, `df`, with FFT info.
  df <- tibble(
    time = temp$time,
    feed_pressure_psi = temp$feed_pressure_psi,
    fft = fft(feed_pressure_psi - mean(feed_pressure_psi)), # Note centered data.
    mod = Mod(fft),
    arg = Arg(fft),
    hzMin = seq(0, 1/end[i], length = nrow(temp)) * 60
  ) %>%
    slice(1:cut) # Cut off at Nyquist frequency.
  dfs[[i]] <- df
  
  if (OUTPUT) {
    png(file = paste('../plots/fft/frequency', i,
                     '.png', sep = ''), height = 800, width = 2167.5)
  }
  
  # Plot the frequency power spectrum.
  g <- df %>%
    ggplot(aes(x = hzMin, y = 0, xend = hzMin, yend = mod)) +
    geom_segment(color = 'SteelBlue', size = 0.50) +
    ggtitle(paste('Experiment', exp.names[i])) +
    labs(subtitle = paste(exp.salt[i], ', ', exp.wave[i], sep='')) +
    xlab(expression(paste('Frequency (minutes'^-1, ')'))) +
    ylab('Magnitude, |z|') +
    theme(panel.grid.minor = element_blank()) +
    theme_minimal(base_size = 35)
  print(g)
  
  if (OUTPUT) {
    dev.off()
  }
}
rm(df, g, cut, temp)

# Print out the frequency at which maximum power occurs for each experiment.
for (i in 1:N) {
  s <- which.max(dfs[[i]]$mod)
  print(dfs[[i]] %>% slice(s))
}





###############################################################################
# This source file performs signal filtering using the FFT.
###############################################################################

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

N <- 8 # Number of experiments = 8.
OUTPUT <- TRUE # Controls plot output.
# If `DISPLAY` is true, user will be prompted to hit <enter> to view each
# plot in succession, akin to plot(lm(...)).
DISPLAY <- FALSE
# Names for each experiment.
exp.names <- c('10A', '10B', '10C', '11A', '11B', '11C', '12A', '13')



# Sanity test: feed pressure time-series plots for each experiment.
for (i in 1:N) {
  df <- data %>%
    filter(experiment == i)
  plot(df$time, df$feed_pressure_psi, type = 'l', lwd = 0.1,
       main = paste('Experiment', i))
}
rm(df)



## This function takes in a dataframe containing a specified time-series and
## performs filtering (smoothing, de-trending) using the FFT. This function
## produces several plots, the output or display of which are controlled by
## global constants `OUTPUT` and `DISPLAY`.
## Input parameters:
##  1) data: Dataframe containing, at least, a `time` variable and an associated
##           time-series.
##  2) var: The column name of the time-series variable of interest.
##  3) sp: The sampling period of the time-series data (in seconds).
##  4) name: The display name of the time-series variable (for plotting purposes).
##  5) q: 0 <= q <= 1 is the cutoff quantile for the modulus of the complex
##        output of the FFT, representing the proportion of frequencies to leave
##        unfiltered for signal re-construction.
## Output:
##  1) Lots of plots.
##  2) A dataframe containing:
##      a) The original time component of the time-series.
##      b) The original response variable.
##      c) The complex output of the FFT.
##      d) The filtered series in the frequency domain.
##      e) The reconstructed signal.
##      f) The residuals.
ts.detrend <- function(data, var, sp, name, q) {
  time <- data$time
  response <- data[[var]]
  N <- length(response)
  expno <- data$experiment[1] # Experiment number (1 - 8).
  expname <- exp.names[expno] # Experiment name.
  
  # Perform FFT (data uncentered so as to facilitate easier reconstruction).
  ft <- fft(response)
  
  # Plot time-series and frequency spectrum.
  x.axis <- seq(0, 1/sp, length = N) * 60
  ts.title <- paste('Experiment ', expname, '\n', name, ' Time Series', sep='')
  if (DISPLAY) {
    par(mfrow = c(2, 1))
    invisible(readline(prompt = 'Hit <Return> to see next plot: '))
    plot(time, response, type = 'l', main = ts.title, xlab = 'Time',
         ylab = name, lwd = 0.1, cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
    # Note the exclusion here of the 0-bin frequency (this is like the average
    # signal strength).
    plot(x.axis[-1], Mod(ft)[-1], type = 'h', main = 'Frequency Spectrum',
         xlab = expression(paste('Frequency (minutes'^-1, ')')),
         ylab = 'Magnitude, |z|', cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.5)
  }
  
  
  # Remove low-power frequencies.
  strongFreq <- ft
  strongFreq[Mod(strongFreq) < quantile(Mod(strongFreq), 1 - q)] <- 0
  
  # Construct color vector to plot the low frequencies in red (others in black).
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
  
  
  # Re-construct signal using only high-power frequencies. Note the
  # necessary normalization. We take just the real part for plotting, though
  # imaginary part should be 0.
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
  
  
  ## Output plots. The following plots are the exact plots constructed above,
  ## now for the purpose of output. Unfortunately, this duplicated code is
  ## necessary, as base R does not allow for the (easy) storage of plots as
  ## objects.
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
  }
  
  # Reset mfrow.
  par(mfrow = c(1, 1))
  
  # Construct dataframe for return.
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
# This is how we call the above function. This is generally done for
# each experiment in turn (that code is excluded).
dfs <- list()
dfs[[1]] <- data %>%
  filter(experiment == 1) %>%
  ts.detrend(
    var = 'feed_pressure_psi',
    sp = 1,
    name = 'Feed Pressure',
    q = 0.01
  )




