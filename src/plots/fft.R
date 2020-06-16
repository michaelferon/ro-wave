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
OUTPUT <- FALSE




exp.names <- c('10A', '10B', '10C', '11A', '11B', '11C', '12A', '13')
exp.salt <- c('35 g/L NaCl', '35 g/L NaCl', '35 g/L NaCl', 'Instant Ocean',
              'Instant Ocean', 'Instant Ocean', '1.5x Instant Ocean',
              'Instant Ocean')
exp.wave <- c('7.5 wave/min', '1.25 wave/min', '12 wave/min', '7.5 wave/min',
              '1.25 wave/min', '12 wave/min', '7.5 wave/min', 'NREL wave')
end <- c(rep(1, 6), 20, 15)

dfs <- list()
for (i in 1:N) {
  temp <- data %>%
    filter(experiment == i)
  if (nrow(temp) %% 2) {
    cut <- (nrow(temp) + 1) / 2
  } else {
    cut <- nrow(temp) / 2 + 1
  }
  
  df <- tibble(
    time = temp$time,
    feed_pressure_psi = temp$feed_pressure_psi,
    fft = fft(feed_pressure_psi - mean(feed_pressure_psi)),
    mod = Mod(fft),
    arg = Arg(fft),
    hzMin = seq(0, 1/end[i], length = nrow(temp)) * 60
  ) %>%
    slice(1:cut)
  dfs[[i]] <- df
  
  if (OUTPUT) {
    # pdf(file = paste('../plots/fft/frequency', i,
    #                  '.pdf', sep = ''), height = 3.5, width = 10.0)
    png(file = paste('../plots/fft/frequency', i,
                     '.png', sep = ''), height = 800, width = 2167.5)
  }
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

for (i in 1:N) {
  s <- which.max(dfs[[i]]$mod)
  print(dfs[[i]] %>% slice(s))
}


