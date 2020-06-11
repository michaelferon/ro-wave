rm(list = ls())

## Load libraries.
library(lubridate)
library(viridis)
library(ggplot2)
library(tidyr)
library(dplyr)

## Load data.
load('../data/data.Rdata')

## Number of experiments = 9.
N <- 8
OUTPUT <- TRUE

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
    hzMin = seq(0, 1, length = nrow(temp)) * 60
  ) %>%
    slice(1:cut)
  dfs[[i]] <- df
  
  if (OUTPUT) {
    pdf(file = paste('../plots/fft/frequency', i,
                     '.pdf', sep = ''), height = 3.5, width = 10.0)
  }
  g <- df %>%
    ggplot(aes(x = hzMin, y = 0, xend = hzMin, yend = mod)) +
    geom_segment(color = 'SteelBlue', size = 0.25) +
    ggtitle(paste('Experiment', i)) +
    xlab(expression(paste('Frequency (minutes'^-1, ')'))) +
    ylab('Magnitude, |z|') +
    theme(panel.grid.minor = element_blank()) +
    theme_minimal()
  print(g)
  
  if (OUTPUT) {
    dev.off()
  }
}
rm(df, g, cut)

for (i in 1:N) {
  s <- which.max(dfs[[i]]$mod)
  print(dfs[[i]] %>% slice(s))
}


