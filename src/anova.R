rm(list = ls())

## Load libraries.
library(lubridate)
library(viridis)
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(RColorBrewer)

## Load data.
load('../data/data.Rdata')
N <- 8
exp.names <- c('10A', '10B', '10C', '11A', '11B', '11C', '12A', '13')
exp.wave <- c('7.5', '1.25', '12', '7.5', '1.25', '12', '7.5', 'NREL')
exp.salt <- c('35 g/L', '35 g/L', '35 g/L', 'IO', 'IO', 'IO', '1.5 IO', 'IO')


for (i in 1:N) {
  df <- data %>%
    filter(experiment == i)
  acf(df$perm_cond_low_us, lag.max = 300,
      main = paste('Experiment', exp.names[i]))
}



data.mean <- data %>%
  select(-time, -mode, -status) %>%
  group_by(experiment) %>%
  summarise_all(mean, na.rm = TRUE)

data.mean$exp <- as.factor(exp.names)
data.mean$wave <- as.factor(exp.wave)
data.mean$salt <- as.factor(exp.salt)
  
data.mean <- data.mean %>%
  select(-experiment) %>%
  select(experiment = exp, wave, salt, everything())

png(file = '../plots/model/perm_cond_wave.png', height = 600, width = 800)
data.mean %>%
  ggplot(aes(experiment, perm_cond_low_us, color = wave)) +
  geom_point(size = 8.5, shape = 15) +
  scale_color_manual(values = brewer.pal(n = 4, name = 'PuOr')) +
  guides(colour = guide_legend(override.aes = list(size = 4.5))) +
  ggtitle('Mean Permeate Conductivity by Experiment') +
  labs(subtitle = 'color-coded by wave type', color = 'Wave Type') +
  xlab('Experiment') + ylab('Permeate Conductivity (uS)') +
  theme_minimal(base_size = 20)
dev.off()


png(file = '../plots/model/perm_cond_salt.png', height = 600, width = 800)
data.mean %>%
  ggplot(aes(experiment, perm_cond_low_us, color = salt)) +
  geom_point(size = 7.5, shape = 15) +
  scale_color_manual(values = viridis(4)) +
  guides(colour = guide_legend(override.aes = list(size = 4.5))) +
  ggtitle('Mean Permeate Conductivity by Experiment') +
  labs(subtitle = 'color-coded by salt content', color = 'Salt Content') +
  xlab('Experiment') + ylab('Permeate Conductivity (uS)') +
  theme_minimal(base_size = 20)
dev.off()


df <- data %>%
  group_by(experiment) %>%
  slice( round(0.5*n()) : n() ) %>%
  sample_n(100) %>%
  ungroup

tapply(df$perm_cond_low_us, df$experiment, mean)





