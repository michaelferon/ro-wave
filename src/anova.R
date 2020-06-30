rm(list = ls())

## Load libraries.
library(lubridate)
library(viridis)
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(RColorBrewer)
library(CombMSC)

## Load data.
load('../data/data.Rdata')
N <- 8
exp.names <- c('10A', '10B', '10C', '11A', '11B', '11C', '12A', '13')
exp.wave <- c('7.5', '1.25', '12', '7.5', '1.25', '12', '7.5', 'NREL')
exp.salt <- c('35 g/L', '35 g/L', '35 g/L', 'IO', 'IO', 'IO', '1.5 IO', 'IO')


# for (i in 1:N) {
#   df <- data %>%
#     filter(experiment == i)
#   acf(df$perm_cond_low_us, lag.max = 300,
#       main = paste('Experiment', exp.names[i]))
# }
# 
# 
# 
# data.mean <- data %>%
#   select(-time, -mode, -status) %>%
#   group_by(experiment) %>%
#   summarise_all(mean, na.rm = TRUE)
# 
# data.mean$exp <- as.factor(exp.names)
# data.mean$wave <- as.factor(exp.wave)
# data.mean$salt <- as.factor(exp.salt)
#   
# data.mean <- data.mean %>%
#   select(-experiment) %>%
#   select(experiment = exp, wave, salt, everything())
# 
# png(file = '../plots/model/perm_cond_wave.png', height = 600, width = 800)
# data.mean %>%
#   ggplot(aes(experiment, perm_cond_low_us, color = wave)) +
#   geom_point(size = 8.5, shape = 15) +
#   scale_color_manual(values = brewer.pal(n = 4, name = 'PuOr')) +
#   guides(colour = guide_legend(override.aes = list(size = 4.5))) +
#   ggtitle('Mean Permeate Conductivity by Experiment') +
#   labs(subtitle = 'color-coded by wave type', color = 'Wave Type') +
#   xlab('Experiment') + ylab('Permeate Conductivity (uS)') +
#   theme_minimal(base_size = 20)
# dev.off()
# 
# 
# png(file = '../plots/model/perm_cond_salt.png', height = 600, width = 800)
# data.mean %>%
#   ggplot(aes(experiment, perm_cond_low_us, color = salt)) +
#   geom_point(size = 7.5, shape = 15) +
#   scale_color_manual(values = viridis(4)) +
#   guides(colour = guide_legend(override.aes = list(size = 4.5))) +
#   ggtitle('Mean Permeate Conductivity by Experiment') +
#   labs(subtitle = 'color-coded by salt content', color = 'Salt Content') +
#   xlab('Experiment') + ylab('Permeate Conductivity (uS)') +
#   theme_minimal(base_size = 20)
# dev.off()






S <- 25
set.seed(1)
df <- data %>%
  group_by(experiment) %>%
  slice( round(0.5*n()) : n() ) %>%
  ungroup
sds <- tapply(df$perm_cond_low_us, df$experiment, sd)

df <- df %>%
  group_by(experiment) %>%
  sample_n(S) %>%
  ungroup

df.mean <- df %>%
  select(-time, -mode, -status) %>%
  group_by(experiment) %>%
  summarise_all(mean, na.rm = TRUE)

df.mean$exp <- as.factor(exp.names)
df.mean$wave <- as.factor(exp.wave)
df.mean$salt <- as.factor(exp.salt)

df.mean$wave <- factor(df.mean$wave, levels = c('NREL', '1.25', '7.5', '12'))
df.mean$salt <- factor(df.mean$salt, levels = c('35 g/L', 'IO', '1.5 IO'))

df.mean <- df.mean %>%
  select(-experiment) %>%
  select(experiment = exp, wave, salt, perm_cond_low_us) %>%
  as.data.frame
df.mean$low <- as.numeric(NA)
df.mean$high <- as.numeric(NA)
z <- qt(1 - 0.05/2, df = S)
df.mean$low <- df.mean$perm_cond_low_us - z*sds/sqrt(S)
df.mean$high <- df.mean$perm_cond_low_us + z*sds/sqrt(S)

png(file = '../plots/model/perm_cond_wave2.png', height = 600, width = 800)
df.mean %>%
  ggplot(aes(experiment, perm_cond_low_us, color = wave)) +
  geom_point(size = 3.5) +
  geom_errorbar(aes(ymin = low, ymax = high)) +
  scale_color_manual(values = brewer.pal(n = 4, name = 'PuOr')) +
  guides(colour = guide_legend(override.aes = list(size = 4.5))) +
  ggtitle('Mean Permeate Conductivity by Experiment') +
  labs(subtitle = 'color-coded by wave type', color = 'Wave Type') +
  xlab('Experiment') + ylab('Permeate Conductivity (uS)') +
  ylim(348, 778) +
  theme_minimal(base_size = 20)
dev.off()

png(file = '../plots/model/perm_cond_salt2.png', height = 600, width = 800)
df.mean %>%
  ggplot(aes(experiment, perm_cond_low_us, color = salt)) +
  geom_point(size = 6.5) +
  scale_color_manual(values = viridis(4)) +
  guides(colour = guide_legend(override.aes = list(size = 4.5))) +
  ggtitle('Mean Permeate Conductivity by Experiment') +
  labs(subtitle = 'color-coded by salt content', color = 'Salt Content') +
  xlab('Experiment') + ylab('Permeate Conductivity (uS)') +
  ylim(348, 778) +
  theme_minimal(base_size = 20)
dev.off()




# tests <- list()
# sets <- subsets(8, 2)
# for (i in 1:nrow(sets)) {
#   tests[[i]] <- df %>%
#     filter(experiment == sets[i, 1] | experiment == sets[i, 2]) %>%
#     t.test(formula = perm_cond_low_us ~ experiment, data = .)
# }





data <- data %>%
  mutate(percent_rem = 1000*rej_cond_ms / (1000*rej_cond_ms + perm_cond_low_us)) %>%
  select(time, experiment, feed_pressure_psi, perm_cond_low_us, percent_rem, everything())

S <- 25
set.seed(1)
df <- data %>%
  group_by(experiment) %>%
  slice( round(0.5*n()) : n() ) %>%
  ungroup
sds <- tapply(df$percent_rem, df$experiment, sd)

df <- df %>%
  group_by(experiment) %>%
  sample_n(S) %>%
  ungroup

df.mean <- df %>%
  select(-time, -mode, -status) %>%
  group_by(experiment) %>%
  summarise_all(mean, na.rm = TRUE)

df.mean$exp <- as.factor(exp.names)
df.mean$wave <- as.factor(exp.wave)
df.mean$salt <- as.factor(exp.salt)

df.mean$wave <- factor(df.mean$wave, levels = c('NREL', '1.25', '7.5', '12'))
df.mean$salt <- factor(df.mean$salt, levels = c('35 g/L', 'IO', '1.5 IO'))

df.mean <- df.mean %>%
  select(-experiment) %>%
  select(experiment = exp, wave, salt, percent_rem) %>%
  as.data.frame
df.mean$low <- as.numeric(NA)
df.mean$high <- as.numeric(NA)
z <- qt(1 - 0.05/2, df = S)
df.mean$low <- df.mean$percent_rem - z*sds/sqrt(S)
df.mean$high <- df.mean$percent_rem + z*sds/sqrt(S)

png(file = '../plots/model/perm_cond_wave3.png', height = 600, width = 800)
df.mean %>%
  ggplot(aes(experiment, percent_rem, color = wave)) +
  geom_point(size = 3.5) +
  geom_errorbar(aes(ymin = low, ymax = high)) +
  scale_color_manual(values = brewer.pal(n = 4, name = 'PuOr')) +
  guides(colour = guide_legend(override.aes = list(size = 4.5))) +
  ggtitle('Mean Percent Salt Removal by Experiment') +
  labs(subtitle = 'color-coded by wave type', color = 'Wave Type') +
  xlab('Experiment') + ylab('Salt Removal (%)') +
  ylim(min(df.mean$low), max(df.mean$high)) +
  theme_minimal(base_size = 20)
dev.off()


png(file = '../plots/model/perm_cond_salt3.png', height = 600, width = 800)
df.mean %>%
  ggplot(aes(experiment, percent_rem, color = salt)) +
  geom_point(size = 6.5) +
  scale_color_manual(values = viridis(4)) +
  guides(colour = guide_legend(override.aes = list(size = 4.5))) +
  ggtitle('Mean Percent Salt Removal by Experiment') +
  labs(subtitle = 'color-coded by salt content', color = 'Salt Content') +
  xlab('Experiment') + ylab('Permeate Conductivity (uS)') +
  xlab('Experiment') + ylab('Salt Removal (%)') +
  theme_minimal(base_size = 20)
dev.off()


# acf(data$perm_cond_low_us[data$experiment == 1], data$rej_cond_ms[data$experiment == 1])
# temp <- data %>%
#   filter(experiment == 1) %>%
#   mutate(new = 1000*rej_cond_ms / (1000*rej_cond_ms + perm_cond_low_us)) %>%
#   select(time, experiment, perm_cond_low_us, new, everything())






