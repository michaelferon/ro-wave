rm(list = ls())

## Load libraries.
library(lubridate)
library(viridis)
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(boot)
library(Hmisc)

## Load data.
load('../data/data.Rdata')

## Number of experiments = 8.
N <- 8
OUTPUT <- FALSE




## Experiments summaries.
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




# Bootstrap for confidence interval for median.
median_cl_boot <- function(x, conf = 0.95) {
  lconf <- (1 - conf)/2
  uconf <- 1 - lconf
  bmedian <- function(x, ind) median(x[ind])
  bt <- boot(x, bmedian, 1000)
  bb <- boot.ci(bt, type = 'perc')
  data.frame(y = median(x), ymin = quantile(bt$t, lconf), ymax = quantile(bt$t, 
                                                                          uconf))
}

# Boxplots by experiment with confidence intervals for median.
data %>%
  ggplot(aes(experiment, water_perm_coef)) +
  geom_boxplot(fill = 'grey', color = 'black', notch = TRUE) +
  theme_minimal() +
  stat_summary(fun.data = median_cl_boot, geom = 'errorbar', color = 'red') +
  stat_summary(fun.y = median, geom = 'point', color = 'red')
  




