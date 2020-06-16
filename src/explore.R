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





