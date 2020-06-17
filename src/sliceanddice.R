rm(list= ls())

# Load Libraries
library(tidyverse)
library(ggplot2)
library(rstatix)
library(emmeans)

# Load Data
load('../data/data.Rdata')

#Cutting outliers

## Experiment 5 
data %>%
  slice(350021:422668) %>%
  ggplot(aes(x = time, y = feed_pressure_psi, color = mode)) + 
  geom_line() + 
  theme_bw() + 
  ggtitle('Experiment 5') +
  labs(x = "Time", y = "Pressure")


### Auto Water Flux CUT
test <- data%>%
  filter(experiment == 5) %>%
  slice(2969:75655)

auto_water_vals <- which(test$mode == "SYSTEM MODE: Auto Water Flux (Fixed)")
which.max(diff(auto_water_vals))
auto_water_vals[2965:2975]

test %>%
  ggplot(aes(x = time, y = feed_pressure_psi, color = mode)) + 
  geom_line() + 
  theme_bw() + 
  ggtitle('Experiment 5') +
  labs(x = "Time", y = "Pressure")



which(data$time == test$time[1])
which(data$time == test$time[72648])

### Manual CUT
which(test$mode == "SYSTEM MODE: Manual")
test <- test %>%
  slice(23:72670)

nrow(test)


## Experiment 6
data %>%
  filter(experiment == 6) %>%
  ggplot(aes(x = time, y = feed_pressure_psi, color = mode)) + 
  geom_line() + 
  theme_bw() + 
  ggtitle('Experiment 6') +
  labs(x = "Time", y = "Pressure")

data %>%
  filter(experiment == 7) %>%
  ggplot(aes(x = time, y = feed_pressure_psi, color = mode)) + 
  geom_line() + 
  theme_bw() + 
  ggtitle('Experiment 7') +
  labs(x = "Time", y = "Pressure")

data %>%
  filter(experiment == 8) %>%
  ggplot(aes(x = time, y = feed_pressure_psi, color = mode)) + 
  geom_line() + 
  theme_bw() + 
  ggtitle('Experiment 8') +
  labs(x = "Time", y = "Pressure")





# Test of for loop
N <- 8

for (i = 1:N){
  data %>%
    filter(experiment == i) %>%
    ggplot(aes(x = time, y = feed_pressure_psi, color = mode)) + 
    geom_line() + 
    theme_bw() + 
    ggtitle('Experiment', i) +
    labs(x = "Time", y = "Pressure")
}
