rm(list= ls())

# Load Libraries
library(tidyverse)
library(ggplot2)
library(rstatix)
library(emmeans)

# Load Data
load('../data/data.Rdata')

#Cutting outliers

data <- data %>%
  slice(-558746)

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

#Initial Graph
data %>%
  filter(experiment == 6) %>%
  ggplot(aes(x = time, y = feed_pressure_psi, color = mode)) + 
  geom_line() + 
  theme_bw() + 
  ggtitle('Experiment 6') +
  labs(x = "Time", y = "Pressure")

#Creating tibble that only contains measurements from experiment 6
test <- data %>%
  filter(experiment == 6)

#Finding the rows that contain specific mode
auto_water_vals <- which(test$mode == "SYSTEM MODE: Auto Water Flux (Fixed)")
manual_vals <- which(test$mode == "SYSTEM MODE: Manual")

#Finding the the rows 
which.max(diff(auto_water_vals))
auto_water_vals[5465:5470]
manual_vals
tail(manual_vals)
tail(auto_water_vals)
#Just find the manual values and added one to the value before the break and subtracted one from the value before the break

#Graphing the plot with slice
test %>%
  slice(5558:90965) %>%
  ggplot(aes(x = time, y = feed_pressure_psi, color = mode)) + 
  geom_line() + 
  theme_bw() +
  ggtitle('Experiment 6') + 
  labs(x = "Time", y = "Pres")


#Set test to the parameters
test <- test %>%
  slice(5558:90965)

#Finding the rows in the "data" file
which(data$time == test$time[1])
which(data$time == test$time[nrow(test)])

#Format data and create new graph
data %>%
  slice(432059:517466) %>%
  ggplot(aes(x = time, y = feed_pressure_psi, color = mode)) + 
  geom_line() + 
  theme_bw() + 
  ggtitle('Experiment 6') +
  labs(x = "Time", y = "Pressure")



## Experiment 7
data %>%
  filter(experiment == 7) %>%
  ggplot(aes(x = time, y = feed_pressure_psi, color = mode)) + 
  geom_line() + 
  theme_bw() + 
  ggtitle('Experiment 7') +
  labs(x = "Time", y = "Pressure")

test <- data %>%
  filter(experiment == 7)

test %>%
  ggplot(aes(x = time, y = feed_pressure_psi, color = mode)) +
  geom_line() + 
  theme_bw()

View(test)
auto_water_vals <- which(test$mode == "SYSTEM MODE: Auto Water Flux (Fixed)")
manual_vals <- which(test$mode == "SYSTEM MODE: Manual")

test %>%
  slice(235:nrow(test)) %>%
  ggplot(aes(x = time, y = feed_pressure_psi, color = mode)) + 
  geom_line() + 
  theme_bw()

which(data$time == test$time[235])
which(data$time == test$time[36045])

data %>%
  slice(522937:558747) %>%
  ggplot(aes(x = time, y = feed_pressure_psi, color = mode)) + 
  geom_line() + 
  theme_bw() + 
  ggtitle('Experiment 7') +
  labs(x = "Time", y = "Pressure")

#Duplicate data!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
huh <- data %>%
  slice(558746:558747) 
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


## Experiment 8
data %>%
  filter(experiment == 8) %>%
  ggplot(aes(x = time, y = feed_pressure_psi, color = mode)) + 
  geom_line() + 
  theme_bw() + 
  ggtitle('Experiment 8') +
  labs(x = "Time", y = "Pressure")


test <- data %>%
  filter(experiment == 8)

manual_vals <- which(test$mode == "SYSTEM MODE: Manual") 

test %>%
  slice(624:79433) %>%
  ggplot(aes(x = time, y = feed_pressure_psi, color = mode)) + 
  geom_line() + 
  theme_bw() + 
  ggtitle('Experiment 8') +
  labs(x = "Time", y = "Pressure")


which(data$time == test$time[624])
which(data$time == test$time[79433])


data %>%
  slice(559370:638179) %>%
  ggplot(aes(x = time, y = feed_pressure_psi, color = mode)) + 
  geom_line() + 
  theme_bw() + 
  ggtitle('Experiment 8') +
  labs(x = "Time", y = "Pressure")
  



# Test of for loop
N <- 8

for (i in 1:N){
  data %>%
    filter(experiment == i) %>%
    ggplot(aes(x = time, y = feed_pressure_psi, color = mode)) + 
    geom_line() + 
    theme_bw() + 
    ggtitle('Experiment', i) +
    labs(x = "Time", y = "Pressure")
}
