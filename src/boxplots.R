rm(list = ls())

#Load Libraries
library(tidyverse)
library(ggplot2)

#Load Data
load("../data/data.Rdata")

#Modifiable Variables
OUTPUT <- TRUE
N <- 8

#Variables
exp_names <- c("10A", "10B", "10C", "11A", "11B", "11C", "12A", "13")
data$dm = as.character(NA)

#For loop that splits the experiments in half and creates another column
for(i in 1:N){
  var <- length(data$experiment[data$experiment == i])
  n = round(var/2)
  data$dm[data$experiment == i][1:n] = paste(i, "A", sep = '')
  data$dm[data$experiment == i][(n + 1):var] = paste(i, "B", sep = '')
}

data$dm = as.factor(data$dm)



if(OUTPUT){
  png(file = paste('../plots/boxplot/boxplots.png', sep = ''),
      height = 800, width = 2167.5)
}

data %>%
  ggplot(aes(x = dm, y = perm_cond_low_us)) +
  geom_boxplot(aes(fill = experiment)) +
  ggtitle("") +
  ylab("Permeate Conductivity (us)") +
  xlab("Experiment") +
  scale_fill_discrete(name = "Experiment", labels = exp_names) +
  theme_minimal() 

if(OUTPUT){
  dev.off()
}
