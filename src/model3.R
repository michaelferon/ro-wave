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
N <- 8
exp.names <- c('10A', '10B', '10C', '11A', '11B', '11C', '12A', '13')
exp.salt <- c('35', '35', '35', 'IO', 'IO', 'IO', '1.5IO', 'IO')
exp.wave <- c('7.5', '1.25', '12', '7.5', '1.25', '12', '7.5', 'NREL')


data$salt <- as.character(NA)
data$wave <- as.character(NA)
for (i in 1:N) {
  data$salt[data$experiment == i] <- exp.salt[i]
  data$wave[data$experiment == i] <- exp.wave[i]
}
data <- data %>%
  mutate_at(c('salt', 'wave'), as.factor)


# Split data into training set.
train <- data %>%
  group_by(experiment) %>%
  slice( 1 : (round(0.75*n())) ) %>%
  ungroup

# Split data into testing set.
test <- data %>%
  group_by(experiment) %>%
  slice( (round(0.75*n()) + 1) : n() ) %>%
  ungroup

# Fit linear model to training set.
model <- train %>%
  select(-time, -experiment, -perm_cond_high_us, -perm_flow_lm, -rej_cond_ms,
         -rej_flow_lm, -rej_valve_open, -mode, -status, -feed_temp_c) %>%
  lm(perm_cond_low_us ~ ., data = .)
png(file = '../plots/model/acf.png', height = 400, width = 850)
acf(resid(model), main = 'Residuals for Training Set', ylim = c(-0.1, 1.0))
dev.off()


# Calculate RMSE.
sqrt(sum(resid(model)^2) / nobs(model))


# Fitting our model to the testing set.
fit <- test %>%
  select(-time, -experiment, -perm_cond_high_us, -perm_flow_lm, -rej_cond_ms,
         -rej_flow_lm, -rej_valve_open, -mode, -status, -feed_temp_c) %>%
  predict(model, .)

sqrt(sum((test$perm_cond_low_us - fit)^2) / length(fit))

for (i in 1:N) {
  response <- test$perm_cond_low_us[test$experiment == i]
  fitted <- fit[test$experiment == i]
  res <- response - fitted
  png(file = paste('../plots/model/acf', i, '.png', sep=''),
      height = 675, width = 800)
  par(mfrow = c(2, 1))
  acf(response, main = paste('Permeate Conductivity ACF for Experiment',
                             exp.names[i]), ylim = c(-0.2, 1.0))
  acf(res, main = 'Residual ACF', ylim = c(-0.2, 1.0))
  dev.off()
}

png(file = '../plots/model/thing.png', height = 675, width = 800)

acf(test$perm_cond_low_us[test$experiment == 8])
dev.off()

df <- tibble(
  response = test$perm_cond_low_us,
  fitted = fit,
  resid = response - fitted,
  experiment = test$experiment
)
levels(df$experiment) <- exp.names

png(file = '../plots/model/test.png', height = 350, width = 745)
df %>%
  ggplot(aes(fitted, resid, color = experiment)) +
  geom_point(size = 0.1) +
  geom_hline(size = 0.5, yintercept = 0, color = 'black', linetype = 'dashed') +
  ggtitle('Linear Model Residual Plot') + xlab('Fitted Values') + ylab('Residuals') +
  labs(subtitle = 'color-coded by experiment', color = 'Experiment') +
  theme_minimal() +
  scale_color_manual(values = magma(8)) +
  guides(colour = guide_legend(override.aes = list(size = 3)))
dev.off()

png(file = '../plots/model/test2.png', height = 350, width = 745)
df %>%
  ggplot(aes(response, fitted, color = experiment)) +
  geom_point(size = 0.1) +
  geom_abline(size = 0.5, slope = 1, intercept = 0, color = 'black',
              linetype = 'dashed') +
  ggtitle('Linear Model Fitted vs. Actual Values') + xlab('Actual Values') + ylab('Fitted Values') +
  labs(subtitle = 'color-coded by experiment', color = 'Experiment') +
  theme_minimal() +
  scale_color_manual(values = magma(8)) +
  guides(colour = guide_legend(override.aes = list(size = 3)))
dev.off()


models <- list()
for (i in 1:N) {
  mod <- data %>%
    filter(experiment == i) %>%
    select(-time, -experiment, -perm_cond_high_us, -perm_flow_lm, -rej_cond_ms,
           -rej_flow_lm, -rej_valve_open, -mode, -status, -feed_temp_c, -salt,
           -wave) %>%
    lm(perm_cond_low_us ~ ., data = .)
  models[[i]] <- mod
}
models.coef <- data.frame(
  intercept = numeric(),
  water_perm_coef = numeric(),
  water_flux_lmh = numeric(),
  feed_pressure_psi = numeric(),
  feed_pump_pow = numeric(),
  feed_volume_l = numeric(),
  feed_flow_lm = numeric(),
  ac_current_a = numeric()
)

for (i in 1:N) {
  models.coef[i, ] <- coef(models[[i]])
}

models.coef$experiment <- exp.names
models.coef <- models.coef %>%
  as_tibble %>%
  select(experiment, everything())
for (i in 1:N) {
  print(summary(models[[i]])$r.squared)
}






model2 <- train %>%
  select(-time, -experiment, -perm_cond_high_us, -perm_flow_lm, -rej_cond_ms,
         -rej_flow_lm, -rej_valve_open, -mode, -status, -feed_temp_c) %>%
  # select(perm_cond_low_us, salt, wave, feed_pressure_psi) %>%
  lm(perm_cond_low_us ~ ., data = .)

# Calculate RMSE.
sqrt(sum(resid(model2)^2) / nobs(model2))


# Fitting our model to the testing set.
fit2 <- test %>%
  select(-time, -experiment, -perm_cond_high_us, -perm_flow_lm, -rej_cond_ms,
         -rej_flow_lm, -rej_valve_open, -mode, -status, -feed_temp_c) %>%
  predict(model2, .)

sqrt(sum((test$perm_cond_low_us - fit2)^2) / length(fit2))



df2 <- tibble(
  response = test$perm_cond_low_us,
  fitted = fit2,
  resid = response - fitted,
  experiment = test$experiment
)
levels(df2$experiment) <- exp.names


df2 %>%
  ggplot(aes(response, fitted, color = experiment)) +
  geom_point(size = 0.1) +
  geom_abline(size = 0.5, slope = 1, intercept = 0, color = 'black',
              linetype = 'dashed') +
  ggtitle('Linear Model Fitted vs. Actual Values') + xlab('Actual Values') + ylab('Fitted Values') +
  labs(subtitle = 'color-coded by experiment', color = 'Experiment') +
  theme_minimal() +
  scale_color_manual(values = magma(8)) +
  guides(colour = guide_legend(override.aes = list(size = 3)))



