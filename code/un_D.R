#| fig-width: 5
#| fig-height: 4.5
#| warning: false

library(tidyverse)
library(ggplot2)
library(ggthemes)
rm(list = ls())
# The dataset can be downloaded here: https://tommasorigon.github.io/datamining/data/cholesterol.txt
dataset <- read.table("../data/cholesterol.txt", header = TRUE)
ggplot(data = dataset, aes(x = compliance, y = cholesterol.decrease)) +
  geom_point() +
  theme_minimal() +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Compliance") +
  ylab("Cholesterol Decrease")


#| fig-width: 5
#| fig-height: 4

rm(list = ls())
# The dataset can be also downloaded here: https://tommasorigon.github.io/datamining/data/auto.txt
auto <- read.table("../data/auto.txt", header = TRUE) %>% select(city.distance, engine.size, n.cylinders, curb.weight, fuel)

ggplot(data = auto, aes(x = engine.size, y = city.distance)) +
  geom_point() +
  theme_minimal() +
  xlab("Engine size (L)") +
  ylab("Urban distance (km/L)")


#| fig-width: 5
#| fig-height: 4

rm(list = ls())
dataset <- MASS::mcycle

x <- dataset$times
y <- dataset$accel

ggplot(data = dataset, aes(x = times, y = accel)) +
  geom_point() +
  theme_minimal() +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")


#| fig-width: 7.8
#| fig-height: 4.55
#| fig-align: center

# Degrees of the polynomials
degree_list <- c(9, 11, 13, 15, 17, 19)

# I am using 30.000 obs to improve the quality of the graph
times_seq <- seq(from = min(dataset$times), to = max(dataset$times), length = 30000)

# Actual fitting procedure
data_pred <- NULL
for (degree in degree_list) {
  # Fitting a polynomial of degree p - 1
  fit <- lm(accel ~ poly(times, degree = degree, raw = FALSE), data = dataset)
  # Fitted values
  y_hat <- predict(fit, newdata = data.frame(times = times_seq))
  data_pred <- rbind(data_pred, data.frame(x = times_seq, y_hat = y_hat, degree = paste("Number of parameters p:", degree + 1)))
}

# Graphical adjustment to get the plots in the right order
data_pred$degree <- factor(data_pred$degree)

# Final plot
ggplot(data = data_pred) +
  geom_line(aes(x = x, y = y_hat, col = degree)) +
  geom_point(data = dataset, aes(x = times, y = accel), size = 0.4) +
  theme_light() +
  theme(legend.position = "none") +
  facet_wrap(. ~ degree, ncol = 3) +
  scale_color_tableau(palette = "Color Blind") +
  xlab("x") +
  ylab("y") # Manual identification of an "interesting" region


#| fig-width: 7.8
#| fig-height: 4.55
#| fig-align: center
library(kknn)
fit_knn <- fitted(kknn(accel ~ times, train = dataset, test = data.frame(times = times_seq), kernel = "rectangular", k = 6))

ggplot(data = dataset, aes(x = times, y = accel)) +
  geom_ribbon(aes(xmin = 51.7, xmax = 57.7), fill = "#fc7d0b", alpha = 0.6) +
  geom_ribbon(aes(xmin = 19, xmax = 21), fill = "#fc7d0b", alpha = 0.6) +
  geom_point(size = 0.7) +
  geom_line(data = data.frame(x = times_seq, y = fit_knn), aes(x = x, y = y), col = "#1170aa") +
  geom_vline(xintercept = 20, lty = "dashed", linewidth = 0.4) +
  geom_vline(xintercept = 54.7, lty = "dashed", linewidth = 0.4) +
  theme_minimal() +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")


#| fig-width: 7.8
#| fig-height: 4
#| fig-align: center
#| message: false
library(KernSmooth)

h_param <- 1
band <- 4 * qnorm(0.75) * h_param # Bandwidth as parametrized in ksmooth
fit_nw <- ksmooth(x, y, kernel = "normal", bandwidth = band, x.points = times_seq)

ggplot(data = dataset, aes(x = times, y = accel)) +
  geom_ribbon(aes(xmin = qnorm(0.05, 20, sd = h_param), xmax = qnorm(0.95, 20, sd = h_param)), fill = "#fc7d0b", alpha = 0.6) +
  geom_point(size = 0.7) +
  geom_line(data = data.frame(x = fit_nw$x, y = fit_nw$y), aes(x = x, y = y), col = "#1170aa") +
  geom_vline(xintercept = 20, lty = "dashed", linewidth = 0.4) +
  theme_minimal() +
  geom_function(fun = function(x) 150 * dnorm(x, 20, h_param) - 134, linetype = "dotted", n = 500, xlim = c(qnorm(0.001, 20, sd = h_param), xmax = qnorm(0.999, 20, sd = h_param))) +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")


#| fig-width: 7.8
#| fig-height: 4
#| fig-align: center

h_param <- 0.3
band <- 4 * qnorm(0.75) * h_param # Bandwidth as parametrized in ksmooth
fit_nw <- ksmooth(x, y, kernel = "normal", bandwidth = band, x.points = times_seq)

ggplot(data = dataset, aes(x = times, y = accel)) +
  geom_ribbon(aes(xmin = qnorm(0.05, 20, sd = h_param), xmax = qnorm(0.95, 20, sd = h_param)), fill = "#fc7d0b", alpha = 0.6) +
  geom_point(size = 0.7) +
  geom_line(data = data.frame(x = fit_nw$x, y = fit_nw$y), aes(x = x, y = y), col = "#1170aa") +
  geom_vline(xintercept = 20, lty = "dashed", linewidth = 0.4) +
  theme_minimal() +
  geom_function(fun = function(x) 50 * dnorm(x, 20, h_param) - 134, linetype = "dotted", n = 500, xlim = c(qnorm(0.001, 20, sd = h_param), xmax = qnorm(0.999, 20, sd = h_param))) +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")


#| fig-width: 7.8
#| fig-height: 4
#| fig-align: center
h_param <- 2
band <- 4 * qnorm(0.75) * h_param # Bandwidth as parametrized in ksmooth
fit_nw <- ksmooth(x, y, kernel = "normal", bandwidth = band, x.points = times_seq)

ggplot(data = dataset, aes(x = times, y = accel)) +
  geom_ribbon(aes(xmin = qnorm(0.05, 20, sd = h_param), xmax = qnorm(0.95, 20, sd = h_param)), fill = "#fc7d0b", alpha = 0.6) +
  geom_point(size = 0.7) +
  geom_line(data = data.frame(x = fit_nw$x, y = fit_nw$y), aes(x = x, y = y), col = "#1170aa") +
  geom_vline(xintercept = 20, lty = "dashed", linewidth = 0.4) +
  theme_minimal() +
  geom_function(fun = function(x) 180 * dnorm(x, 20, h_param) - 134, linetype = "dotted", n = 500, xlim = c(qnorm(0.001, 20, sd = h_param), xmax = qnorm(0.999, 20, sd = h_param))) +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")


#| fig-width: 7.8
#| fig-height: 4
#| fig-align: center
h_param <- 4
band <- 4 * qnorm(0.75) * h_param # Bandwidth as parametrized in ksmooth
fit_nw <- ksmooth(x, y, kernel = "normal", bandwidth = band, x.points = times_seq)

ggplot(data = dataset, aes(x = times, y = accel)) +
  geom_ribbon(aes(xmin = qnorm(0.05, 20, sd = h_param), xmax = qnorm(0.95, 20, sd = h_param)), fill = "#fc7d0b", alpha = 0.6) +
  geom_point(size = 0.7) +
  geom_line(data = data.frame(x = fit_nw$x, y = fit_nw$y), aes(x = x, y = y), col = "#1170aa") +
  geom_vline(xintercept = 20, lty = "dashed", linewidth = 0.4) +
  theme_minimal() +
  geom_function(fun = function(x) 600 * dnorm(x, 20, h_param) - 134, linetype = "dotted", n = 500, xlim = c(qnorm(0.001, 20, sd = h_param), xmax = qnorm(0.999, 20, sd = h_param))) +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")
