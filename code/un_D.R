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



loclin1 <- function(x, y, bandwidth, x0) {
  w <- dnorm(x, mean = x0, sd = bandwidth)
  a0 <- mean(w)
  a1 <- mean(w * (x - x0))
  a2 <- mean(w * (x - x0)^2)
  mean(((a2 - a1 * (x - x0)) * w * y) / (a2 * a0 - a1^2))
}

loclin <- Vectorize(loclin1, vectorize.args = "x0")

S_diag <- function(x, y, bandwidth) {
  n <- length(y)
  S_diag <- numeric(n)
  for (i in 1:n) {
    x0 <- x[i]
    w <- dnorm(x, mean = x0, sd = bandwidth)
    a0 <- mean(w)
    a1 <- mean(w * (x - x0))
    a2 <- mean(w * (x - x0)^2)
    S_diag[i] <- ((a2 - a1 * (x[i] - x0)) * w[i]) / (a2 * a0 - a1^2) / n
  }
  S_diag
}

df_loclin <- function(x, y, bandwidth) {
  sum(S_diag(x, y, bandwidth))
}



# Code execution and storage of the interesting quantities
bandwidth_list <- exp(seq(from = -1, to = 2, length = 100))
data_goodness <- data.frame(bandwidth = bandwidth_list)
for (i in 1:length(bandwidth_list)) {
  # Fitting a polynomial of degree p -1
  lev <- S_diag(x, y, bandwidth_list[i])
  fit <- loclin(x, y, bandwidth_list[i], x)
  res_loo <- (y - fit) / (1 - lev)
  data_goodness$df[i] <- sum(lev)
  data_goodness$LOO_CV[i] <- mean(res_loo^2)
  data_goodness$LOO_CV_SE[i] <- sd(res_loo) / sqrt(nrow(dataset))
  data_goodness$GCV[i] <- mean(((y - fit) / (1 - sum(lev) / nrow(dataset)))^2)
}


#| fig-width: 9
#| fig-height: 5
#| fig-align: center
#|
id_opt <- which.min(data_goodness$LOO_CV)
h_opt <- data_goodness$bandwidth[id_opt]
df_opt <- data_goodness$df[id_opt]

x_seq <- seq(from = min(x), to = max(x), length = 2000)
fit_locpoly <- loclin(x, y, bandwidth = h_opt, x0 = x_seq)

ggplot(data = dataset, aes(x = times, y = accel)) +
  geom_point(size = 0.7) +
  geom_line(data = data.frame(x = x_seq, y = fit_locpoly), aes(x = x, y = y), col = "#1170aa") +
  theme_minimal() +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")


#| fig-width: 9
#| fig-height: 5
#| fig-align: center

# Organization of the results for graphical purposes
data_bv <- data.frame(df = data_goodness$df, GCV = data_goodness$GCV, LOO_CV = data_goodness$LOO_CV, SE = data_goodness$LOO_CV_SE)
data_bv <- reshape2::melt(data_bv, id = c("df", "SE"))
data_bv$SE[data_bv$variable == "GCV"] <- NA
levels(data_bv$variable) <- c("GCV", "LOO-CV")
colnames(data_bv) <- c("df", "SE", "Error term", "value")

ggplot(data = data_bv, aes(x = df, y = value, col = `Error term`)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = df_opt, linetype = "dotted", col = "#fc7d0b") +
  theme_light() +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Effective degrees of freedom (df)") +
  ylab("Mean Squared Error (MSE)")


#| message: false
#| fig-width: 10
#| fig-height: 8
#| fig-align: center

library(sm)
auto <- read.table("../data/auto.txt", header = TRUE) %>% select(city.distance, engine.size, n.cylinders, curb.weight, fuel)

h_sm <- c(0.5, 150)
sm.options(ngrid = 50)

fit_sm <- sm.regression(cbind(auto$engine.size, auto$curb.weight), auto$city.distance,
  h = h_sm, display = "none",
  options = list(xlab = "Engine size (L)", ylab = "Curb weight (kg)", zlab = "City distance (km/L)")
)

surf.colors <- function(x, col = terrain.colors(20)) {
  # First we drop the 'borders' and average the facet corners
  # we need (nx - 1)(ny - 1) facet colours!
  x.avg <- (x[-1, -1] + x[-1, -(ncol(x) - 1)] +
    x[-(nrow(x) - 1), -1] + x[-(nrow(x) - 1), -(ncol(x) - 1)]) / 4

  # Now we construct the actual colours matrix
  colors <- col[cut(x.avg, breaks = length(col), include.lowest = T)]

  return(colors)
}

persp(fit_sm$eval.points[, 1], fit_sm$eval.points[, 2], fit_sm$estimate,
  xlab = "Engine size (L)", ylab = "Curb weight (kg)", zlab = "City distance (km/L)", cex = 0.4,
  theta = 145, phi = 20, ticktype = "detailed", col = surf.colors(fit_sm$estimate, col = terrain.colors(80)), expand = 0.8
)


#| fig-width: 6
#| fig-height: 3
#| fig-align: center

library(splines2)

knots <- c(15, 25)
fit_bs <- lm(accel ~ bsp(times, knots = knots, degree = 0, intercept = TRUE) - 1, data = dataset)
y_hat_bs <- predict(fit_bs, newdata = data.frame(times = times_seq))

ggplot(data = dataset, aes(x = times, y = accel)) +
  geom_point(size = 0.7) +
  geom_line(data = data.frame(x = times_seq, y = y_hat_bs), aes(x = x, y = y), col = "#1170aa") +
  theme_minimal() +
  geom_vline(xintercept = 15, linetype = "dotted") +
  geom_vline(xintercept = 25, linetype = "dotted") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")


#| fig-width: 9
#| fig-height: 5
#| fig-align: center

library(splines)
knots <- quantile(dataset$times, ppoints(n = 9))

dataset$time_cut <- cut(dataset$times, breaks = c(2.3, knots, 57.6))

fit_piece <- lm(accel ~ time_cut + times * time_cut + I(times^2) * time_cut, data = dataset)
y_hat_piece <- predict(fit_piece, newdata = data.frame(times = times_seq, time_cut = cut(times_seq, breaks = c(2.3, knots, 57.6))))

fit_bs <- lm(accel ~ bsp(times, knots = knots, degree = 2, intercept = TRUE) - 1, data = dataset)
y_hat_bs <- predict(fit_bs, newdata = data.frame(times = times_seq))

data_plot <- data.frame(
  times = times_seq, pred = c(y_hat_piece, y_hat_bs),
  Method = rep(c("Piecewise quadratic", "Piecewise quadratic with continuity constraints"),
    each = length(times_seq)
  )
)
ggplot(data = data_plot, aes(x = times, y = pred, col = Method)) +
  geom_line() +
  geom_point(data = dataset, aes(x = times, y = accel), size = 0.7, col = "black") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")


#| fig-width: 9
#| fig-height: 5
#| fig-align: center

knots <- quantile(dataset$times, ppoints(n = 15))

fit_bs_3 <- lm(accel ~ bs(times, knots = knots, degree = 3, intercept = TRUE) - 1, data = dataset)
y_hat_bs_3 <- predict(fit_bs_3, newdata = data.frame(times = times_seq))

fit_bs_2 <- lm(accel ~ bs(times, knots = knots, degree = 2, intercept = TRUE) - 1, data = dataset)
y_hat_bs_2 <- predict(fit_bs_2, newdata = data.frame(times = times_seq))


fit_bs_1 <- lm(accel ~ bs(times, knots = knots, degree = 1, intercept = TRUE) - 1, data = dataset)
y_hat_bs_1 <- predict(fit_bs_1, newdata = data.frame(times = times_seq))


data_plot <- data.frame(
  times = times_seq, pred = c(y_hat_bs_3, y_hat_bs_2, y_hat_bs_1),
  Method = rep(c("Regression splines (M = 4)", "Regression splines (M = 3)", "Regression splines (M = 2)"),
    each = length(times_seq)
  )
)
ggplot(data = data_plot, aes(x = times, y = pred, col = Method)) +
  geom_line() +
  geom_point(data = dataset, aes(x = times, y = accel), size = 0.7, col = "black") +
  theme_minimal() +
  theme(legend.position = "top") +
  # geom_vline(xintercept = knots, linetype = "dotted") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")


#| fig-width: 9
#| fig-height: 5
#| fig-align: center

knots <- quantile(dataset$times, ppoints(n = 15))

fit_bs_3 <- lm(accel ~ bs(times, knots = knots, degree = 3, intercept = TRUE) - 1, data = dataset)
y_hat_bs_3 <- predict(fit_bs_3, newdata = data.frame(times = times_seq))

# Here it's tricky, because by default ns does a different thing (it uses the boundary knots as regular knots)
fit_ns_3 <- lm(accel ~ ns(times, knots = knots[-c(1, 15)], Boundary.knots = c(knots[1], knots[15]), intercept = TRUE) - 1, data = dataset)
y_hat_ns_3 <- predict(fit_ns_3, newdata = data.frame(times = times_seq))

data_plot <- data.frame(
  times = times_seq, pred = c(y_hat_bs_3, y_hat_ns_3),
  Method = rep(c("Cubic regression splines", "Natural cubic splines"),
    each = length(times_seq)
  )
)
ggplot(data = data_plot, aes(x = times, y = pred, col = Method)) +
  geom_line() +
  geom_point(data = dataset, aes(x = times, y = accel), size = 0.7, col = "black") +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_vline(xintercept = knots[c(1, 15)], linetype = "dotted") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")



tpower <- function(x, t, p) {
  (x - t)^p * (x > t)
}

tbase <- function(x, knots, degree = 3) {
  B <- cbind(outer(x, 0:degree, "^"), outer(x, knots, function(x, y) pmax(x - y, 0))^degree)
  B
}

knots <- c(10, 30, 50)
data_plot <- data.frame(times_seq,
  rbind(reshape2::melt(bs(times_seq, knots = knots, degree = 3, intercept = TRUE)), reshape2::melt(tbase(times_seq, knots = knots, degree = 3))),
  Basis = rep(c("B-splines", "Truncated power basis"),
    each = length(times_seq) * (length(knots) + 4)
  )
)


ggplot(data = data_plot, aes(x = times_seq, y = value, col = as.factor(Var2))) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_grid(Basis ~ ., scales = "free_y") +
  geom_vline(xintercept = knots, linetype = "dotted") +
  scale_color_tableau(type = "ordered-diverging", palette = "Orange-Blue Diverging") +
  xlab("x") +
  ylab(expression(h[j](x)))
