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

rm(list = ls())
# The dataset can also be downloaded here: https://tommasorigon.github.io/datamining/data/auto.txt
auto <- read.table("../data/auto.txt", header = TRUE) %>% select(city.distance, engine.size, n.cylinders, curb.weight, fuel)

ggplot(data = auto, aes(x = engine.size, y = city.distance)) +
  geom_point() +
  theme_minimal() +
  xlab("Engine size (L)") +
  ylab("Urban distance (km/L)")

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

library(kknn)
fit_knn <- fitted(kknn(accel ~ times, train = dataset, test = data.frame(times = times_seq), kernel = "rectangular", k = 6))

ggplot(data = dataset, aes(x = times, y = accel)) +
  geom_ribbon(aes(xmin = 51.7, xmax = 57.7), fill = "#FF8C00", alpha = 0.6) +
  geom_ribbon(aes(xmin = 19, xmax = 21), fill = "#FF8C00", alpha = 0.6) +
  geom_point(size = 0.7) +
  geom_line(data = data.frame(x = times_seq, y = fit_knn), aes(x = x, y = y), col = "#3232AA") +
  geom_vline(xintercept = 20, lty = "dashed", linewidth = 0.4) +
  geom_vline(xintercept = 54.7, lty = "dashed", linewidth = 0.4) +
  theme_minimal() +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")

library(KernSmooth)

h_param <- 1
band <- 4 * qnorm(0.75) * h_param # Bandwidth as parametrized in ksmooth
fit_nw <- ksmooth(x, y, kernel = "normal", bandwidth = band, x.points = times_seq)

ggplot(data = dataset, aes(x = times, y = accel)) +
  geom_ribbon(aes(xmin = qnorm(0.05, 20, sd = h_param), xmax = qnorm(0.95, 20, sd = h_param)), fill = "#FF8C00", alpha = 0.6) +
  geom_point(size = 0.7) +
  geom_line(data = data.frame(x = fit_nw$x, y = fit_nw$y), aes(x = x, y = y), col = "#3232AA") +
  geom_vline(xintercept = 20, lty = "dashed", linewidth = 0.4) +
  theme_minimal() +
  geom_function(fun = function(x) 150 * dnorm(x, 20, h_param) - 134, linetype = "dotted", n = 500, xlim = c(qnorm(0.001, 20, sd = h_param), xmax = qnorm(0.999, 20, sd = h_param))) +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")

h_param <- 0.3
band <- 4 * qnorm(0.75) * h_param # Bandwidth as parametrized in ksmooth
fit_nw <- ksmooth(x, y, kernel = "normal", bandwidth = band, x.points = times_seq)

ggplot(data = dataset, aes(x = times, y = accel)) +
  geom_ribbon(aes(xmin = qnorm(0.05, 20, sd = h_param), xmax = qnorm(0.95, 20, sd = h_param)), fill = "#FF8C00", alpha = 0.6) +
  geom_point(size = 0.7) +
  geom_line(data = data.frame(x = fit_nw$x, y = fit_nw$y), aes(x = x, y = y), col = "#3232AA") +
  geom_vline(xintercept = 20, lty = "dashed", linewidth = 0.4) +
  theme_minimal() +
  geom_function(fun = function(x) 50 * dnorm(x, 20, h_param) - 134, linetype = "dotted", n = 500, xlim = c(qnorm(0.001, 20, sd = h_param), xmax = qnorm(0.999, 20, sd = h_param))) +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")

h_param <- 2
band <- 4 * qnorm(0.75) * h_param # Bandwidth as parametrized in ksmooth
fit_nw <- ksmooth(x, y, kernel = "normal", bandwidth = band, x.points = times_seq)

ggplot(data = dataset, aes(x = times, y = accel)) +
  geom_ribbon(aes(xmin = qnorm(0.05, 20, sd = h_param), xmax = qnorm(0.95, 20, sd = h_param)), fill = "#FF8C00", alpha = 0.6) +
  geom_point(size = 0.7) +
  geom_line(data = data.frame(x = fit_nw$x, y = fit_nw$y), aes(x = x, y = y), col = "#3232AA") +
  geom_vline(xintercept = 20, lty = "dashed", linewidth = 0.4) +
  theme_minimal() +
  geom_function(fun = function(x) 180 * dnorm(x, 20, h_param) - 134, linetype = "dotted", n = 500, xlim = c(qnorm(0.001, 20, sd = h_param), xmax = qnorm(0.999, 20, sd = h_param))) +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")

h_param <- 4
band <- 4 * qnorm(0.75) * h_param # Bandwidth as parametrized in ksmooth
fit_nw <- ksmooth(x, y, kernel = "normal", bandwidth = band, x.points = times_seq)

ggplot(data = dataset, aes(x = times, y = accel)) +
  geom_ribbon(aes(xmin = qnorm(0.05, 20, sd = h_param), xmax = qnorm(0.95, 20, sd = h_param)), fill = "#FF8C00", alpha = 0.6) +
  geom_point(size = 0.7) +
  geom_line(data = data.frame(x = fit_nw$x, y = fit_nw$y), aes(x = x, y = y), col = "#3232AA") +
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

id_opt <- which.min(data_goodness$LOO_CV)
h_opt <- data_goodness$bandwidth[id_opt]
df_opt <- data_goodness$df[id_opt]

x_seq <- seq(from = min(x), to = max(x), length = 2000)
fit_locpoly <- loclin(x, y, bandwidth = h_opt, x0 = x_seq)

ggplot(data = dataset, aes(x = times, y = accel)) +
  geom_point(size = 0.7) +
  geom_line(data = data.frame(x = x_seq, y = fit_locpoly), aes(x = x, y = y), col = "#3232AA") +
  theme_minimal() +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")

# Organization of the results for graphical purposes
data_bv <- data.frame(df = data_goodness$df, GCV = data_goodness$GCV, LOO_CV = data_goodness$LOO_CV, SE = data_goodness$LOO_CV_SE)
data_bv <- reshape2::melt(data_bv, id = c("df", "SE"))
data_bv$SE[data_bv$variable == "GCV"] <- NA
levels(data_bv$variable) <- c("GCV", "LOO-CV")
colnames(data_bv) <- c("df", "SE", "Error term", "value")

ggplot(data = data_bv, aes(x = df, y = value, col = `Error term`)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = df_opt, linetype = "dotted", col = "#FF8C00") +
  theme_light() +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Effective degrees of freedom (df)") +
  ylab("Mean Squared Error (MSE)")

library(sm)
auto <- read.table("../data/auto.txt", header = TRUE) %>% select(city.distance, engine.size, n.cylinders, curb.weight, fuel)

h_sm <- c(0.5, 150)
sm.options(ngrid = 50)

fit_sm <- sm.regression(cbind(auto$engine.size, auto$curb.weight), auto$city.distance,
  h = h_sm, display = "none", ngrid = 500,
  hull = TRUE,
  options = list(xlab = "Engine size (L)", ylab = "Curb weight (kg)", zlab = "City distance (km/L)")
)

contour(fit_sm$eval.points[, 1], fit_sm$eval.points[, 2], fit_sm$estimate,
  xlab = "Engine size (L)", ylab = "Curb weight (kg)", col = "#3232AA"
)
points(auto$engine.size, auto$curb.weight, cex = 0.5, pch = 16, col = "#FF8C00")

library(splines2)

knots <- c(15, 25)
fit_bs <- lm(accel ~ bsp(times, knots = knots, degree = 0, intercept = TRUE) - 1, data = dataset)
y_hat_bs <- predict(fit_bs, newdata = data.frame(times = times_seq))

ggplot(data = dataset, aes(x = times, y = accel)) +
  geom_point(size = 0.7) +
  geom_line(data = data.frame(x = times_seq, y = y_hat_bs), aes(x = x, y = y), col = "#3232AA") +
  theme_minimal() +
  geom_vline(xintercept = 15, linetype = "dotted") +
  geom_vline(xintercept = 25, linetype = "dotted") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")

library(splines)
knots <- quantile(dataset$times, ppoints(n = 9))

dataset$time_cut <- cut(dataset$times, breaks = c(2.3, knots, 57.6))

fit_piece <- lm(accel ~ time_cut + times * time_cut + I(times^2) * time_cut, data = dataset)
y_hat_piece <- predict(fit_piece, newdata = data.frame(times = times_seq, time_cut = cut(times_seq, breaks = c(2.3, knots, 57.6))))

fit_bs <- lm(accel ~ bsp(times, knots = knots, degree = 2, intercept = TRUE) - 1, data = dataset)
y_hat_bs <- predict(fit_bs, newdata = data.frame(times = times_seq))

data_plot <- data.frame(
  times = times_seq, pred = c(y_hat_piece, y_hat_bs),
  Method = rep(c("Piecewise quadratic", "Piecewise quadratic (continuity constraints)"),
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

fit_bs_3 <- lm(accel ~ bs(times, df = 12, degree = 3, intercept = TRUE) - 1, data = dataset)
y_hat_bs_3 <- predict(fit_bs_3, newdata = data.frame(times = times_seq))

fit_bs_2 <- lm(accel ~ bs(times, df = 12, degree = 2, intercept = TRUE) - 1, data = dataset)
y_hat_bs_2 <- predict(fit_bs_2, newdata = data.frame(times = times_seq))

fit_bs_1 <- lm(accel ~ bs(times, df = 12, degree = 1, intercept = TRUE) - 1, data = dataset)
y_hat_bs_1 <- predict(fit_bs_1, newdata = data.frame(times = times_seq))

data_plot <- data.frame(
  times = times_seq, pred = c(y_hat_bs_3, y_hat_bs_2, y_hat_bs_1),
  Method = rep(c("3. Cubic spline (d = 3)", "2. Quadratic spline (d = 2)", "1. Linear splines (d = 1)"),
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

# Code execution and storage of the interesting quantities
p_list <- 4:40
data_goodness <- data.frame(p = p_list)
for (i in 1:length(p_list)) {
  # Fitting a polynomial of degree p -1
  fit <- lm(accel ~ bs(times, degree = 3, df = p_list[i], intercept = TRUE) - 1, data = dataset)
  # Computation of the leverages h_i efficiently (using QR)
  lev <- influence(fit)$hat
  res_loo <- (y - fitted(fit)) / (1 - lev)
  data_goodness$LOO_CV[i] <- mean(res_loo^2)
  data_goodness$LOO_CV_SE[i] <- sd(res_loo) / sqrt(nrow(dataset))
  data_goodness$GCV[i] <- mean(((y - fitted(fit)) / (1 - p_list[i] / nrow(dataset)))^2)
}

# Organization of the results for graphical purposes
data_bv <- data.frame(p = p_list, GCV = data_goodness$GCV, LOO_CV = data_goodness$LOO_CV, SE = data_goodness$LOO_CV_SE)
data_bv <- reshape2::melt(data_bv, id = c("p", "SE"))
data_bv$SE[data_bv$variable == "GCV"] <- NA
levels(data_bv$variable) <- c("GCV", "LOO-CV")
colnames(data_bv) <- c("p", "SE", "Error term", "value")

ggplot(data = data_bv, aes(x = p, y = value, col = `Error term`)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 12, linetype = "dotted", col = "#FF8C00") +
  theme_light() +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Model complexity (p)") +
  ylab("Mean Squared Error (MSE)")

knots <- quantile(dataset$times, ppoints(n = 12))

fit_bs_3 <- lm(accel ~ bs(times, knots = knots, degree = 3, intercept = TRUE) - 1, data = dataset)
y_hat_bs_3 <- predict(fit_bs_3, newdata = data.frame(times = times_seq))

# Here it's tricky, because by default ns does a different thing (it uses the boundary knots as regular knots)
fit_ns_3 <- lm(accel ~ ns(times, knots = knots[-c(1, 12)], Boundary.knots = c(knots[1], knots[12]), intercept = TRUE) - 1, data = dataset)
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
  geom_vline(xintercept = knots[c(1, 12)], linetype = "dotted") +
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

x_seq <- seq(from = min(x), to = max(x), length = 2000)
fit_smooth <- smooth.spline(x, y, all.knots = TRUE)
y_hat_smooth <- predict(fit_smooth, x = x_seq)

ggplot(data = dataset, aes(x = times, y = accel)) +
  geom_point(size = 0.7) +
  geom_line(data = data.frame(x = y_hat_smooth$x, y = y_hat_smooth$y), aes(x = x, y = y), col = "#3232AA") +
  theme_minimal() +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Time (ms)") +
  ylab("Head acceleration (g)")
