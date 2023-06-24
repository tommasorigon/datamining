## ----r------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggthemes)

rm(list = ls())
# The dataset can be downloaded here: https://tommasorigon.github.io/datamining/data/yesterday.txt
dataset <- read.table("../data/yesterday.txt", header = TRUE)
ggplot(data = dataset, aes(x = x, y = y.yesterday)) +
  geom_point() +
  theme_light() +
  scale_color_tableau(palette = "Color Blind") +
  xlab("x") +
  ylab("y")


## ----r------------------------------------------------------------------------
# Degrees of the polynomials
degree_list <- c(1, 3, 5, 11, 17, 23)

# I am using 30.000 obs to improve the quality of the graph
x_seq <- seq(from = min(dataset$x), to = max(dataset$x), length = 30000)

# Actual fitting procedure
data_pred <- NULL
for (degree in degree_list) {
  # Fitting a polynomial of degree p - 1
  fit <- lm(y.yesterday ~ poly(x, degree = degree, raw = FALSE), data = dataset)
  # Fitted values
  y_hat <- predict(fit, newdata = data.frame(x = x_seq))
  data_pred <- rbind(data_pred, data.frame(x = x_seq, y_hat = y_hat, degree = paste("Number of parameters p:", degree + 1)))
}

# Graphical adjustment to get the plots in the right order
data_pred$degree <- factor(data_pred$degree)
data_pred$degree <- factor(data_pred$degree, levels = levels(data_pred$degree)[c(3, 5, 6, 1, 2, 4)])

# Final plot
ggplot(data = data_pred) +
  geom_line(aes(x = x, y = y_hat, col = degree)) +
  geom_point(data = dataset, aes(x = x, y = y.yesterday), size = 0.8) +
  theme_light() +
  theme(legend.position = "none") +
  facet_wrap(. ~ degree, ncol = 3) +
  scale_color_tableau(palette = "Color Blind") +
  xlab("x") +
  ylab("y") +
  ylim(c(0.42, 0.56)) # Manual identification of an "interesting" region


## ----r------------------------------------------------------------------------
# Main chunk of code; fitting several models and storing some relevant quantities
degree_list <- 1:23

# Tomorrow's mean-squared error
MSE_tot <- mean((dataset$y.tomorrow - mean(dataset$y.tomorrow))^2)

# Initialization
data_goodness <- data.frame(degree = degree_list, MSE = NA, R_squared = NA, MSE_test = NA, R_squared_test = NA)

# Code execution
for (degree in degree_list) {
  # Fitting a polynomial of degree p -1
  fit <- lm(y.yesterday ~ poly(x, degree = degree, raw = FALSE), data = dataset)
  y_hat <- fitted(fit)

  # Training goodness of fit
  data_goodness$MSE[degree] <- mean((dataset$y.yesterday - y_hat)^2)
  data_goodness$R_squared[degree] <- summary(fit)$r.squared

  # Test goodness of fit
  data_goodness$MSE_test[degree] <- mean((dataset$y.tomorrow - y_hat)^2)
  data_goodness$R_squared_test[degree] <- 1 - data_goodness$MSE_test[degree] / MSE_tot
}


## ----r------------------------------------------------------------------------
ggplot(data = data_goodness, aes(x = degree + 1, y = MSE)) +
  geom_line() +
  geom_point() +
  theme_light() +
  xlab("# of parameters p") +
  ylab("MSE")


## ----r------------------------------------------------------------------------
ggplot(data = data_goodness, aes(x = degree + 1, y = R_squared)) +
  geom_line() +
  geom_point() +
  theme_light() +
  xlab("# of parameters p") +
  ylab(expression(R^2))


## ----r------------------------------------------------------------------------
lagrange <- function(x0, y0) {
  f <- function(x) {
    sum(y0 * sapply(seq_along(x0), \(j) {
      prod(x - x0[-j]) / prod(x0[j] - x0[-j])
    }))
  }
  Vectorize(f, "x")
}
f <- lagrange(dataset$x, dataset$y.yesterday)

plot(dataset$x, dataset$y.yesterday, pch = 16, xlab = "x", ylab = "y", main = "Degree of the polynomial: n-1")
curve(f(x), n = 300, add = TRUE)


## ----r------------------------------------------------------------------------
ggplot(data = data_pred) +
  geom_line(aes(x = x, y = y_hat, col = degree)) +
  geom_point(data = dataset, aes(x = x, y = y.tomorrow), size = 0.8) +
  theme_light() +
  theme(legend.position = "none") +
  facet_wrap(. ~ degree, ncol = 3) +
  scale_color_tableau(palette = "Color Blind") +
  xlab("x") +
  ylab("y") +
  ylim(c(0.42, 0.56))


## ----r------------------------------------------------------------------------
ggplot(data = data_goodness, aes(x = degree + 1, y = MSE_test)) +
  geom_line() +
  geom_point() +
  theme_light() +
  xlab("# of parameters p") +
  ylab("MSE")


## ----r------------------------------------------------------------------------
ggplot(data = data_goodness, aes(x = degree + 1, y = R_squared_test)) +
  geom_line() +
  geom_point() +
  theme_light() +
  xlab("# of parameters p") +
  ylab(expression(R^2))


## ----r------------------------------------------------------------------------
fit <- lm(y.yesterday ~ poly(x, degree = 3, raw = FALSE), data = dataset)
X <- model.matrix(fit)
colnames(X) <- c("Intercept", "x1", "x2", "x3")
round(t(X) %*% X, 8)


## ----r------------------------------------------------------------------------
# I am storing these information for simplicity
x <- dataset$x
n <- nrow(dataset)

# The standard deviation of the data generative process is sigma2true = 0.01, declared in the book and the slides
sigmatrue <- 0.01

# The true values have been downloaded from the A&S texbook; see here: http://azzalini.stat.unipd.it/Book-DM/f_true.R
ftrue <- c(
  0.4342, 0.4780, 0.5072, 0.5258, 0.5369,
  0.5426, 0.5447, 0.5444, 0.5425, 0.5397,
  0.5364, 0.5329, 0.5294, 0.5260, 0.5229,
  0.5200, 0.5174, 0.5151, 0.5131, 0.5113,
  0.5097, 0.5083, 0.5071, 0.5061, 0.5052,
  0.5044, 0.5037, 0.5032, 0.5027, 0.5023
)


## ----r------------------------------------------------------------------------
# Number of degree of the polynomial
degree_list <- 1:23
# Number of parameters in the model
p_list <- degree_list + 1

# Squared bias
Bias2s <- sapply(p_list, function(p) {
  mean((ftrue - fitted(lm(ftrue ~ poly(x, degree = p - 1))))^2)
})

# Variance
Vars <- p_list * (sigmatrue^2) / n
# This simplified formula can be obtained after some algebraic manipulation, under the assumption that the covariates in the test set are equal to those in the training; please refer to the exercises.

# Reducible errors
MSEs <- Bias2s + Vars

# Organize the data to
data_bv <- data.frame(p = p_list, Bias = Bias2s, Variance = Vars, MSE = MSEs)
data_bv <- reshape2::melt(data_bv, id = "p")
levels(data_bv$variable) <- c("Squared Bias", "Variance", "Reducible error")
colnames(data_bv) <- c("p", "Error term", "value")


## ----r------------------------------------------------------------------------
ggplot(data = data_bv, aes(x = p, y = value, col = `Error term`)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 6, linetype = "dotted") +
  theme_light() +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Model complexity (p)") +
  ylab("Error")


## ----r------------------------------------------------------------------------
data_bv <- data.frame(
  p = p_list, MSE = sigmatrue^2 + Bias2s + Vars,
  MSE_train = data_goodness$MSE, MSE_test = data_goodness$MSE_test
)
data_bv <- reshape2::melt(data_bv, id = "p")
levels(data_bv$variable) <- c("Expected prediction error (theoretical)", "MSE train", "MSE test")
colnames(data_bv) <- c("p", "Error term", "value")

ggplot(data = data_bv, aes(x = p, y = value, col = `Error term`)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 5, linetype = "dotted") +
  theme_light() +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Model complexity (p)") +
  ylab("Error")


## ----r------------------------------------------------------------------------
degree_list <- 1:23
# Creation of the dataset using 60 observations.
data_cv <- data.frame(x = c(dataset$x, dataset$x), y = c(dataset$y.yesterday, dataset$y.tomorrow))
# Code execution and storage of the interesting quantities
for (degree in degree_list) {
  # Fitting a polynomial of degree p -1
  fit <- lm(y ~ poly(x, degree = degree, raw = FALSE), data = data_cv)
  # Computation of the leverages h_i in an efficient manner (using QR)
  lev <- influence(fit)$hat
  data_goodness$LOO_CV[degree] <- mean(((data_cv$y - fitted(fit)) / (1 - lev))^2)
}

# Organization of the results for graphical purposes
data_bv <- data.frame(p = p_list, MSE = sigmatrue^2 + Bias2s + Vars, LOO_CV = data_goodness$LOO_CV)
data_bv <- reshape2::melt(data_bv, id = "p")
levels(data_bv$variable) <- c("Expected prediction error (theoretical)", "LOO-CV")
colnames(data_bv) <- c("p", "Error term", "value")


## ----r------------------------------------------------------------------------
ggplot(data = data_bv, aes(x = p, y = value, col = `Error term`)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 6, linetype = "dotted") +
  theme_light() +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Model complexity (p)") +
  ylab("Error")
