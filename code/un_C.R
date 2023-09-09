rm(list = ls())
library(tidyverse)
prostate <- read.table("../data/prostate_data.txt")
glimpse(prostate)

# Standardize the predictors, as in Tibshirani (1996)
which_vars <- which(colnames(prostate) %in% c("lpsa", "train"))
prostate[, -which_vars] <- apply(prostate[, -which_vars], 2, function(x) (x - mean(x)) / sd(x))

# Split in training and test
prostate_train <- filter(prostate, train) %>% select(-train)
prostate_test <- filter(prostate, train == FALSE) %>% select(-train)

glimpse(prostate)

library(ggcorrplot)
corr <- cor(subset(prostate_train, select = -lpsa)) # Remove the outcome lpsa
ggcorrplot(corr,
  hc.order = FALSE,
  outline.col = "white",
  ggtheme = ggplot2::theme_bw,
  colors = c("#fc7d0b", "white", "#1170aa")
)

tab <- data.frame(broom::tidy(lm(lpsa ~ ., data = prostate_train), conf.int = FALSE))
rownames(tab) <- tab[, 1]
tab <- t(as.matrix(tab[, -1]))
knitr::kable(tab, digits = 2)

# Here I compute some basic quantities
X <- model.matrix(lpsa ~ ., data = prostate_train)[, -1]
y <- prostate_train$lpsa
n <- nrow(X)
p <- ncol(X) # This does not include the intercept

library(leaps)
fit_best <- regsubsets(lpsa ~ ., data = prostate_train, method = "exhaustive", nbest = 40, nvmax = p)
sum_best <- summary(fit_best)
sum_best$p <- rowSums(sum_best$which) - 1 # Does not include the intercept here

library(ggplot2)
library(ggthemes)
data_best_subset <- data.frame(p = sum_best$p, MSE = sum_best$rss / n)
data_best_subset <- reshape2::melt(data_best_subset, id = c("p"))
colnames(data_best_subset) <- c("p", "MSE", "value")

data_best_subset2 <- data.frame(p = unique(sum_best$p), MSE = tapply(sum_best$rss / n, sum_best$p, min))

ggplot(data = data_best_subset, aes(x = p, y = value)) +
  geom_point() +
  theme_light() +
  theme(legend.position = "top") +
  geom_line(data = data_best_subset2, aes(x = p, y = MSE), col = "#fc7d0b") +
  geom_point(data = data_best_subset2, aes(x = p, y = MSE), col = "#fc7d0b", size = 1.5) +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Number of covariates") +
  ylab("Mean squared error (training)")

summary(regsubsets(lpsa ~ ., data = prostate_train, method = "exhaustive", nbest = 1, nvmax = p))$outmat

library(rsample)

set.seed(123)
cv_fold <- vfold_cv(prostate_train, v = 10)
resid_subs <- matrix(0, n, p + 1)

for (k in 1:10) {
  # Estimation of the null model
  fit_null <- lm(lpsa ~ 1, data = analysis(cv_fold$splits[[k]]))
  # Best subset using branch and bound
  fit <- regsubsets(lpsa ~ ., data = analysis(cv_fold$splits[[k]]), method = "exhaustive", nbest = 1, nvmax = p)
  sum <- summary(fit)

  # Hold-out quantities
  X_k <- as.matrix(cbind(1, assessment(cv_fold$splits[[k]]) %>% select(-lpsa)))
  y_k <- assessment(cv_fold$splits[[k]])$lpsa

  # MSE of the null model
  resid_subs[complement(cv_fold$splits[[k]]), 1] <- y_k - predict(fit_null, assessment(cv_fold$splits[[k]]))

  # MSE of the best models for different values of p
  for (j in 2:(p + 1)) {
    y_hat <- X_k[, sum$which[j - 1, ]] %*% coef(fit, j - 1)
    resid_subs[complement(cv_fold$splits[[k]]), j] <- y_k - y_hat
  }
}

data_cv <- data.frame(
  p = 0:p,
  MSE = apply(resid_subs^2, 2, mean),
  SE = apply(resid_subs^2, 2, function(x) sd(x) / sqrt(n))
)

se_rule <- data_cv$MSE[which.min(data_cv$MSE)] + data_cv$SE[which.min(data_cv$MSE)]
p_optimal <- which(data_cv$MSE < se_rule)[1] - 1

ggplot(data = data_cv, aes(x = p, y = MSE)) +
  geom_point() +
  geom_line() +
  geom_linerange(aes(ymax = MSE + SE, ymin = MSE - SE)) +
  geom_hline(yintercept = se_rule, linetype = "dotted") +
  geom_vline(xintercept = p_optimal, linetype = "dotted") +
  theme_light() +
  scale_x_continuous(breaks = 0:9) +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Number of covariates") +
  ylab("Mean squared error (10-fold cv)")

fit_forward <- regsubsets(lpsa ~ ., data = prostate_train, method = "forward", nbest = 1, nvmax = p)
sum_forward <- summary(fit_forward)
fit_backward <- regsubsets(lpsa ~ ., data = prostate_train, method = "backward", nbest = 1, nvmax = p)
sum_backward <- summary(fit_backward)

# Organization of the results for graphical purposes
data_stepwise <- data.frame(
  p = c(1:p, 1:p, 1:p), MSE = c(
    sum_forward$rss,
    sum_backward$rss,
    tapply(sum_best$rss, sum_best$p, min)
  ) / n,
  Stepwise = rep(c("Forward", "Backward", "Best subset"), each = p)
)
data_stepwise <- reshape2::melt(data_stepwise, id = c("p", "Stepwise"))
colnames(data_stepwise) <- c("p", "Stepwise", "MSE", "value")

ggplot(data = data_stepwise, aes(x = p, y = value, col = Stepwise)) +
  geom_line() +
  geom_point() +
  facet_grid(. ~ Stepwise) +
  theme_light() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = 0:9) +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Number of covariates") +
  ylab("MSE (training)")

pr <- princomp(prostate_train[, -9], cor = FALSE)
ggplot(data = data.frame(p = 1:p, vars = pr$sdev^2 / sum(pr$sdev^2)), aes(x = p, xmin = p, xmax = p, y = vars, ymax = vars, ymin = 0)) +
  geom_pointrange() +
  theme_light() +
  scale_x_continuous(breaks = 1:9) +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Number of principal components") +
  ylab("Fraction of explained variance")

library(pls)
resid_pcr <- matrix(0, n, p)

for (k in 1:10) {
  # Hold-out dataset
  y_k <- assessment(cv_fold$splits[[k]])$lpsa
  # MSE of the null model
  resid_pcr[complement(cv_fold$splits[[k]]), 1] <- y_k - predict(fit_null, assessment(cv_fold$splits[[k]]))
  # Fitting PCR (all the components at once)
  fit_pcr <- pcr(lpsa ~ ., data = analysis(cv_fold$splits[[k]]), center = TRUE, scale = FALSE)

  for (j in 2:p) {
    # Predictions
    y_hat <- predict(fit_pcr, newdata = assessment(cv_fold$splits[[k]]))[, , j - 1]
    # MSE of the best models for different values of p
    resid_pcr[complement(cv_fold$splits[[k]]), j] <- y_k - y_hat
  }
}

data_cv <- data.frame(
  p = 1:p,
  MSE = apply(resid_pcr^2, 2, mean),
  SE = apply(resid_pcr^2, 2, function(x) sd(x) / sqrt(n))
)

se_rule <- data_cv$MSE[which.min(data_cv$MSE)] + data_cv$SE[which.min(data_cv$MSE)]
p_optimal <- which(data_cv$MSE < se_rule)[1]

ggplot(data = data_cv, aes(x = p, y = MSE)) +
  geom_point() +
  geom_line() +
  geom_linerange(aes(ymax = MSE + SE, ymin = MSE - SE)) +
  geom_hline(yintercept = se_rule, linetype = "dotted") +
  geom_vline(xintercept = p_optimal, linetype = "dotted") +
  theme_light() +
  scale_x_continuous(breaks = 1:9) +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Number of principal components") +
  ylab("Mean squared error (10-fold cv)")

fit_pcr <- pcr(lpsa ~ ., data = prostate_train, center = TRUE, scale = FALSE)

data_pcr <- reshape2::melt(coef(fit_pcr, 1:8))
colnames(data_pcr) <- c("Covariate", "lpsa", "Components", "value")
data_pcr$Components <- as.numeric(data_pcr$Components)
data_pcr <- rbind(data_pcr, data.frame(Covariate = data_pcr$Covariate[data_pcr$Components == 1], lpsa = NA, Components = 0, value = 0))
data_pcr$Covariate <- as.factor(as.character(data_pcr$Covariate))
ggplot(data = data_pcr, aes(x = Components, y = value, col = Covariate)) +
  geom_point() +
  geom_line() +
  theme_light() +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = 0:9) +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Number of principal components") +
  ylab("Regression coefficients")



my_ridge <- function(X, y, lambda_tilde, standardize = TRUE) {
  n <- nrow(X)
  p <- ncol(X)
  lambda <- lambda_tilde * n
  y_mean <- mean(y) # Center the response
  y <- y - y_mean

  # Centering the covariates
  X_mean <- colMeans(X)
  if (standardize) {
    X_scale <- apply(X, 2, function(x) sqrt(mean(x^2) - mean(x)^2))
  } else {
    X_scale <- rep(1, p)
  }

  X <- scale(X, center = X_mean, scale = X_scale)

  # Ridge solution (scaled data)
  beta_scaled <- solve(crossprod(X) + lambda * diag(rep(1, p)), crossprod(X, y))

  # Transform back to the original scale
  beta <- beta_scaled / X_scale
  # Compute the intercept
  beta0 <- y_mean - X_mean %*% beta
  return(c(beta0, beta))
}

df_ridge <- function(lambda_tilde, X, standardize = TRUE) {
  n <- nrow(X)
  lambda <- lambda_tilde * n
  # Rescale the predictors
  X_mean <- colMeans(X)
  if (standardize) {
    X_scale <- apply(X, 2, function(x) sqrt(mean(x^2) - mean(x)^2))
  } else {
    X_scale <- rep(1, p)
  }
  X <- scale(X, center = X_mean, scale = X_scale)
  d2 <- eigen(crossprod(X))$values
  1 + sum(d2 / (d2 + lambda))
}

df_ridge <- Vectorize(df_ridge, vectorize.args = "lambda_tilde")

lambda_tilde_seq <- exp(seq(from = -4, to = 5, length = 50))
data_ridge <- cbind(lambda_tilde_seq, matrix(0, length(lambda_tilde_seq), p))

mse_ridge <- data.frame(lambda_tilde_seq, Cp = NA, mse = NA, df = df_ridge(lambda_tilde_seq, X, standardize = TRUE), sigma2 = NA)

for (i in 1:length(lambda_tilde_seq)) {
  data_ridge[i, -1] <- my_ridge(X, y, lambda_tilde = lambda_tilde_seq[i], standardize = TRUE)[-1]
  mse_ridge$mse[i] <- mean((y - mean(y) - X %*% data_ridge[i, -1])^2)
  mse_ridge$sigma2[i] <- mse_ridge$mse[i] * n / (n - mse_ridge$df[i])
  mse_ridge$Cp[i] <- mse_ridge$mse[i] + 2 * mse_ridge$sigma2[i] / n * mse_ridge$df[i]
}

colnames(data_ridge)[-1] <- colnames(X)
data_ridge <- tidyr::gather(data.frame(data_ridge), lambda, value, lcavol:pgg45)
colnames(data_ridge) <- c("lambda_tilde", "Covariate", "value")

ggplot(data = data_ridge, aes(x = lambda_tilde, y = value, col = Covariate)) +
  geom_point() +
  geom_line() +
  theme_light() +
  theme(legend.position = "top") +
  scale_x_log10() +
  scale_color_tableau(palette = "Color Blind") +
  xlab(expression(lambda / n)) +
  ylab("Coefficients (original scale)")

df_min_cp <- mse_ridge$df[which.min(mse_ridge$Cp)]
lambda_tilde_min_cp <- mse_ridge$lambda_tilde_seq[which.min(mse_ridge$Cp)]

ggplot(data = mse_ridge, aes(x = df, y = Cp)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = df_min_cp, linetype = "dotted") +
  theme_light() +
  scale_x_continuous(breaks = 1:9) +
  xlab("Effective degrees of freedom (df)") +
  ylab(expression(C[p]))

ggplot(data = mse_ridge, aes(x = lambda_tilde_seq, y = Cp)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = lambda_tilde_min_cp, linetype = "dotted") +
  theme_light() +
  scale_x_log10() +
  xlab(expression(lambda / n)) +
  ylab(expression(C[p]))

resid_ridge <- matrix(0, n, length(lambda_tilde_seq))

for (k in 1:10) {
  # Hold-out dataset
  X_test_k <- as.matrix(subset(assessment(cv_fold$splits[[k]]), select = -c(lpsa)))
  y_test_k <- assessment(cv_fold$splits[[k]])$lpsa

  X_train_k <- as.matrix(subset(analysis(cv_fold$splits[[k]]), select = -c(lpsa)))
  y_train_k <- analysis(cv_fold$splits[[k]])$lpsa

  for (j in 1:length(lambda_tilde_seq)) {
    # Estimates
    beta_hat <- my_ridge(X_train_k, y_train_k, lambda_tilde = lambda_tilde_seq[j], standardize = TRUE)
    # Predictions
    y_hat <- cbind(1, X_test_k) %*% beta_hat
    # MSE of the best models for different values of lambda
    resid_ridge[complement(cv_fold$splits[[k]]), j] <- y_test_k - y_hat
  }
}

data_cv <- data.frame(
  lambda_tilde = lambda_tilde_seq,
  df = df_ridge(lambda_tilde_seq, X, standardize = TRUE),
  MSE = apply(resid_ridge^2, 2, mean),
  SE = apply(resid_ridge^2, 2, function(x) sd(x) / sqrt(n))
)

se_rule <- data_cv$MSE[which.min(data_cv$MSE)] + data_cv$SE[which.min(data_cv$MSE)]
lambda_tilde_min_cv <- lambda_tilde_seq[tail(which(data_cv$MSE < se_rule), 1)]
df_min_cv <- data_cv$df[tail(which(data_cv$MSE < se_rule), 1)]

ggplot(data = data_cv, aes(x = lambda_tilde, y = MSE)) +
  geom_point() +
  geom_line() +
  geom_linerange(aes(ymax = MSE + SE, ymin = MSE - SE)) +
  scale_x_log10() +
  geom_hline(yintercept = se_rule, linetype = "dotted") +
  geom_vline(xintercept = lambda_tilde_min_cv, linetype = "dotted") +
  theme_light() +
  xlab(expression(tilde(lambda))) +
  ylab("Mean squared error (10-fold cv)")

# Double checks
library(glmnet)
y_std <- prostate_train$lpsa / sqrt(mean(prostate_train$lpsa^2) - mean(prostate_train$lpsa)^2)

as.numeric(coef(glmnet(X, y_std, family = "gaussian", standardize = TRUE, alpha = 0, thresh = 1e-20, lambda = lambda_tilde_min_cv)))

my_ridge(X, y_std, standardize = TRUE, lambda_tilde = lambda_tilde_min_cv)

ggplot(data = data_ridge, aes(x = df_ridge(lambda_tilde, X), y = value, col = Covariate)) +
  geom_point() +
  geom_line() +
  theme_light() +
  geom_vline(xintercept = df_min_cv, linetype = "dashed") +
  geom_vline(xintercept = df_min_cp, linetype = "dotted") +
  scale_x_continuous(breaks = 1:9) +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Effective degrees of freedom (df)") +
  ylab("Regression coefficients")



x <- seq(-0.5, 0.5, length = 100)
data_plot <- data.frame(
  x = x,
  OLS = x,
  Ridge = x / (1 + sqrt(0.2)),
  Lasso = sign(x) * pmax(abs(x) - 0.2, 0)
)
data_plot <- pivot_longer(data_plot, cols = c(OLS, Ridge, Lasso), names_to = "Estimate")
ggplot(data = data_plot, aes(x = x, y = value, col = Estimate)) +
  geom_line() +
  theme_light() +
  coord_fixed() +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab(expression(hat(beta)[ols])) +
  ylab(expression(hat(beta)))

library(lars)

my_lasso <- function(X, y, standardize = TRUE) {
  n <- nrow(X)
  p <- ncol(X)
  y_mean <- mean(y) # Center the response
  y <- y - y_mean

  # Centering the covariates
  X_mean <- colMeans(X)
  if (standardize) {
    X_scale <- apply(X, 2, function(x) sqrt(mean(x^2) - mean(x)^2))
  } else {
    X_scale <- rep(1, p)
  }

  X <- scale(X, center = X_mean, scale = X_scale)

  fit <- lars(x = X, y = y, type = "lasso", normalize = FALSE, intercept = TRUE)
  beta_lasso <- coef(fit)

  # Transform back to the original scale
  beta <- t(t(beta_lasso) / X_scale)
  # Compute the intercept
  beta0 <- y_mean - X_mean %*% t(beta)


  return(list(beta_scaled = cbind(intercept = t(beta0), beta), lambda = c(fit$lambda, 0) / n))
}

fit_lasso <- my_lasso(X, y)
data_lasso <- cbind(lambda_seq = fit_lasso$lambda, coef = fit_lasso$beta_scaled)
mse_lasso <- data.frame(lambda = data_lasso[, 1], Cp = NA, df = 1:9, sigma2 = NA)

for (i in 1:nrow(mse_lasso)) {
  mse_lasso$mse[i] <- mean((y - cbind(1, X) %*% data_lasso[i, -1])^2)
  mse_lasso$sigma2[i] <- mse_lasso$mse[i] * n / (n - mse_lasso$df[i])
  mse_lasso$Cp[i] <- mse_lasso$mse[i] + 2 * mse_lasso$sigma2[i] / n * mse_lasso$df[i]
}

colnames(data_lasso)[-c(1, 2)] <- colnames(X)
data_lasso <- tidyr::gather(data.frame(data_lasso[, -2]), lambda, value, lcavol:pgg45)
colnames(data_lasso) <- c("lambda", "Covariate", "value")

ggplot(data = data_lasso, aes(x = lambda, y = value, col = Covariate)) +
  geom_point() +
  geom_line() +
  theme_light() +
  theme(legend.position = "top") +
  scale_x_sqrt() +
  scale_color_tableau(palette = "Color Blind") +
  xlab(expression(lambda)) +
  ylab("Regression coefficients")

n_sim <- 100
p_sim <- 25
R <- 5000
sigma_sim <- 0.5

set.seed(220)
X_sim <- matrix(runif(n_sim * p_sim), n_sim, p_sim)
# Here data are not related with any of the covariates
Y_sim <- matrix(rnorm(n_sim * R, mean = 4, sigma_sim), R, n_sim)

pred_best_sim <- array(0, c(R, n_sim, p_sim + 1))
pred_lasso_sim <- array(0, c(R, n_sim, p_sim + 1))

for (r in 1:R) {
  y_sim <- Y_sim[r, ]
  # Lasso fit
  lasso_sim <- my_lasso(X_sim, y_sim, standardize = TRUE)
  # Best subset
  best_sim <- regsubsets(y_sim ~ X_sim, data = NULL, method = "exhaustive", nbest = 1, nvmax = p_sim)
  sum_best_sim <- summary(best_sim)

  for (j in 1:(p_sim + 1)) {
    pred_lasso_sim[r, , j] <- cbind(1, X_sim) %*% lasso_sim$beta_scaled[j, ]
    if (j == 1) {
      pred_best_sim[r, , 1] <- pred_lasso_sim[r, , 1]
    } else {
      pred_best_sim[r, , j] <- cbind(1, X_sim)[, sum_best_sim$which[j - 1, ]] %*% coef(best_sim, j - 1)
    }
  }
}

df_lasso <- matrix(0, n_sim, p_sim + 1)
df_best <- matrix(0, n_sim, p_sim + 1)

for (j in 1:(p_sim + 1)) {
  for (i in 1:n_sim) {
    df_lasso[i, j] <- cov(pred_lasso_sim[, i, j], Y_sim[, i])
    df_best[i, j] <- cov(pred_best_sim[, i, j], Y_sim[, i])
  }
}
df_lasso <- colSums(df_lasso) / sigma_sim^2
df_best <- colSums(df_best) / sigma_sim^2

data_df <- data.frame(active_set = 1:(p_sim + 1), df = c(df_lasso, df_best), Method = rep(c("LAR", "Best subset"), each = p_sim + 1))

ggplot(data = data_df, aes(x = active_set, y = df, col = Method)) +
  geom_point() +
  geom_line() +
  theme_light() +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  xlab("Number of non-zero coefficients") +
  ylab("Effective degrees of freedom")

resid_lasso <- matrix(0, n, (p + 1))

for (k in 1:10) {
  # Hold-out dataset
  X_test_k <- as.matrix(subset(assessment(cv_fold$splits[[k]]), select = -c(lpsa)))
  y_test_k <- assessment(cv_fold$splits[[k]])$lpsa

  X_train_k <- as.matrix(subset(analysis(cv_fold$splits[[k]]), select = -c(lpsa)))
  y_train_k <- analysis(cv_fold$splits[[k]])$lpsa

  fit_lasso <- my_lasso(X_train_k, y_train_k, standardize = TRUE)

  for (j in 1:(p + 1)) {
    # Predictions
    y_hat <- cbind(1, X_test_k) %*% fit_lasso$beta_scaled[j, ]
    # MSE of the best models for different values of lambda
    resid_lasso[complement(cv_fold$splits[[k]]), j] <- y_test_k - y_hat
  }
}

data_cv <- data.frame(
  df = 1:9,
  MSE = apply(resid_lasso^2, 2, mean),
  SE = apply(resid_lasso^2, 2, function(x) sd(x) / sqrt(n))
)

se_rule <- data_cv$MSE[which.min(data_cv$MSE)] + data_cv$SE[which.min(data_cv$MSE)]
df_min_cv <- data_cv$df[head(which(data_cv$MSE < se_rule), 1)]

ggplot(data = data_cv, aes(x = df, y = MSE)) +
  geom_point() +
  geom_line() +
  geom_linerange(aes(ymax = MSE + SE, ymin = MSE - SE)) +
  scale_x_continuous(breaks = 1:9) +
  geom_hline(yintercept = se_rule, linetype = "dotted") +
  geom_vline(xintercept = df_min_cv, linetype = "dotted") +
  theme_light() +
  xlab("Degrees of freedom") +
  ylab("Mean squared error (10-fold cv)")

# Double checks
y_std <- prostate_train$lpsa / sqrt(mean(prostate_train$lpsa^2) - mean(prostate_train$lpsa)^2)

fit_lasso <- my_lasso(X, y_std, standardize = TRUE)

t(fit_lasso$beta_scaled)

coef(glmnet(X, y_std, family = "gaussian", standardize = TRUE, alpha = 1, thresh = 1e-20, lambda = fit_lasso$lambda))

data_lasso$df <- 1:9
ggplot(data = data_lasso, aes(x = df, y = value, col = Covariate)) +
  geom_point() +
  geom_line() +
  theme_light() +
  geom_vline(xintercept = 4, linetype = "dashed") +
  scale_x_continuous(breaks = 1:9) +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Degrees of freedom") +
  ylab("Regression coefficients")

library(DT)

tab <- data.frame(OLS = rep(0, p + 1), best_subset = rep(0, p + 1), PCR = rep(0, p + 1), Ridge = rep(0, p + 1), Lasso = rep(0, p + 1))

rownames(tab) <- colnames(sum_best$which)

# OLS
tab$OLS <- coef(lm(lpsa ~ ., data = prostate_train))

# Best subset
tab$best_subset <- c(coef(lm(lpsa ~ lcavol + lweight, data = prostate_train)), rep(0, 6))

# Principal components regression (PCR)
fit_pcr <- pcr(lpsa ~ ., data = prostate_train, center = TRUE, scale = FALSE)
beta_pcr <- c(coef(fit_pcr, 3))
beta_pcr <- c(mean(prostate_train$lpsa) - colMeans(X) %*% beta_pcr, beta_pcr)
tab$PCR <- beta_pcr

# Ridge

tab$Ridge <- my_ridge(X, y, standardize = TRUE, lambda_tilde = lambda_tilde_min_cp)

# Lasso
tab$Lasso <- c(my_lasso(X, y)$beta_scale[4, ])


# Output
datatable(tab, colnames = c("OLS", "Best subset", "PCR", "Ridge", "Lasso"), options = list(pageLength = 9, dom = "t")) %>%
  formatRound(columns = 1:5, digits = 3) %>%
  formatStyle(
    columns = 0, fontWeight = "bold"
  ) %>%
  formatStyle(
    columns = 1:5,
    backgroundColor = styleInterval(0, c("#FED8B1", "#DBE9FA"))
  ) %>%
  formatStyle(
    columns = 1:5,
    backgroundColor = styleEqual(0, c("white"))
  )

X_test <- cbind(1, as.matrix(prostate_test)[, -9])
y_test <- prostate_test[, 9]

pred_ols <- X_test %*% tab$OLS
pred_best <- X_test %*% tab$best_subset
pred_pcr <- X_test %*% tab$PCR
pred_ridge <- X_test %*% tab$Ridge
pred_lasso <- X_test %*% tab$Lasso

tab_results <- matrix(c(
  mean((y_test - pred_ols)^2),
  mean((y_test - pred_best)^2),
  mean((y_test - pred_pcr)^2),
  mean((y_test - pred_ridge)^2),
  mean((y_test - pred_lasso)^2)
), nrow = 1)
colnames(tab_results) <- c("OLS", "Best subset", "PCR", "Ridge", "Lasso")
rownames(tab_results) <- "Test error (MSE)"
knitr::kable(tab_results, digits = 3)

# Objective function
f <- function(x) {
  return((1 - x[1] - 2 * x[2])^2 + (3 - x[1] - 2 * x[2])^2 + 5 * (abs(x[1]) + abs(x[2])))
}

# Finds the minimum
m <- optim(c(0, 0), f, method = "Nelder-Mead")

# Create a data frame for contour plotting
delta <- 0.05
x <- seq(-2.5, 2.5, delta)
y <- seq(-2.5, 3, delta)
df <- expand.grid(X = x, Y = y)
df$Z <- apply(df, 1, function(row) f(row))

ggplot(data = df, aes(x = X, y = Y)) +
  theme_light() +
  geom_contour(aes(z = Z), colour = "#1170aa", bins = 25) +
  geom_point(
    data = data.frame(
      X = c(m$par[1], -1.53),
      Y = c(m$par[2], -2)
    ),
    aes(x = X, y = Y), color = "black"
  ) +
  geom_segment(aes(x = -1.53, y = -2, xend = -1.53, yend = 1.5),
    arrow = arrow(type = "closed", angle = 25, length = unit(0.05, "inches")), color = "#fc7d0b"
  ) +
  geom_segment(aes(x = -1.53, y = 1.5, xend = m$par[1], yend = 1.5),
    arrow = arrow(type = "closed", angle = 25, length = unit(0.05, "inches")), color = "#fc7d0b"
  ) +
  geom_segment(aes(x = m$par[1], y = 1.5, xend = m$par[1], yend = m$par[2]),
    arrow = arrow(type = "closed", angle = 25, length = unit(0.05, "inches")), color = "#fc7d0b"
  ) +
  labs(x = expression(beta[1]), y = expression(beta[2])) +
  theme_minimal() +
  coord_fixed()
