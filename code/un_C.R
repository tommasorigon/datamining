#| message: false
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


#| fig-width: 15
#| fig-height: 7
#| fig-align: center
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


#| fig-width: 10
#| fig-height: 5
#| fig-align: center

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


#| fig-width: 10
#| fig-height: 5
#| fig-align: center

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


#| fig-width: 10
#| fig-height: 3.5
#| fig-align: center

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


#| fig-width: 10
#| fig-height: 5
#| fig-align: center
pr <- princomp(prostate_train[, -9], cor = FALSE)
ggplot(data = data.frame(p = 1:p, vars = pr$sdev^2 / sum(pr$sdev^2)), aes(x = p, xmin = p, xmax = p, y = vars, ymax = vars, ymin = 0)) +
  geom_pointrange() +
  theme_light() +
  scale_x_continuous(breaks = 1:9) +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Number of principal components") +
  ylab("Fraction of explained variance")


#| message: false
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


#| fig-width: 10
#| fig-height: 5
#| fig-align: center

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


#| fig-width: 9
#| fig-height: 5
#| fig-align: center
fit_pcr <- pcr(lpsa ~ ., data = prostate_train, center = TRUE, scale = FALSE)

data_pcr <- reshape2::melt(coef(fit_pcr, 1:8))
colnames(data_pcr) <- c("Covariate", "lpsa", "Components", "value")
data_pcr$Components <- as.numeric(data_pcr$Components)
data_pcr <- rbind(data_pcr, data.frame(Covariate = data_pcr$Covariate[data_pcr$Components == 1], lpsa = NA, Components = 0, value = 0))
ggplot(data = data_pcr, aes(x = Components, y = value, col = Covariate)) +
  geom_point() +
  geom_line() +
  theme_light() +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = 0:9) +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Number of principal components") +
  ylab("Regression coefficients")



my_ridge <- function(X, y, lambda, standardize = FALSE) {
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

  # Ridge solution (scaled data)
  beta_scaled <- solve(crossprod(X) + lambda * diag(rep(1, p)), crossprod(X, y))

  # Transform back to the original scale
  beta <- beta_scaled / X_scale
  # Compute the intercept
  beta0 <- y_mean - X_mean %*% beta
  return(c(beta0, beta))
}



df_ridge <- function(lambda, X, standardize = FALSE) {
  # Rescale the predictors
  X_mean <- colMeans(X)
  if (standardize) {
    X_scale <- apply(X, 2, function(x) sqrt(mean(x^2) - mean(x)^2))
  } else {
    X_scale <- rep(1, p)
  }
  X <- scale(X, center = X_mean, scale = X_scale)
  d2 <- eigen(crossprod(X))$values
  sum(d2 / (d2 + lambda))
}

df_ridge <- Vectorize(df_ridge, vectorize.args = "lambda")


#| fig-width: 9
#| fig-height: 5
#| fig-align: center
lambda_seq <- c(seq(0.1, 0.9, length = 10), exp(seq(from = 0, to = 9, length = 50)))
data_ridge <- cbind(lambda_seq, matrix(0, length(lambda_seq), p))

mse_ridge <- data.frame(lambda_seq, Cp = NA, mse = NA, df = df_ridge(lambda_seq, X, standardize = FALSE), sigma2 = NA)

for (i in 1:length(lambda_seq)) {
  data_ridge[i, -1] <- my_ridge(X, y, lambda = lambda_seq[i], standardize = FALSE)[-1]
  mse_ridge$mse[i] <- mean((y - mean(y) - X %*% data_ridge[i, -1])^2)
  mse_ridge$sigma2[i] <- mse_ridge$mse[i] * n / (n - mse_ridge$df[i])
  mse_ridge$Cp[i] <- mse_ridge$mse[i] + 2 * mse_ridge$sigma2[i] / n * mse_ridge$df[i]
}

colnames(data_ridge)[-1] <- colnames(X)
data_ridge <- tidyr::gather(data.frame(data_ridge), lambda, value, lcavol:pgg45)
colnames(data_ridge) <- c("lambda", "Covariate", "value")

ggplot(data = data_ridge, aes(x = lambda, y = value, col = Covariate)) +
  geom_point() +
  geom_line() +
  theme_light() +
  theme(legend.position = "top") +
  scale_x_log10() +
  scale_color_tableau(palette = "Color Blind") +
  xlab(expression(lambda)) +
  ylab("Regression coefficients")


#| fig-width: 5
#| fig-height: 4
df_min_cp <- mse_ridge$df[which.min(mse_ridge$Cp)]
lambda_min <- mse_ridge$lambda_seq[which.min(mse_ridge$Cp)]

ggplot(data = mse_ridge, aes(x = df, y = Cp)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = df_min_cp, linetype = "dotted") +
  theme_light() +
  scale_x_continuous(breaks = 0:8) +
  xlab("Effective degrees of freedom (df)") +
  ylab(expression(C[p]))


#| fig-width: 5
#| fig-height: 4
ggplot(data = mse_ridge, aes(x = lambda_seq, y = Cp)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = lambda_min, linetype = "dotted") +
  theme_light() +
  scale_x_log10() +
  xlab(expression(lambda)) +
  ylab(expression(C[p]))


#| message: false
resid_ridge <- matrix(0, n, length(lambda_seq))

for (k in 1:10) {
  # Hold-out dataset
  X_test_k <- as.matrix(subset(assessment(cv_fold$splits[[k]]), select = -c(lpsa)))
  y_test_k <- assessment(cv_fold$splits[[k]])$lpsa

  X_train_k <- as.matrix(subset(analysis(cv_fold$splits[[k]]), select = -c(lpsa)))
  y_train_k <- analysis(cv_fold$splits[[k]])$lpsa

  for (j in 1:length(lambda_seq)) {
    # Estimates
    beta_hat <- my_ridge(X_train_k, y_train_k, lambda = lambda_seq[j], standardize = FALSE)
    # Predictions
    y_hat <- cbind(1, X_test_k) %*% beta_hat
    # MSE of the best models for different values of lambda
    resid_ridge[complement(cv_fold$splits[[k]]), j] <- y_test_k - y_hat
  }
}


#| fig-width: 10
#| fig-height: 5
#| fig-align: center

data_cv <- data.frame(
  lambda = lambda_seq,
  df = df_ridge(lambda_seq, X, standardize = FALSE),
  MSE = apply(resid_ridge^2, 2, mean),
  SE = apply(resid_ridge^2, 2, function(x) sd(x) / sqrt(n))
)

se_rule <- data_cv$MSE[which.min(data_cv$MSE)] + data_cv$SE[which.min(data_cv$MSE)]
lambda_min_cv <- lambda_seq[tail(which(data_cv$MSE < se_rule), 1)]
df_min_cv <- data_cv$df[tail(which(data_cv$MSE < se_rule), 1)]

ggplot(data = data_cv, aes(x = df, y = MSE)) +
  geom_point() +
  geom_line() +
  geom_linerange(aes(ymax = MSE + SE, ymin = MSE - SE)) +
  scale_x_continuous(breaks = 0:8) +
  geom_hline(yintercept = se_rule, linetype = "dotted") +
  geom_vline(xintercept = df_min_cv, linetype = "dotted") +
  theme_light() +
  xlab("Effective degrees of freedom (df)") +
  ylab("Mean squared error (10-fold cv)")


#| fig-width: 9
#| fig-height: 5
#| fig-align: center
ggplot(data = data_ridge, aes(x = df_ridge(lambda, X), y = value, col = Covariate)) +
  geom_point() +
  geom_line() +
  theme_light() +
  geom_vline(xintercept = df_min_cv, linetype = "dashed") +
  geom_vline(xintercept = df_min_cp, linetype = "dotted") +
  scale_x_continuous(breaks = 0:8) +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Effective degrees of freedom (df)") +
  ylab("Regression coefficients")



# l = 10
#
# my_ridge(X, y, lambda = l)
# coef(glmnet(X, y, alpha = 0, lambda = l/n, thresh = 1e-20))
#
# y_std <-  scale(y, center=TRUE, scale=sd(y)*sqrt((n-1)/n) )[,]
# my_ridge(X, y_std, lambda = l, standardize = TRUE)
# as.numeric(coef(glmnet(X, y_std, alpha=0, lambda = l/(n), thresh = 1e-20)))




#

#

# lambda <- 100

# XX <- X[,-1] #scale(X[, -1], TRUE, scale = apply(X[, -1], 2, function(x)  sqrt(mean(x^2) - mean(x)^2)))

# yy <- y #(y - mean(y)) / sqrt(mean(y^2) - mean(y)^2)

#

# cv_ridge_fit <- cv.glmnet(XX, yy, family = "gaussian", standardize = FALSE, lambda = exp(seq(-10, 12, length = 500)),

#                     alpha = 0, thresh = 1e-16)

# plot(cv_ridge_fit)

#

# c(solve((crossprod(XX) + lambda * diag(p-1)), crossprod(XX, yy)))

# c(coef(fit_ridge)[-1, ])




# plot(log(cv_ridge_fit$lambda), cv_ridge_fit$cvm, type = "l")

# plot(1 + df_ridge(nrow(X[, -1]) * cv_ridge_fit$lambda, X[, -1]),

#      cv_ridge_fit$cvm, type = "b", xlab = "Model complexity", ylab = "MSE")

# lines(1 + df_ridge(nrow(X[, -1]) * cv_ridge_fit$lambda, X[, -1]),

#       cv_ridge_fit$cvup, type = "b", xlab = "Model complexity", ylab = "MSE", lty = "dashed", col = "red")

#

# 1 + df_ridge(nrow(X[, -1]) * cv_ridge_fit$lambda.min, X[, -1])

# 1 + df_ridge(nrow(X[, -1]) * cv_ridge_fit$lambda.1se, X[, -1])

#

# ridge_fit <- cv_ridge_fit$glmnet.fit

# coef(ridge_fit)

# plot(ridge_fit, , label = TRUE)
