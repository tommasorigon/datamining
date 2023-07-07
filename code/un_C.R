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



library(leaps)

# Here I compute some basic quantities
X <- model.matrix(lpsa ~ ., data = prostate_train)
y <- prostate_train$lpsa
n <- nrow(X)
p <- ncol(X) # This includes the intercept as well



fit_best <- regsubsets(lpsa ~ ., data = prostate_train, method = "exhaustive", nbest = 10, nvmax = p)
sum_best <- summary(fit_best)
sum_best$p <- rowSums(sum_best$which)


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
  geom_point(alpha = 0.6) +
  theme_light() +
  theme(legend.position = "top") +
  geom_line(data = data_best_subset2, aes(x = p, y = MSE), col = "#fc7d0b") +
  geom_point(data = data_best_subset2, aes(x = p, y = MSE), col = "#fc7d0b", size = 2.5) +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Model complexity (p)") +
  ylab("MSE (training)")



library(rsample)

set.seed(123)
cv_fold <- vfold_cv(prostate_train, v = 10)
resid_subs <- matrix(0, n, p)

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
  for (j in 2:p) {
    y_hat <- X_k[, sum$which[j - 1, ]] %*% coef(fit, j - 1)
    resid_subs[complement(cv_fold$splits[[k]]), j] <- y_k - y_hat
  }
}


#| fig-width: 10
#| fig-height: 5
#| fig-align: center

data_cv <- data.frame(
  p = 1:p,
  MSE = apply(resid_subs^2, 2, mean),
  SE = apply(resid_subs^2, 2, function(x) sd(x) / sqrt(n))
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
  xlab("Model complexity (p)") +
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
  p = c(2:p, 2:p, 2:p), MSE = c(
    sum_forward$rss,
    sum_backward$rss,
    tapply(sum_best$rss, sum_best$p, min)
  ) / n,
  Stepwise = rep(c("Forward", "Backward", "Best subset"), each = p - 1)
)
data_stepwise <- reshape2::melt(data_stepwise, id = c("p", "Stepwise"))
colnames(data_stepwise) <- c("p", "Stepwise", "MSE", "value")

ggplot(data = data_stepwise, aes(x = p, y = value, col = Stepwise)) +
  geom_line() +
  geom_point() +
  facet_grid(. ~ Stepwise) +
  theme_light() +
  theme(legend.position = "none") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Model complexity (p)") +
  ylab("MSE (training)")



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
  xlab("Model complexity (p)") +
  ylab("Mean squared error (10-fold cv)")



library(DT)

tab <- data.frame(OLS = rep(0, p), best_subset = rep(0, p), PCR = rep(0, p))
rownames(tab) <- colnames(sum_best$which)

tab$OLS <- coef(lm(lpsa ~ ., data = prostate_train))
tab$best_subset <- c(coef(lm(lpsa ~ lcavol + lweight, data = prostate_train)), rep(0, 6))

# Principal components regression (PCR)
fit_pcr <- pcr(lpsa ~ ., data = prostate_train, center = TRUE, scale = FALSE)
beta <- c(coef(fit_pcr, 3))
beta <- c(mean(prostate_train$lpsa) - colMeans(X[, -1]) %*% beta, beta)
tab$PCR <- beta

datatable(tab, colnames = c("OLS", "Best subset", "PCR"), options = list(
  pageLength = 9,
  dom = "t"
)) %>%
  formatRound(columns = 1:3, digits = 3) %>%
  formatStyle(
    columns = 0, fontWeight = "bold"
  ) %>%
  formatStyle(
    columns = 1:3,
    backgroundColor = styleInterval(0, c("#FED8B1", "#DBE9FA"))
  ) %>%
  formatStyle(
    columns = 1:3,
    backgroundColor = styleEqual(0, c("white"))
  )



df_ridge <- function(lambda, X) {
  X_tilde <- scale(X, TRUE, FALSE)
  d2 <- eigen(crossprod(X_tilde))$values
  sum(d2 / (d2 + lambda))
}
df_ridge <- Vectorize(df_ridge, vectorize.args = "lambda")



library(glmnet)
my_ridge <- function(X, y, lambda) {
  n <- nrow(X)
  p <- ncol(X)
  y_mean <- mean(y)
  y <- y - y_mean
  X_mean <- colMeans(X)
  X <- X - rep(1, n) %*% t(X_mean)
  X_scale <- sqrt(diag((1 / n) * crossprod(X)))
  X <- X %*% diag(1 / X_scale)
  beta_scaled <- solve(crossprod(X) + lambda * diag(rep(1, p)), t(X) %*% y)
  beta <- diag(1 / X_scale) %*% beta_scaled
  beta0 <- y_mean - X_mean %*% beta
  return(c(beta0, beta))
}

l <- 1
my_ridge(X, y, lambda = l)
coef(glmnet(X, y, alpha = 0, lambda = l / n, thresh = 1e-20))

y_std <- scale(y, center = TRUE, scale = sd(y) * sqrt((n - 1) / n))[, ]
my_ridge(X, y_std, lambda = l)
coef(glmnet(X, y_std, alpha = 0, lambda = l / n, thresh = 1e-20))



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
#      cv_ridge_fit$cvm, type = "b", xlab = "Model complexity (p)", ylab = "MSE")
# lines(1 + df_ridge(nrow(X[, -1]) * cv_ridge_fit$lambda, X[, -1]),
#       cv_ridge_fit$cvup, type = "b", xlab = "Model complexity (p)", ylab = "MSE", lty = "dashed", col = "red")
#
# 1 + df_ridge(nrow(X[, -1]) * cv_ridge_fit$lambda.min, X[, -1])
# 1 + df_ridge(nrow(X[, -1]) * cv_ridge_fit$lambda.1se, X[, -1])
#
# ridge_fit <- cv_ridge_fit$glmnet.fit
# coef(ridge_fit)
# plot(ridge_fit, , label = TRUE)



library(lars)
lambda <- 100
XX <- scale(X[, -1], TRUE, scale = apply(X[, -1], 2, function(x) sqrt(mean(x^2) - mean(x)^2)))
yy <- y # (y - mean(y)) / sqrt(mean(y^2) - mean(y)^2)

cv_lars <- cv.lars(x = XX, y = yy, K = 10, type = "lasso", mode = "step")
fit_lars <- lars(x = XX, y = yy, type = "lasso", normalize = FALSE)

cv_lasso_fit <- cv.glmnet(XX, yy,
  standardize = FALSE,
  family = "gaussian", alpha = 1, nfolds = 10,
  lambda = 1 / n * fit_lars$lambda, thresh = 1e-16
)
plot(cv_lasso_fit)
lasso_fit <- cv_lasso_fit$glmnet.fit
lambda_sel <- 4
round(coef(fit_lars)[lambda_sel, ], 5)
round(coef(lasso_fit, mode = "lambda")[-1, lambda_sel], 5)
