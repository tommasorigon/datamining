# ----------------------------------------
# Title: LAB 3 (Ames Housing, regression)
# Author: Tommaso Rigon
# ----------------------------------------

rm(list = ls())

library(tidyverse)
library(tidymodels)
source("https://tommasorigon.github.io/datamining/code/routines.R", echo = TRUE)

# Reading the clean data
ames <- read_csv("https://tommasorigon.github.io/datamining/data/ames.csv")

# Preprocessing and three-way split: 50% train / 25% validation / 25% test ----------------------
# Near-zero variance predictors are removed. The outcome is log(SalePrice), used directly
# in the model formulas below; SalePrice is retained for evaluation on the original scale.

main_rec <- recipe(SalePrice ~ ., data = ames) %>%
  step_nzv(all_predictors(), unique_cut = 10)

ames <- bake(prep(main_rec), new_data = ames)

set.seed(123)
split <- initial_validation_split(ames, prop = c(0.5, 0.25))

ames_tr <- training(split)
ames_val <- validation(split)
ames_te <- testing(split) # kept untouched until the very end

glimpse(ames_tr)


# EDA plots ---------------------------------------------------------------------------------------------

numeric_vars <- c(
  "Gr.Liv.Area", "Total.Bsmt.SF",
  "Porch.SF", "Tot.Bath", "House.Age"
)
factor_vars <- c(
  "Overall.Qual", "Bsmt.Qual", "Exter.Qual",
  "Kitchen.Qual", "MS.Zoning", "Roof.Style", "Neighborhood"
)

ames_tr %>%
  select(SalePrice, all_of(numeric_vars)) %>%
  pivot_longer(-SalePrice) %>%
  ggplot(aes(value, SalePrice)) +
  geom_point(alpha = 0.4, size = 0.8) +
  facet_wrap(~name, scales = "free_x") +
  labs(title = "SalePrice vs numeric predictors") +
  theme_light()

ames_tr %>%
  select(SalePrice, all_of(factor_vars)) %>%
  mutate(across(all_of(factor_vars), as.factor)) %>%
  pivot_longer(-SalePrice) %>%
  ggplot(aes(value, SalePrice)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "SalePrice vs categorical predictors") +
  theme_light()


# Setting a benchmark ---------------------------------------------------------------------

MAE <- function(y, y_fit) {
  mean(abs(y - y_fit))
}

MSLE <- function(y, y_fit) {
  mean((log(y) - log(y_fit))^2)
}

# Median prediction: optimal rule when no covariates are available
y_hat_median <- rep(median(ames_tr$SalePrice), nrow(ames_val))

round(MAE(ames_val$SalePrice, y_hat_median), 4)
round(MSLE(ames_val$SalePrice, y_hat_median), 4)


# A first simple model --------------------------------------------------------------------------

m_simple <- lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + House.Age + Tot.Bath, data = ames_tr)
summary(m_simple)

y_hat_simple <- predict(m_simple, newdata = ames_val)

# Truncate predictions below 30000 to avoid implausible negative or near-zero values
y_hat_simple <- pmax(y_hat_simple, 30000)

round(MAE(ames_val$SalePrice, y_hat_simple), 4)
round(MSLE(ames_val$SalePrice, y_hat_simple), 4)


# Taking the log scale -----------------------------------------------------------------------------------

m_simple <- lm(log(SalePrice) ~ Overall.Qual + Gr.Liv.Area + House.Age + Tot.Bath, data = ames_tr)
summary(m_simple)

# Back-transform predictions to the original dollar scale
y_hat_simple <- exp(predict(m_simple, newdata = ames_val))

round(MAE(ames_val$SalePrice, y_hat_simple), 4)
round(MSLE(ames_val$SalePrice, y_hat_simple), 4)


# A larger model -----------------------------------------------------------------------------------------

# Number of columns in the full design matrix
dim(model.matrix(log(SalePrice) ~ ., data = ames_tr)[, -1])

# Full model: collinear columns will trigger warnings from lm()
m_full <- lm(log(SalePrice) ~ ., data = ames_tr)
summary(m_full)

# Predictions for the full model (warnings expected due to collinearity)
y_hat_full <- exp(predict(m_full, newdata = ames_val))

round(MAE(ames_val$SalePrice, y_hat_full), 5)
round(MSLE(ames_val$SalePrice, y_hat_full), 5)


# Forward and backward regression ----------------------------------------------------

library(leaps)

# Maximum number of covariates
p_max <- length(m_full$coefficients) - 1

# Collinear variables will produce warnings
m_forward <- regsubsets(log(SalePrice) ~ .,
  data = ames_tr, method = "forward", nbest = 1, nvmax = p_max, really.big = TRUE
)
sum_forward <- summary(m_forward)

m_backward <- regsubsets(log(SalePrice) ~ .,
  data = ames_tr, method = "backward", nbest = 1, nvmax = p_max
)
sum_backward <- summary(m_backward)

m_forward_summary <- m_forward %>%
  tidy() %>%
  rowwise() %>%
  mutate(p = sum(c_across(MS.SubClass160:House.Age)), .keep = "unused") %>%
  ungroup()

m_backward_summary <- m_backward %>%
  tidy() %>%
  rowwise() %>%
  mutate(p = sum(c_across(MS.SubClass160:House.Age)), .keep = "unused") %>%
  ungroup()

# Inspect the first few backward models
which(sum_backward$which[1, ])
which(sum_backward$which[2, ])
which(sum_backward$which[3, ])
which(sum_backward$which[4, ])

round(coef(m_backward, 1, ames_tr), 6)
round(coef(m_backward, 2, ames_tr), 6)
round(coef(m_backward, 3, ames_tr), 6)
round(coef(m_backward, 4, ames_tr), 6)

# Quick sanity check on in-sample predictions
head(exp(predict(m_backward, data = ames_tr, newdata = ames_tr, id = 2)))


# Validation set — selection of p and performance comparisons ----------------------------------------

resid_back <- matrix(0, nrow(ames_val), p_max + 1)
resid_log_back <- matrix(0, nrow(ames_val), p_max + 1)

# Null model (p = 0)
resid_back[, 1] <- ames_val$SalePrice - exp(predict(lm(log(SalePrice) ~ 1, data = ames_tr), newdata = ames_val))
resid_log_back[, 1] <- log(ames_val$SalePrice) - predict(lm(log(SalePrice) ~ 1, data = ames_tr), newdata = ames_val)

for (j in 2:(p_max + 1)) {
  y_hat <- exp(predict(m_backward, data = ames_tr, newdata = ames_val, j - 1))
  resid_back[, j] <- ames_val$SalePrice - y_hat
  resid_log_back[, j] <- log(ames_val$SalePrice) - log(y_hat)
}

data_cv <- data.frame(
  p    = 0:p_max,
  MAE  = apply(resid_back, 2, function(x) mean(abs(x))),
  MSLE = apply(resid_log_back^2, 2, mean)
)

p_back_optimal <- data_cv$p[which.min(data_cv$MAE)]
p_back_optimal

par(mfrow = c(1, 2))
plot(data_cv$p, data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = "p")
abline(v = p_back_optimal, lty = "dashed")
abline(h = MAE(ames_val$SalePrice, y_hat_simple), lty = "dotted")

plot(data_cv$p, data_cv$MSLE, type = "b", pch = 16, cex = 0.6, ylab = "MSLE (validation)", xlab = "p")
abline(v = p_back_optimal, lty = "dashed")
abline(h = MSLE(ames_val$SalePrice, y_hat_simple), lty = "dotted")

# Optimal backward model on the validation set
y_hat_back <- exp(predict(m_backward, data = ames_tr, newdata = ames_val, id = p_back_optimal))

MAE(ames_val$SalePrice, y_hat_back)
MSLE(ames_val$SalePrice, y_hat_back)


# Principal components regression ----------------------------------------------------------------------

library(pls)

m_pcr <- pcr(log(SalePrice) ~ ., data = ames_tr, center = TRUE, scale = TRUE)
summary(m_pcr)

resid_pcr <- matrix(0, nrow(ames_val), p_max + 1)
resid_log_pcr <- matrix(0, nrow(ames_val), p_max + 1)

# Null model: reuse residuals from backward section
resid_pcr[, 1] <- resid_back[, 1]
resid_log_pcr[, 1] <- resid_log_back[, 1]

y_hat_pcr <- exp(predict(m_pcr, newdata = ames_val))
for (j in 2:(p_max + 1)) {
  resid_pcr[, j] <- ames_val$SalePrice - y_hat_pcr[, , j - 1]
  resid_log_pcr[, j] <- log(ames_val$SalePrice) - log(y_hat_pcr[, , j - 1])
}

data_cv <- data.frame(
  p    = 0:p_max,
  MAE  = apply(resid_pcr, 2, function(x) mean(abs(x))),
  MSLE = apply(resid_log_pcr^2, 2, mean)
)

p_pcr_optimal <- data_cv$p[which.min(data_cv$MAE)]
p_pcr_optimal

par(mfrow = c(1, 2))
plot(data_cv$p, data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = "p")
abline(v = p_pcr_optimal, lty = "dashed")
abline(h = MAE(ames_val$SalePrice, y_hat_simple), lty = "dotted")

plot(data_cv$p, data_cv$MSLE, type = "b", pch = 16, cex = 0.6, ylab = "MSLE (validation)", xlab = "p")
abline(v = p_pcr_optimal, lty = "dashed")
abline(h = MSLE(ames_val$SalePrice, y_hat_simple), lty = "dotted")

# Optimal PCR model on the validation set
MAE(ames_val$SalePrice, y_hat_pcr[, , p_pcr_optimal])
MSLE(ames_val$SalePrice, y_hat_pcr[, , p_pcr_optimal])


# Ridge regression ----------------------------------------------------------------------

library(glmnet)

# Design matrix (centring/scaling handled internally by glmnet via standardize = TRUE, the default)
X_shrinkage <- model.matrix(SalePrice ~ ., data = ames_tr)[, -1]
y_shrinkage <- ames_tr$SalePrice

lambda_ridge_grid <- exp(seq(-6, 6, length = 100))
m_ridge <- glmnet(X_shrinkage, log(y_shrinkage), alpha = 0, lambda = lambda_ridge_grid)

par(mfrow = c(1, 1))
plot(m_ridge, xvar = "lambda")

# Select optimal lambda on the validation set
resid_ridge <- matrix(0, nrow(ames_val), length(lambda_ridge_grid))
resid_log_ridge <- matrix(0, nrow(ames_val), length(lambda_ridge_grid))

X_val <- model.matrix(SalePrice ~ ., data = ames_val)[, -1]
y_hat_ridge <- exp(predict(m_ridge, newx = X_val, s = lambda_ridge_grid))

for (j in seq_along(lambda_ridge_grid)) {
  resid_ridge[, j] <- ames_val$SalePrice - y_hat_ridge[, j]
  resid_log_ridge[, j] <- log(ames_val$SalePrice) - log(y_hat_ridge[, j])
}

data_cv <- data.frame(
  lambda = lambda_ridge_grid,
  MAE    = apply(resid_ridge, 2, function(x) mean(abs(x))),
  MSLE   = apply(resid_log_ridge^2, 2, mean)
)

lambda_ridge_optimal <- lambda_ridge_grid[which.min(data_cv$MSLE)]
lambda_ridge_optimal

par(mfrow = c(1, 2))
plot(log(data_cv$lambda), data_cv$MAE,
  type = "b", pch = 16, cex = 0.6,
  ylab = "MAE (validation)", xlab = expression(log(lambda))
)
abline(v = log(lambda_ridge_optimal), lty = "dashed")

plot(log(data_cv$lambda), data_cv$MSLE,
  type = "b", pch = 16, cex = 0.6,
  ylab = "MSLE (validation)", xlab = expression(log(lambda))
)
abline(v = log(lambda_ridge_optimal), lty = "dashed")

# Optimal ridge model on the validation set
y_hat_ridge <- exp(predict(m_ridge, newx = X_val, s = lambda_ridge_optimal))

MAE(ames_val$SalePrice, y_hat_ridge)
MSLE(ames_val$SalePrice, y_hat_ridge)

# Cross-validation for ridge (using only the training set)
ridge_cv <- cv.glmnet(X_shrinkage, log(y_shrinkage), alpha = 0, lambda = lambda_ridge_grid)
par(mfrow = c(1, 1))
plot(ridge_cv)

ridge_cv$lambda.min
ridge_cv$lambda.1se

# MSE (on log scale) for lambda.min and lambda.1se
ridge_cv$cvm[ridge_cv$index]


## LARS --------------------------------------------------------------------------

library(lars)

m_lar <- lars(X_shrinkage, log(y_shrinkage), type = "lar")

# Order of variable inclusion
m_lar

# Coefficient path
plot(m_lar, breaks = FALSE)

plot(m_lar$df, m_lar$Cp, type = "b", xlab = "Degrees of freedom", ylab = "Mallows' Cp")
abline(v = which.min(m_lar$Cp))

y_hat_lar <- exp(predict(m_lar,
  newx = model.matrix(SalePrice ~ ., data = ames_val)[, -1],
  s    = which.min(m_lar$Cp)
)$fit)

MAE(ames_val$SalePrice, y_hat_lar)
MSLE(ames_val$SalePrice, y_hat_lar)

# Cross-validation for LAR
lar_cv <- cv.lars(X_shrinkage, log(y_shrinkage), plot.it = TRUE)


## Elastic net (alpha = 0.5) -----------------------------------------------------------------------------------------

lambda_en_grid <- exp(seq(-10, 0, length = 100))
m_en <- glmnet(X_shrinkage, log(y_shrinkage), alpha = 0.5, lambda = lambda_en_grid)

# Coefficient path
plot(m_en, xvar = "lambda")

# Select optimal lambda on the validation set
resid_en <- matrix(0, nrow(ames_val), length(lambda_en_grid))
resid_log_en <- matrix(0, nrow(ames_val), length(lambda_en_grid))

y_hat_en <- exp(predict(m_en, newx = X_val, s = lambda_en_grid))

for (j in seq_along(lambda_en_grid)) {
  resid_en[, j] <- ames_val$SalePrice - y_hat_en[, j]
  resid_log_en[, j] <- log(ames_val$SalePrice) - log(y_hat_en[, j])
}

data_cv <- data.frame(
  lambda = lambda_en_grid,
  MAE    = apply(resid_en, 2, function(x) mean(abs(x))),
  MSLE   = apply(resid_log_en^2, 2, mean)
)

lambda_en_optimal <- lambda_en_grid[which.min(data_cv$MSLE)]
lambda_en_optimal

par(mfrow = c(1, 2))
plot(log(data_cv$lambda), data_cv$MAE,
  type = "b", pch = 16, cex = 0.6,
  ylab = "MAE (validation)", xlab = expression(log(lambda))
)
abline(v = log(lambda_en_optimal), lty = "dashed")

plot(log(data_cv$lambda), data_cv$MSLE,
  type = "b", pch = 16, cex = 0.6,
  ylab = "MSLE (validation)", xlab = expression(log(lambda))
)
abline(v = log(lambda_en_optimal), lty = "dashed")

# Optimal elastic net model on the validation set
y_hat_en <- exp(predict(m_en, newx = X_val, s = lambda_en_optimal))

MAE(ames_val$SalePrice, y_hat_en)
MSLE(ames_val$SalePrice, y_hat_en)

# Cross-validation for elastic net (using only the training set)
en_cv <- cv.glmnet(X_shrinkage, log(y_shrinkage), alpha = 0.5, lambda = lambda_en_grid)

par(mfrow = c(1, 1))
plot(en_cv)

en_cv$lambda.min
en_cv$lambda.1se

# MSE (on log scale) for lambda.min and lambda.1se
en_cv$cvm[en_cv$index]


## Random forests (spoiler!) ------------------------------------------------------------------------------

library(ranger)
m_rf <- ranger(log(SalePrice) ~ ., data = ames_tr, num.trees = 2000, mtry = 10, max.depth = 30)
y_hat_rf <- exp(predict(m_rf, data = ames_val, type = "response")$predictions)

MAE(ames_val$SalePrice, y_hat_rf)
MSLE(ames_val$SalePrice, y_hat_rf)


# Final comparison on the test set --------------------------------------------------------------------------------------------

y_hat_median <- rep(median(ames_tr$SalePrice), times = nrow(ames_te))
y_hat_simple <- exp(predict(m_simple, newdata = ames_te))
y_hat_full <- exp(predict(m_full, newdata = ames_te))
y_hat_back <- exp(predict(m_backward, data = ames_tr, newdata = ames_te, id = p_back_optimal))
y_hat_pcr <- exp(predict(m_pcr, newdata = ames_te))[, , p_pcr_optimal]
y_hat_ridge <- exp(predict(m_ridge, newx = model.matrix(SalePrice ~ ., data = ames_te)[, -1], s = lambda_ridge_optimal))
y_hat_lar <- exp(predict(m_lar, newx = model.matrix(SalePrice ~ ., data = ames_te)[, -1], s = which.min(m_lar$Cp))$fit)
y_hat_en <- exp(predict(m_en, newx = model.matrix(SalePrice ~ ., data = ames_te)[, -1], s = lambda_en_optimal))
y_hat_rf <- exp(predict(m_rf, data = ames_te, type = "response")$predictions)

n_test <- nrow(ames_te)
final_summary <- data.frame(
  Predictions = c(
    y_hat_median, y_hat_simple, y_hat_full, y_hat_back,
    y_hat_pcr, y_hat_ridge, y_hat_lar, y_hat_en, y_hat_rf
  ),
  Model = rep(c(
    "Median", "Simple", "Full model", "Backward regression",
    "PCR", "Ridge", "LAR", "Elastic net", "Random Forest"
  ), each = n_test),
  Truth = ames_te$SalePrice
)
final_summary$Errors <- final_summary$Predictions - final_summary$Truth

library(knitr)

final_summary %>%
  group_by(Model) %>%
  summarise(
    MAE = mean(abs(Errors)),
    SD  = sd(Errors),
    IQR = diff(quantile(Errors, c(0.25, 0.75)))
  ) %>%
  arrange(MAE) %>%
  kable(digits = 1)
