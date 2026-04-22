# ----------------------------------------
# Title: LAB 3 (Ames Housing, regression)
# Author: Tommaso Rigon
# ----------------------------------------

rm(list = ls())

ames <- read.table("../data/ames.csv", header = TRUE, sep = ",", 
                   stringsAsFactors = TRUE)
source("routines.R")

# Training, validation and test set ----------------------------------------------------------------------

library(tidymodels)
glimpse(ames)

main_rec <- recipe(SalePrice ~ ., data = ames) %>% 
  step_nzv(all_predictors(), freq_cut = 95/5, unique_cut = 10) 

ames <- bake(prep(main_rec), new_data = ames) 

# Simple training and test
set.seed(123)
split <- initial_validation_split(ames, prop = c(0.6, 0.2))

ames_tr <- training(split)
ames_val <- validation(split)
ames_te <- testing(split) # kept untouched until the very end

glimpse(ames_tr)

# Some initial plots ------------------------------------------------------------------------------------

ggplot(ames_tr, aes(x = Gr.Liv.Area, y = SalePrice)) +
  geom_point(alpha = 0.4) + theme_bw() +
  labs(x = "Ground living area", y = "Sale Price")

ggplot(ames_tr, aes(x = Total.Bsmt.SF, y = SalePrice)) +
  geom_point(alpha = 0.4) + theme_bw() +
  labs(x = "Total Basement SF", y = "Sale Price")

ggplot(ames_tr, aes(x = Porch.SF, y = SalePrice)) +
  geom_point(alpha = 0.4) + theme_bw() +
  labs(x = "Porch Square Feet", y = "Sale Price")

ggplot(ames_tr, aes(x = Tot.Bath, y = SalePrice)) +
  geom_point(alpha = 0.4) + theme_bw() +
  labs(x = "Total Bathrooms", y = "Sale Price")

ggplot(ames_tr, aes(x = House.Age, y = SalePrice)) +
  geom_point(alpha = 0.4) + theme_bw() +
  labs(x = "House Age (Years)", y = "Sale Price")

ggplot(ames_tr, aes(x = factor(Overall.Qual), y = SalePrice)) +
  geom_boxplot() + theme_bw() +
  labs(x = "Overall Quality", y = "Sale Price")

ggplot(ames_tr, aes(x = Bsmt.Qual, y = SalePrice)) +
  geom_boxplot() + theme_bw() +
  labs(x = "Basement Quality", y = "Sale Price")

ggplot(ames_tr, aes(x = Exter.Qual, y = SalePrice)) +
  geom_boxplot() + theme_bw() +
  labs(x = "Exterior Quality", y = "Sale Price")

ggplot(ames_tr, aes(x = Kitchen.Qual, y = SalePrice)) +
  geom_boxplot() + theme_bw() +
  labs(x = "Kitchen Quality", y = "Sale Price")

ggplot(ames_tr, aes(x = MS.Zoning, y = SalePrice)) +
  geom_boxplot() + theme_bw() +
  labs(x = "MS Zoning", y = "Sale Price")

ggplot(ames_tr, aes(x = Roof.Style, y = SalePrice)) +
  geom_boxplot() + theme_bw() +
  labs(x = "Roof Style", y = "Sale Price")

ggplot(ames_tr, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() + theme_bw() +
  labs(x = "Neighborhood", y = "Sale Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Setting a benchmark ---------------------------------------------------------------------

MAE <- function(y, y_fit) {
  mean(abs(y - y_fit))
}

MSLE <- function(y, y_fit) {
  mean((log(y) - log(y_fit))^2)
}

# This is the median prediction, the optimal value if covariates were not available
y_hat_median <- rep(median(ames_tr$SalePrice), nrow(ames_val)) # Prediction

round(MAE(ames_val$SalePrice, y_hat_median), 4)
round(MSLE(ames_val$SalePrice, y_hat_median), 4)

# A first simple model --------------------------------------------------------------------------
m_simple <- lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + House.Age + Tot.Bath, data = ames_tr)
summary(m_simple)

y_hat_simple <- predict(m_simple, newdata = ames_val)

# Perform a small correction:
y_hat_simple <- pmax(y_hat_simple, 30000)

round(MAE(ames_val$SalePrice, y_hat_simple), 4)
round(MSLE(ames_val$SalePrice, y_hat_simple), 4)

# Taking the log scale -----------------------------------------------------------------------------------
m_simple <- lm(log(SalePrice) ~ Overall.Qual + Gr.Liv.Area + House.Age + Tot.Bath, data = ames_tr)
summary(m_simple)

# Re-obtain the original scale
y_hat_simple <- exp(predict(m_simple, newdata = ames_val))

round(MAE(ames_val$SalePrice, y_hat_simple), 4)
round(MSLE(ames_val$SalePrice, y_hat_simple), 4)

# A larger model -----------------------------------------------------------------------------------------

# How many variables are involved?
dim(model.matrix(log(SalePrice) ~ ., data = ames_tr)[, -1])

# Linear regression model with all the covariates (some of them are going to be redundant!)
m_full <- lm(log(SalePrice) ~ ., data = ames_tr)
summary(m_full)

# Predictions for the full model. This command, due to collinearity, will produced warnings!
y_hat_full <- exp(predict(m_full, newdata = ames_val))

round(MAE(ames_val$SalePrice, y_hat_full), 5)
round(MSLE(ames_val$SalePrice, y_hat_full), 5)

# Forward and backward regression ----------------------------------------------------

library(leaps)

# Maximum number of covariates included in the list ()
p_max <- 113

# There are some collinear variables, therefore this will produce warnings!
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

# Let us see what happens at the lowest levels
which(sum_backward$which[1, ]) # Model with one covariate
which(sum_backward$which[2, ]) # Model with two covariates
which(sum_backward$which[3, ]) # Model with three covariates
which(sum_backward$which[4, ]) # Model with four covariates

round(coef(m_backward, 1, ames_tr), 6)
round(coef(m_backward, 2, ames_tr), 6)
round(coef(m_backward, 3, ames_tr), 6)
round(coef(m_backward, 4, ames_tr), 6)

# Let see out it works
head(exp(predict(m_backward, data = ames_tr, newdata = ames_tr, id = 2)))

# Validation set - selection of p and performance comparisons ----------------------------------------
resid_back <- matrix(0, nrow(ames_val), p_max + 1)
resid_log_back <- matrix(0, nrow(ames_val), p_max + 1)

# We first comput the null model
resid_back[, 1] <- ames_val$SalePrice - exp(predict(lm(log(SalePrice) ~ 1, data = ames_tr), newdata = ames_val))
resid_log_back[, 1] <- log(ames_val$SalePrice) - predict(lm(log(SalePrice) ~ 1, data = ames_tr), newdata = ames_val)

for (j in 2:(p_max + 1)) {
  y_hat <- exp(predict(m_backward, data = ames_tr, newdata = ames_val, j - 1))
  resid_back[, j] <- ames_val$SalePrice - y_hat
  resid_log_back[, j] <- log(ames_val$SalePrice) - log(y_hat)
}

# Displaying the results
data_cv <- data.frame(
  p = 0:p_max,
  MAE = apply(resid_back, 2, function(x) mean(abs(x))),
  MSLE = apply(resid_log_back^2, 2, function(x) mean(x))
)

p_back_optimal <- data_cv$p[which.min(data_cv$MAE)]
p_back_optimal

par(mfrow = c(1, 2))
plot(data_cv$p, data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = "p")
abline(v = p_back_optimal, lty = "dashed")
abline(h = MAE(ames_val$SalePrice, y_hat_simple), lty = "dotted")

plot(data_cv$p, data_cv$MSLE, type = "b", pch = 16, cex = 0.6, ylab = "MSLE", xlab = "p")
abline(v = p_back_optimal, lty = "dashed")
abline(h = MSLE(ames_val$SalePrice, y_hat_simple), lty = "dotted")

# Optimal model on the validation set
y_hat_back <- exp(predict(m_backward, data = ames_tr, newdata = ames_val, id = p_back_optimal))

MAE(ames_val$SalePrice, y_hat_back)
MSLE(ames_val$SalePrice, y_hat_back)

# Principal components regression ----------------------------------------------------------------------

library(pls)

m_pcr <- pcr(log(SalePrice) ~ ., data = ames_tr, center = TRUE, scale = TRUE)
summary(m_pcr)

resid_pcr <- matrix(0, nrow(ames_val), p_max + 1)
resid_log_pcr <- matrix(0, nrow(ames_val), p_max + 1)

# We first comput the null model
resid_pcr[, 1] <- resid_back[, 1]
resid_log_pcr[, 1] <- resid_log_back[, 1]

y_hat_pcr <- exp(predict(m_pcr, newdata = ames_val))
for (j in 2:(p_max + 1)) {
  resid_pcr[, j] <- ames_val$SalePrice - y_hat_pcr[, , j - 1]
  resid_log_pcr[, j] <- log(ames_val$SalePrice) - log(y_hat_pcr[, , j - 1])
}

data_cv <- data.frame(
  p = 0:p_max,
  MAE = apply(resid_pcr, 2, function(x) mean(abs(x))),
  MSLE = apply(resid_log_pcr^2, 2, function(x) mean(x))
)

p_pcr_optimal <- data_cv$p[which.min(data_cv$MAE)]
p_pcr_optimal

# Plots on the validation set
plot(data_cv$p, data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = "p")
abline(v = p_pcr_optimal, lty = "dashed")
abline(h = MAE(ames_val$SalePrice, y_hat_simple), lty = "dotted")

plot(data_cv$p, data_cv$MSLE, type = "b", pch = 16, cex = 0.6, ylab = "MSLE", xlab = "p")
abline(v = p_pcr_optimal, lty = "dashed")
abline(h = MSLE(ames_val$SalePrice, y_hat_simple), lty = "dotted")

# Optimal model on the validation set
MAE(ames_val$SalePrice, y_hat_pcr[, , p_pcr_optimal])
MSLE(ames_val$SalePrice, y_hat_pcr[, , p_pcr_optimal])

# Ridge regression ----------------------------------------------------------------------

library(glmnet)

# The lambda parameter can be then conveniently selected via cross-validation
X_shrinkage <- model.matrix(SalePrice ~ ., data = ames_tr)[, -1]
y_shrinkage <- ames_tr$SalePrice

# We need to set alpha = 0 to use the ridge
lambda_ridge_grid <- exp(seq(-6, 6, length = 100))
m_ridge <- glmnet(X_shrinkage, log(y_shrinkage), alpha = 0, lambda = lambda_ridge_grid)

par(mfrow = c(1, 1))
plot(m_ridge, xvar = "lambda")

# How to select the "optimal" lambda?
resid_ridge <- matrix(0, nrow(ames_val), length(lambda_ridge_grid))
resid_log_ridge <- matrix(0, nrow(ames_val), length(lambda_ridge_grid))

y_hat_ridge <- exp(predict(m_ridge, newx = model.matrix(SalePrice ~ ., data = ames_val)[, -1], s = lambda_ridge_grid))
for (j in 1:length(lambda_ridge_grid)) {
  resid_ridge[, j] <- ames_val$SalePrice - y_hat_ridge[, j]
  resid_log_ridge[, j] <- log(ames_val$SalePrice) - log(y_hat_ridge[, j])
}

data_cv <- data.frame(
  lambda = lambda_ridge_grid,
  MAE = apply(resid_ridge, 2, function(x) mean(abs(x))),
  MSLE = apply(resid_log_ridge^2, 2, function(x) mean(x))
)

lambda_ridge_optimal <- lambda_ridge_grid[which.min(data_cv$MSLE)]
lambda_ridge_optimal

par(mfrow = c(1, 2))
plot(log(data_cv$lambda), data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = expression(log(lambda)))
abline(v = log(lambda_ridge_optimal), lty = "dashed")

plot(log(data_cv$lambda), data_cv$MSLE, type = "b", pch = 16, cex = 0.6, ylab = "MSLE", xlab = "p")
abline(v = log(lambda_ridge_optimal), lty = "dashed")

# Optimal model on the validation set
y_hat_ridge <- exp(predict(m_ridge, newx = model.matrix(SalePrice ~ ., data = ames_val)[, -1], s = lambda_ridge_optimal))

MAE(ames_val$SalePrice, y_hat_ridge)
MSLE(ames_val$SalePrice, y_hat_ridge)

## Cross-validation for ridge regression
ridge_cv <- cv.glmnet(X_shrinkage, log(y_shrinkage), alpha = 0, lambda = lambda_ridge_grid)
par(mfrow = c(1, 1))
plot(ridge_cv)

ridge_cv$lambda.min
ridge_cv$lambda.1se

# MSLE for lambda.min and lambda.1se
ridge_cv$cvm[ridge_cv$index]

## LARS --------------------------------------------------------------------------
library(lars)

m_lar <- lars(X_shrinkage, log(y_shrinkage), type = "lar")

# Order of inclusion
m_lar

# Coefficient path
plot(m_lar, breaks = FALSE)

plot(m_lar$df, m_lar$Cp, type = "b", xlab = "Degrees of freedom", ylab = "Cp of Mallow")
abline(v = which.min(m_lar$Cp))

y_hat_lar <- exp(predict(m_lar, newx = model.matrix(SalePrice ~ ., data = ames_val)[, -1], 
                         s = which.min(m_lar$Cp))$fit)

# Optimal model on the validation set
MAE(ames_val$SalePrice, y_hat_lar)
MSLE(ames_val$SalePrice, y_hat_lar)

# Cross-validation for the lar
lar_cv <- cv.lars(X_shrinkage, log(y_shrinkage), plot.it = TRUE)

## Elastic-net -----------------------------------------------------------------------------------------

# We need to set (for example) alpha = 0.5 to select the elastic-net penalty. Any 0 < alpha < 1 would use an elastic-net penalty.
lambda_en_grid <- exp(seq(-10, 0, length = 100))
m_en <- glmnet(X_shrinkage, log(y_shrinkage), alpha = 0.5, lambda = lambda_en_grid)

# Coefficient path
plot(m_en, xvar = "lambda")

# How to select the "optimal" lambda?
resid_en <- matrix(0, nrow(ames_val), length(lambda_en_grid))
resid_log_en <- matrix(0, nrow(ames_val), length(lambda_en_grid))

y_hat_en <- exp(predict(m_en, newx = model.matrix(SalePrice ~ ., data = ames_val)[, -1], s = lambda_en_grid))
for (j in 1:length(lambda_en_grid)) {
  resid_en[, j] <- ames_val$SalePrice - y_hat_en[, j]
  resid_log_en[, j] <- log(ames_val$SalePrice) - log(y_hat_en[, j])
}

data_cv <- data.frame(
  lambda = lambda_en_grid,
  MAE = apply(resid_en, 2, function(x) mean(abs(x))),
  MSLE = apply(resid_log_en^2, 2, function(x) mean(x))
)

lambda_en_optimal <- lambda_en_grid[which.min(data_cv$MSLE)]
lambda_en_optimal

par(mfrow = c(1, 2))
plot(log(data_cv$lambda), data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = expression(log(lambda)))
abline(v = log(lambda_en_optimal), lty = "dashed")

plot(log(data_cv$lambda), data_cv$MSLE, type = "b", pch = 16, cex = 0.6, ylab = "MSLE", xlab = "p")
abline(v = log(lambda_en_optimal), lty = "dashed")

# Optimal model on the validation set
y_hat_en <- exp(predict(m_en, newx = model.matrix(SalePrice ~ ., data = ames_val)[, -1], s = lambda_en_optimal))

MAE(ames_val$SalePrice, y_hat_en)
MSLE(ames_val$SalePrice, y_hat_en)

## Cross-validation for elastic-net
en_cv <- cv.glmnet(X_shrinkage, log(y_shrinkage), alpha = 0.5, lambda = lambda_en_grid)

par(mfrow = c(1, 1))
plot(en_cv)

en_cv$lambda.min
en_cv$lambda.1se

# MSLE for lambda.min and lambda.1se
en_cv$cvm[en_cv$index]

## Random forests (spoiler!) ------------------------------------------------------------------------------
library(ranger)
m_rf <- ranger(log(SalePrice) ~ ., data = ames_tr, num.trees = 2000, mtry = 10, max.depth = 30)
y_hat_rf <- exp(predict(m_rf, data = ames_val, type = "response")$predictions)

MAE(ames_val$SalePrice, y_hat_rf)
MSLE(ames_val$SalePrice, y_hat_rf)

# Final choice --------------------------------------------------------------------------------------------

# Null
y_hat_median <- rep(median(ames_tr$SalePrice), times = nrow(ames_te))

# Simple
y_hat_simple <- exp(predict(m_simple, newdata = ames_te))

# Full
y_hat_full <- exp(predict(m_full, newdata = ames_te))

# Backward
y_hat_back <- exp(predict(m_backward, data = ames_tr, newdata = ames_te, id = p_back_optimal))

# PCR
y_hat_pcr <- exp(predict(m_pcr, newdata = ames_te))[, , p_pcr_optimal]

# Ridge
y_hat_ridge <- exp(predict(m_ridge, newx = model.matrix(SalePrice ~ ., data = ames_te)[, -1], s = lambda_en_optimal))

# LAR
y_hat_lar <- exp(predict(m_lar, newx = model.matrix(SalePrice ~ ., data = ames_te)[, -1], s = which.min(m_lar$Cp))$fit)

# Elastic net
y_hat_en <- exp(predict(m_en, newx = model.matrix(SalePrice ~ ., data = ames_te)[, -1], s = lambda_en_optimal))

# Random forest
y_hat_rf <- exp(predict(m_rf, data = ames_te, type = "response")$predictions)

# Final summary of the results ----------------------------------------------
n_test <- nrow(ames_te)
final_summary <- data.frame(
  Predictions = c(y_hat_median, y_hat_simple, y_hat_full, y_hat_back, y_hat_pcr, y_hat_ridge, y_hat_lar, y_hat_en,  y_hat_rf),
  Model = rep(c("Median", "Simple", "Full model", "Backward regression", "PCR", "Ridge", "Lar", "Elastic net",  "Random Forest"), each = n_test),
  Truth = ames_te$SalePrice
)
final_summary$Errors <- final_summary$Predictions - final_summary$Truth

par(mfrow = c(1, 1))
boxplot(Errors ~ Model, data = final_summary)

# MAE
tapply(final_summary$Errors, final_summary$Model, function(x) mean(abs(x)))

# Standard deviations
tapply(final_summary$Errors, final_summary$Model, sd)

# Interquartile range
tapply(final_summary$Errors, final_summary$Model, function(x) diff(quantile(x, probs = c(0.25, 0.75))))

