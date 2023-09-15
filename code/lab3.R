# -----------------------------------
# LAB 3 (Ames Housing)
# Course: Data Mining
# Author: Tommaso Rigon
# --------------------------------

# Predictive analysis ----------------------------------------------------------------------------

rm(list = ls())
ames <- read.table("../data/ames.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

# Training, validation and test set ----------------------------------------------------------------------------

# Manual subdivision in training / test
set.seed(123)
# Randomly allocate the id of the variables into training and test
id_train <- sort(sample(1:nrow(ames), size = floor(0.5 * nrow(ames)), replace = FALSE))
id_validation_test <- setdiff(1:nrow(ames), id_train)
# Now we allocate the validation test
id_validation <- sort(sample(id_validation_test, size = floor(0.5 * length(id_validation_test)), replace = FALSE))
# And finally the test set
id_test <- setdiff(id_validation_test, id_validation)

# Create two different datasets
ames_train <- ames[id_train, ]
ames_validation <- ames[id_validation, ]
ames_test <- ames[id_test, ]

skimr::skim(ames_train)

# Some initial plots ----------------------------------------------------------------------------------------------

par(mfrow = c(1, 1))
plot(ames_train$Gr.Liv.Area, ames_train$SalePrice,
  xlab = "Ground living area", ylab = "Sale Price", pch = 16, cex = 0.8
)

plot(ames_train$Total.Bsmt.SF, ames_train$SalePrice,
  xlab = "Total Basement SF", ylab = "Sale Price", pch = 16, cex = 0.8
)

plot(ames_train$Garage.Area, ames_train$SalePrice, xlab = "Garage Area", ylab = "Sale Price", pch = 16, cex = 0.8)

plot(ames_train$Porch.Sq.Feet, ames_train$SalePrice,
  xlab = "Porch Square Feet", ylab = "Sale Price", pch = 16, cex = 0.8
)

plot(ames_train$Tot.Bathrooms, ames_train$SalePrice,
  xlab = "Tot Bathrooms", ylab = "Sale Price", pch = 16, cex = 0.8
)

boxplot(SalePrice ~ Overall.Qual, data = ames_train)
boxplot(SalePrice ~ Bsmt.Qual, data = ames_train)
boxplot(SalePrice ~ Exter.Qual, data = ames_train)
boxplot(SalePrice ~ Kitchen.Qual, data = ames_train)

plot(ames_train$House.Age, ames_train$SalePrice,
  xlab = "House Age (Years)", ylab = "Sale Price", pch = 16, cex = 0.8
)

boxplot(SalePrice ~ MS.Zoning, data = ames_train)
boxplot(SalePrice ~ Roof.Style, data = ames_train)
boxplot(SalePrice ~ Neighborhood, data = ames_train)

# Setting a benchmark ---------------------------------------------------------------------

MAE <- function(y, y_fit) {
  mean(abs(y - y_fit))
}

MSLE <- function(y, y_fit) {
  mean((log(y) - log(y_fit))^2)
}

y_hat_median <- rep(median(ames_train$SalePrice), nrow(ames_validation)) # Prediction

round(MAE(ames_validation$SalePrice, y_hat_median), 4)
round(MSLE(ames_validation$SalePrice, y_hat_median), 4)

# A first simple model --------------------------------------------------------------------------
m_simple <- lm(SalePrice ~ Gr.Liv.Area + Overall.Qual + House.Age + Tot.Bathrooms, data = ames_train)
summary(m_simple)

y_hat_simple <- predict(m_simple, newdata = ames_validation)

# Perform a small correction:
y_hat_simple <- pmax(y_hat_simple, 30000)

round(MAE(ames_validation$SalePrice, y_hat_simple), 4)
round(MSLE(ames_validation$SalePrice, y_hat_simple), 4)

# Taking the log scale -----------------------------------------------------------------------------------
m_simple <- lm(log(SalePrice) ~ Gr.Liv.Area + Overall.Qual + House.Age + Tot.Bathrooms, data = ames_train)
summary(m_simple)

# Re-obtain the original scale
y_hat_simple <- exp(predict(m_simple, newdata = ames_validation))

round(MAE(ames_validation$SalePrice, y_hat_simple), 4)
round(MSLE(ames_validation$SalePrice, y_hat_simple), 4)

# A larger model -----------------------------------------------------------------------------------------

# Here I compute some basic quantities
dim(model.matrix(SalePrice ~ ., data = ames_train)[, -1])

m_full <- lm(log(SalePrice) ~ ., data = ames_train)
summary(m_full)

# 4 collinearities are due to "no basement", 3 collinearities are due to "no garage"

# Moreover, note that at the basement
head(cbind(
  ames_train$Bsmt.Unf.SF + ames_train$BsmtFin.SF.1 + ames_train$BsmtFin.SF.2,
  ames_train$Total.Bsmt.SF
))

# And that at the ground floors
head(cbind(ames_train$X1st.Flr.SF + ames_train$X2nd.Flr.SF + ames_train$Low.Qual.Fin.SF, ames_train$Gr.Liv.Area))

y_hat_full <- exp(predict(m_full, newdata = ames_validation))

round(MAE(ames_validation$SalePrice, y_hat_full), 4)
round(MSLE(ames_validation$SalePrice, y_hat_full), 4)


# Forward and backward regression ----------------------------------------------------

library(leaps)

# Maximum number of covariates included in the list
p_max <- 190
fit_forward <- regsubsets(log(SalePrice) ~ ., data = ames_train, method = "forward", nbest = 1, nvmax = p_max, really.big = TRUE)
sum_forward <- summary(fit_forward)

fit_backward <- regsubsets(log(SalePrice) ~ ., data = ames_train, method = "backward", nbest = 1, nvmax = p_max)
sum_backward <- summary(fit_backward)

library(broom)
library(dplyr)
fit_forward_summary <- fit_forward %>%
  tidy() %>%
  rowwise() %>%
  mutate(p = sum(c_across(MS.SubClassOne_and_Half_Story_Finished_All_Ages:House.Age)), .keep = "unused") %>%
  ungroup()

fit_backward_summary <- fit_backward %>%
  tidy() %>%
  rowwise() %>%
  mutate(p = sum(c_across(MS.SubClassOne_and_Half_Story_Finished_All_Ages:House.Age)), .keep = "unused") %>%
  ungroup()

# R-squared and Mallow's Cp
par(mfrow = c(1, 2))
plot(fit_forward_summary$p, fit_forward_summary$r.squared,
  type = "b", cex = 0.8, pch = 16, ylab = "R-squared", xlab = "p",
  main = "Forward regression"
)
plot(fit_forward_summary$p, fit_forward_summary$mallows_cp,
  type = "b", cex = 0.8, pch = 16, ylab = "Mallows' Cp", xlab = "p",
  main = "Forward regression"
)
abline(v = which.min(fit_forward_summary$mallows_cp), lty = "dashed")

plot(fit_backward_summary$p, fit_backward_summary$r.squared,
  type = "b", cex = 0.8, pch = 16, ylab = "R-squared", xlab = "p",
  main = "Backward regression"
)
plot(fit_backward_summary$p, fit_backward_summary$mallows_cp,
  type = "b", cex = 0.8, pch = 16, ylab = "Mallows' Cp", xlab = "p",
  main = "Backward regression"
)
abline(v = which.min(fit_backward_summary$mallows_cp), lty = "dashed")

# Which do we prefer?
which.min(fit_backward_summary$mallows_cp)
# The list here is quite long
which(sum_backward$which[which.min(fit_backward_summary$mallows_cp), ])

# Let us see what happens at the lowest levels
which(sum_backward$which[1, ]) # Model with one covariate
which(sum_backward$which[2, ]) # Model with two covariates
which(sum_backward$which[3, ]) # Model with three covariates
which(sum_backward$which[4, ]) # Model with four covariates
which(sum_backward$which[5, ]) # Model with five covariates
which(sum_backward$which[6, ]) # Model with six covariates
which(sum_backward$which[7, ]) # Model with seven covariates

# Coding time. Regsubsets does not have a "predict" method, we need to do it ourselves
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object[["call"]][[2]])

  # Compute the design matrix
  X <- model.matrix(form, newdata)
  # Identify the correct beta coefficients
  beta_hat <- coef(object, id = id)
  xvars <- names(beta_hat)

  # Making the predictions
  pred_mat <- X[, xvars] %*% beta_hat

  # Housekeeping
  pred <- as.numeric(pred_mat)
  names(pred) <- rownames(X)
  pred
}

# Let see out it works
head(exp(predict(fit_backward, newdata = ames_train, id = 2)))

# Validation set - selection of p and performance comparisons ----------------------------------------

resid_back <- matrix(0, nrow(ames_validation), p_max + 1)
resid_log_back <- matrix(0, nrow(ames_validation), p_max + 1)

# We first comput the null model
resid_back[, 1] <- ames_validation$SalePrice - exp(predict(lm(log(SalePrice) ~ 1, data = ames_train), newdata = ames_validation))
resid_log_back[, 1] <- log(ames_validation$SalePrice) - predict(lm(log(SalePrice) ~ 1, data = ames_train), newdata = ames_validation)

for (j in 2:(p_max + 1)) {
  y_hat <- exp(predict(fit_backward, newdata = ames_validation, j - 1))
  resid_back[, j] <- ames_validation$SalePrice - y_hat
  resid_log_back[, j] <- log(ames_validation$SalePrice) - log(y_hat)
}

# Displaying the results
data_cv <- data.frame(
  p = 0:p_max,
  MAE = apply(resid_back, 2, function(x) mean(abs(x))),
  MSLE = apply(resid_log_back^2, 2, function(x) mean(x)),
  SE = apply(resid_log_back^2, 2, function(x) sd(x) / sqrt(nrow(ames_train)))
)

se_rule <- data_cv$MSLE[which.min(data_cv$MSLE)] + data_cv$SE[which.min(data_cv$MSLE)]
p_back_optimal <- which(data_cv$MSLE < se_rule)[1]
p_back_optimal

plot(data_cv$p, data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = "p")
abline(v = p_back_optimal, lty = "dashed")
abline(h = MAE(ames_validation$SalePrice, y_hat_simple), lty = "dotted")

plot(data_cv$p, data_cv$MSLE, type = "b", pch = 16, cex = 0.4, ylab = "MSLE", xlab = "p")
abline(v = p_back_optimal, lty = "dashed")
abline(h = MSLE(ames_validation$SalePrice, y_hat_simple), lty = "dotted")

# Optimal model on the validation set
y_hat_back <- exp(predict(fit_backward, newdata = ames_validation, id = p_back_optimal))

MAE(ames_validation$SalePrice, y_hat_back)
MSLE(ames_validation$SalePrice, y_hat_back)

# Principal components regression ----------------------------------------------------------------------

library(pls)

fit_pcr <- pcr(log(SalePrice) ~ ., data = ames_train, center = TRUE, scale = TRUE)
summary(fit_pcr)

resid_pcr <- matrix(0, nrow(ames_validation), p_max + 1)
resid_log_pcr <- matrix(0, nrow(ames_validation), p_max + 1)

# We first comput the null model
resid_pcr[, 1] <- resid_back[, 1]
resid_log_pcr[, 1] <- resid_log_back[, 1]

y_hat_pcr <- exp(predict(fit_pcr, newdata = ames_validation))
for (j in 2:(p_max + 1)) {
  resid_pcr[, j] <- ames_validation$SalePrice - y_hat_pcr[, , j - 1]
  resid_log_pcr[, j] <- log(ames_validation$SalePrice) - log(y_hat_pcr[, , j - 1])
}

data_cv <- data.frame(
  p = 0:p_max,
  MAE = apply(resid_pcr, 2, function(x) mean(abs(x))),
  MSLE = apply(resid_log_pcr^2, 2, function(x) mean(x)),
  SE = apply(resid_log_pcr^2, 2, function(x) sd(x) / sqrt(nrow(ames_train)))
)

se_rule <- data_cv$MSLE[which.min(data_cv$MSLE)] + data_cv$SE[which.min(data_cv$MSLE)]
p_pcr_optimal <- which(data_cv$MSLE < se_rule)[1]
p_pcr_optimal

plot(data_cv$p, data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = "p")
abline(v = p_pcr_optimal, lty = "dashed")
abline(h = MAE(ames_validation$SalePrice, y_hat_simple), lty = "dotted")

plot(data_cv$p, data_cv$MSLE, type = "b", pch = 16, cex = 0.4, ylab = "MSLE", xlab = "p")
abline(v = p_pcr_optimal, lty = "dashed")
abline(h = MSLE(ames_validation$SalePrice, y_hat_simple), lty = "dotted")

# Optimal model on the validation set

MAE(ames_validation$SalePrice, y_hat_pcr[, , p_pcr_optimal])
MSLE(ames_validation$SalePrice, y_hat_pcr[, , p_pcr_optimal])

# A simplified model is basically almost as appropriate
MAE(ames_validation$SalePrice, y_hat_pcr[, , 5])
MSLE(ames_validation$SalePrice, y_hat_pcr[, , 5])

# Ridge regression ----------------------------------------------------------------------

library(glmnet)

# The lambda parameter can be then conveniently selected via cross-validation
X_shrinkage <- model.matrix(SalePrice ~ ., data = ames_train)[, -1]
y_shrinkage <- ames_train$SalePrice

# We need to set alpha = 0 to use the ridge
lambda_ridge_grid <- exp(seq(-5, 6, length = 100))
fit_ridge <- glmnet(X_shrinkage, log(y_shrinkage), alpha = 0, lambda = lambda_ridge_grid)
par(mfrow = c(1, 1))
plot(fit_ridge, xvar = "lambda")

# How to select the "optimal" lambda?
resid_ridge <- matrix(0, nrow(ames_validation), length(lambda_ridge_grid))
resid_log_ridge <- matrix(0, nrow(ames_validation), length(lambda_ridge_grid))

y_hat_ridge <- exp(predict(fit_ridge, newx = model.matrix(SalePrice ~ ., data = ames_validation)[, -1], s = lambda_ridge_grid))
for (j in 1:length(lambda_ridge_grid)) {
  resid_ridge[, j] <- ames_validation$SalePrice - y_hat_ridge[, j]
  resid_log_ridge[, j] <- log(ames_validation$SalePrice) - log(y_hat_ridge[, j])
}

data_cv <- data.frame(
  lambda = lambda_ridge_grid,
  MAE = apply(resid_ridge, 2, function(x) mean(abs(x))),
  MSLE = apply(resid_log_ridge^2, 2, function(x) mean(x)),
  SE = apply(resid_log_ridge^2, 2, function(x) sd(x) / sqrt(nrow(ames_train)))
)

se_rule <- data_cv$MSLE[which.min(data_cv$MSLE)] + data_cv$SE[which.min(data_cv$MSLE)]
lambda_ridge_optimal <- lambda_ridge_grid[tail(which(data_cv$MSLE < se_rule), 1)]
lambda_ridge_optimal

plot(log(data_cv$lambda), data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = expression(log(lambda)))
abline(v = log(lambda_ridge_optimal), lty = "dashed")
abline(h = MAE(ames_validation$SalePrice, y_hat_simple), lty = "dotted")

plot(log(data_cv$lambda), data_cv$MSLE, type = "b", pch = 16, cex = 0.4, ylab = "MSLE", xlab = "p")
abline(v = log(lambda_ridge_optimal), lty = "dashed")
abline(h = MSLE(ames_validation$SalePrice, y_hat_simple), lty = "dotted")

# Optimal model on the validation set
y_hat_ridge <- exp(predict(fit_ridge, newx = model.matrix(SalePrice ~ ., data = ames_validation)[, -1], s = lambda_ridge_optimal))

MAE(ames_validation$SalePrice, y_hat_ridge)
MSLE(ames_validation$SalePrice, y_hat_ridge)

## Cross-validation for ridge regression

ridge_cv <- cv.glmnet(X_shrinkage, log(y_shrinkage), alpha = 0, lambda = lambda_ridge_grid)
plot(ridge_cv)

ridge_cv$lambda.min
ridge_cv$lambda.1se

# MSLE for lambda.min and lambda.1se
ridge_cv$cvm[ridge_cv$index]

## LARS ------------------------------------------
library(lars)

m_lar <- lars(X_shrinkage, log(y_shrinkage), type = "lar")

# Order of inclusion
m_lar
plot(m_lar, breaks = FALSE)

lar_cv <- cv.lars(X_shrinkage, log(y_shrinkage), plot.it = TRUE)

plot(m_lar$df, m_lar$Cp, type = "b")
## Elastic-net -----------------------------------------------------------------------------------------

# We need to set alpha = 0 to use the ridge
lambda_en_grid <- exp(seq(-10, 0, length = 100))
fit_en <- glmnet(X_shrinkage, log(y_shrinkage), alpha = 0.5, lambda = lambda_en_grid)
par(mfrow = c(1, 1))
plot(fit_en, xvar = "lambda")

# How to select the "optimal" lambda?
resid_en <- matrix(0, nrow(ames_validation), length(lambda_en_grid))
resid_log_en <- matrix(0, nrow(ames_validation), length(lambda_en_grid))

y_hat_en <- exp(predict(fit_en, newx = model.matrix(SalePrice ~ ., data = ames_validation)[, -1], s = lambda_en_grid))
for (j in 1:length(lambda_en_grid)) {
  resid_en[, j] <- ames_validation$SalePrice - y_hat_en[, j]
  resid_log_en[, j] <- log(ames_validation$SalePrice) - log(y_hat_en[, j])
}

data_cv <- data.frame(
  lambda = lambda_en_grid,
  MAE = apply(resid_en, 2, function(x) mean(abs(x))),
  MSLE = apply(resid_log_en^2, 2, function(x) mean(x)),
  SE = apply(resid_log_en^2, 2, function(x) sd(x) / sqrt(nrow(ames_train)))
)

se_rule <- data_cv$MSLE[which.min(data_cv$MSLE)] + data_cv$SE[which.min(data_cv$MSLE)]
lambda_en_optimal <- lambda_en_grid[tail(which(data_cv$MSLE < se_rule), 1)]
lambda_en_optimal

plot(log(data_cv$lambda), data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = expression(log(lambda)))
abline(v = log(lambda_en_optimal), lty = "dashed")
abline(h = MAE(ames_validation$SalePrice, y_hat_simple), lty = "dotted")

plot(log(data_cv$lambda), data_cv$MSLE, type = "b", pch = 16, cex = 0.6, ylab = "MSLE", xlab = "p")
abline(v = log(lambda_en_optimal), lty = "dashed")
abline(h = MSLE(ames_validation$SalePrice, y_hat_simple), lty = "dotted")

# Optimal model on the validation set
y_hat_en <- exp(predict(fit_en, newx = model.matrix(SalePrice ~ ., data = ames_validation)[, -1], s = lambda_en_optimal))

MAE(ames_validation$SalePrice, y_hat_en)
MSLE(ames_validation$SalePrice, y_hat_en)

## Cross-validation for elastic-net
en_cv <- cv.glmnet(X_shrinkage, log(y_shrinkage), alpha = 0.5, lambda = lambda_en_grid)
plot(en_cv)

en_cv$lambda.min
en_cv$lambda.1se

# MSLE for lambda.min and lambda.1se
en_cv$cvm[en_cv$index]

## Random forests (spoiler!) ------------------------------------------------------------------------------
library(ranger)
m_rf <- ranger(SalePrice ~ ., data = ames_train)
y_hat_rf <- predict(m_rf, data = ames_validation, type = "response")$predictions

MAE(ames_validation$SalePrice, y_hat_rf)
MSLE(ames_validation$SalePrice, y_hat_rf)

# Final choice -------------------------------------------------------------------

# Null
y_hat_median <- rep(median(ames_train$SalePrice), times = nrow(ames_test))
round(MAE(ames_test$SalePrice, y_hat_median), 4)

# Simple
y_hat_simple <- exp(predict(m_simple, newdata = ames_test))
round(MAE(ames_test$SalePrice, y_hat_simple), 4)

# Full
y_hat_full <- exp(predict(m_full, newdata = ames_test))
round(MAE(ames_test$SalePrice, y_hat_full), 4)

# Backward
y_hat_back <- exp(predict(fit_backward, newdata = ames_test, id = p_back_optimal))
round(MAE(ames_test$SalePrice, y_hat_back), 4)

# Ridge
y_hat_ridge <- exp(predict(fit_ridge, newx = model.matrix(SalePrice ~ ., data = ames_test)[, -1], s = lambda_en_optimal))
round(MAE(ames_test$SalePrice, y_hat_ridge), 4)

# Lasso

# Elastic net
y_hat_en <- exp(predict(fit_en, newx = model.matrix(SalePrice ~ ., data = ames_test)[, -1], s = lambda_en_optimal))
round(MAE(ames_test$SalePrice, y_hat_en), 4)

# Random forest
y_hat_rf <- predict(m_rf, data = ames_test, type = "response")$predictions
MAE(ames_test$SalePrice, y_hat_rf)
