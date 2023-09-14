# -----------------------------------
# LAB 3 (Ames Housing)
# Course: Data Mining
# Author: Tommaso Rigon
# --------------------------------


# Predictive analysis ----------------------------------------------------------------------------

rm(list = ls())
ames_train <- read.table("../data/ames_train.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
ames_validation <- read.table("../data/ames_validation.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
ames_test <- read.table("../data/ames_test.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

skimr::skim(ames_train)

par(mfrow = c(1, 1))
plot(ames_train$Gr.Liv.Area, ames_train$SalePrice,
  xlab = "Ground living area", ylab = "Sale Price", pch = 16, cex = 0.8
)

plot(ames_train$Total.Bsmt.SF, ames_train$SalePrice,
  xlab = "Total Basement SF", ylab = "Sale Price", pch = 16, cex = 0.8
)

plot(ames_train$Garage.Area, ames_train$SalePrice,
  xlab = "Garage Area", ylab = "Sale Price", pch = 16, cex = 0.8
)

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

RMSLE <- function(y, y_fit) {
  sqrt(mean((log(y) - log(y_fit))^2))
}

y_hat_median <- rep(median(ames_train$SalePrice), nrow(ames_validation)) # Prediction

round(MAE(ames_validation$SalePrice, y_hat_median), 4)
round(RMSLE(ames_validation$SalePrice, y_hat_median), 4)

# A first simple model --------------------------------------------------------------------------
m_simple <- lm(SalePrice ~ Gr.Liv.Area + Overall.Qual + House.Age + Tot.Bathrooms, data = ames_train)
summary(m_simple)

y_hat_simple <- predict(m_simple, newdata = ames_validation)

# Perform a small correction:
y_hat_simple <- pmax(y_hat_simple, 30000)

round(MAE(ames_validation$SalePrice, y_hat_simple), 4)
round(RMSLE(ames_validation$SalePrice, y_hat_simple), 4)

# Taking the log scale -----------------------------------------------------------------------------------
m_simple <- lm(log(SalePrice) ~ Gr.Liv.Area + Overall.Qual + House.Age + Tot.Bathrooms, data = ames_train)
summary(m_simple)

# Re-obtain the original scale
y_hat_simple <- exp(predict(m_simple, newdata = ames_test))

round(MAE(ames_test$SalePrice, y_hat_simple), 4)
round(RMSLE(ames_test$SalePrice, y_hat_simple), 4)

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

# Forward and backward regression ----------------------------------------------------

library(leaps)
fit_forward <- regsubsets(log(SalePrice) ~ .,
  data = ames_train, method = "forward",
  nbest = 1, nvmax = 200, really.big = TRUE
)
sum_forward <- summary(fit_forward)

fit_backward <- regsubsets(log(SalePrice) ~ .,
  data = ames_train, method = "backward",
  nbest = 1, nvmax = 200
)
sum_backward <- summary(fit_backward)

library(broom)
fit_forward_summary <- fit_forward %>%
  tidy() %>%
  rowwise() %>%
  mutate(p = sum(c_across(MS.SubClass:House.Age)), .keep = "unused") %>%
  ungroup()

fit_backward_summary <- fit_backward %>%
  tidy() %>%
  rowwise() %>%
  mutate(p = sum(c_across(MS.SubClass:House.Age)), .keep = "unused") %>%
  ungroup()

par(mfrow = c(1, 2))
plot(fit_forward_summary$p, fit_forward_summary$r.squared,
  type = "b", cex = 0.8, pch = 16, ylab = "R-squared", xlab = "p",
  main = "Forward regression"
)
plot(fit_forward_summary$p, fit_forward_summary$mallows_cp,
  type = "b", cex = 0.8, pch = 16, ylab = "Mallows' Cp", xlab = "p",
  main = "Forward regression"
)

plot(fit_backward_summary$p, fit_backward_summary$r.squared,
  type = "b", cex = 0.8, pch = 16, ylab = "R-squared", xlab = "p",
  main = "Backward regression"
)
plot(fit_backward_summary$p, fit_backward_summary$mallows_cp,
  type = "b", cex = 0.8, pch = 16, ylab = "Mallows' Cp", xlab = "p",
  main = "Backward regression"
)
abline(v = which.min(fit_backward_summary$mallows_cp), lty = "dashed")

which.min(fit_backward_summary$mallows_cp)
which(sum_backward$which[which.min(fit_backward_summary$mallows_cp), ])

which(sum_backward$which[1, ]) # Model with one covariate
which(sum_backward$which[2, ]) # Model with two covariates
which(sum_backward$which[3, ]) # Model with three covariates
which(sum_backward$which[4, ]) # Model with four covariates
which(sum_backward$which[5, ]) # Model with five covariates
which(sum_backward$which[6, ]) # Model with six covariates
which(sum_backward$which[7, ]) # Model with seven covariates

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

head(exp(predict(fit_backward, newdata = ames_train, id = 2)))

library(rsample)

p_max <- 200
set.seed(123)

V <- 10 # Number of CV fold
cv_fold <- vfold_cv(ames_train, v = V)

resid_back <- matrix(0, nrow(ames_train), p_max + 1)
resid_log_back <- matrix(0, nrow(ames_train), p_max + 1)

resid_simple <- numeric(nrow(ames_train))
resid_log_simple <- numeric(nrow(ames_train))

for (k in 1:V) {
  # Estimation of the null model
  fit_cv_null <- lm(log(SalePrice) ~ 1, data = data.frame(analysis(cv_fold$splits[[k]])))

  # Simple model
  fit_cv_simple <- lm(log(SalePrice) ~ Gr.Liv.Area + Overall.Qual + House.Age + Tot.Bathrooms,
    data = analysis(cv_fold$splits[[k]])
  )

  # Forward and backward regression
  fit_cv_back <- regsubsets(log(SalePrice) ~ .,
    data = analysis(cv_fold$splits[[k]]),
    method = "backward", nbest = 1, nvmax = p_max
  )

  # Hold-out quantities
  y_k <- assessment(cv_fold$splits[[k]])$SalePrice

  # Residuals for the null model
  y_k_null <- exp(predict(fit_cv_null, assessment(cv_fold$splits[[k]])))
  resid_back[complement(cv_fold$splits[[k]]), 1] <- y_k - y_k_null
  resid_log_back[complement(cv_fold$splits[[k]]), 1] <- log(y_k) - log(y_k_null)

  # Residuals for the simple model
  y_k_simple <- exp(predict(fit_cv_simple, assessment(cv_fold$splits[[k]])))
  resid_simple[complement(cv_fold$splits[[k]])] <- y_k - y_k_simple
  resid_log_simple[complement(cv_fold$splits[[k]])] <- log(y_k) - log(y_k_simple)

  # Residuals of the best models for different values of p
  for (j in 2:(p_max + 1)) {
    y_k_back <- exp(predict(fit_cv_back, assessment(cv_fold$splits[[k]]), j - 1))
    resid_back[complement(cv_fold$splits[[k]]), j] <- y_k - y_k_back
    resid_log_back[complement(cv_fold$splits[[k]]), j] <- log(y_k) - log(y_k_back)
  }
}

# Housekeeping
rm(y_k, y_k_back, y_k_null, y_k_simple, fit_cv_null, fit_cv_simple, fit_cv_back, j, k)

data_cv <- data.frame(
  p = 0:p_max,
  MAE = apply(resid_back, 2, function(x) mean(abs(x))),
  MSLE = apply(resid_log_back^2, 2, function(x) mean(x)),
  SE = apply(resid_log_back^2, 2, function(x) sd(x) / sqrt(nrow(ames_train)))
)

se_rule <- data_cv$MSLE[which.min(data_cv$MSLE)] + data_cv$SE[which.min(data_cv$MSLE)]
p_optimal <- which(data_cv$MSLE < se_rule)[1]
p_optimal

plot(data_cv$p, data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE", xlab = "p")
abline(v = p_optimal, lty = "dashed")
abline(h = mean(abs(resid_simple)), lty = "dotted")

plot(data_cv$p, sqrt(data_cv$MSLE), type = "b", pch = 16, cex = 0.4, ylab = "RMSLE", xlab = "p")
abline(v = p_optimal, lty = "dashed")
abline(h = sqrt(mean(resid_log_simple^2)), lty = "dotted")

y_hat_back <- exp(predict(fit_backward, newdata = ames_test, id = p_optimal))

library(pls)
p_max <- 200
V <- 10
resid_pcr <- matrix(0, nrow(ames_train), p_max)
resid_log_pcr <- matrix(0, nrow(ames_train), p_max)

for (k in 1:V) {
  # Estimation of the null model
  fit_null <- lm(log(SalePrice) ~ 1, data = data.frame(analysis(cv_fold$splits[[k]])))

  # Hold-out dataset
  y_k <- assessment(cv_fold$splits[[k]])$SalePrice
  # MSE of the null model
  resid_pcr[complement(cv_fold$splits[[k]]), 1] <- y_k - exp(predict(fit_null, assessment(cv_fold$splits[[k]])))
  resid_log_pcr[complement(cv_fold$splits[[k]]), 1] <- log(y_k) - predict(fit_null, assessment(cv_fold$splits[[k]]))

  # Fitting PCR (all the components at once)
  fit_cv_pcr <- pcr(log(SalePrice) ~ ., data = analysis(cv_fold$splits[[k]]), center = TRUE, scale = FALSE)
  # Predictions
  y_k_pcr <- exp(predict(fit_cv_pcr, newdata = assessment(cv_fold$splits[[k]])))
  for (j in 2:p_max) {
    # MSE of the best models for different values of p
    resid_pcr[complement(cv_fold$splits[[k]]), j] <- y_k - y_k_pcr[, , j - 1]
    resid_log_pcr[complement(cv_fold$splits[[k]]), j] <- log(y_k) - log(y_k_pcr[, , j - 1])
  }
}

data_cv <- data.frame(
  p = 1:p_max,
  MAE = apply(resid_pcr, 2, function(x) mean(abs(x))),
  MSLE = apply(resid_log_pcr^2, 2, function(x) mean(x)),
  SE = apply(resid_log_pcr^2, 2, function(x) sd(x) / sqrt(nrow(ames_train)))
)

se_rule <- data_cv$MSLE[which.min(data_cv$MSLE)] + data_cv$SE[which.min(data_cv$MSLE)]
p_optimal <- which(data_cv$MSLE < se_rule)[1]
p_optimal

plot(data_cv$p, data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE", xlab = "p")
abline(v = p_optimal, lty = "dashed")
abline(h = mean(abs(resid_simple)), lty = "dotted")

plot(data_cv$p, sqrt(data_cv$MSLE), type = "b", pch = 16, cex = 0.6, ylab = "RMSLE", xlab = "p")
abline(v = p_optimal, lty = "dashed")
abline(h = sqrt(mean(resid_log_simple^2)), lty = "dotted")

# Null
round(MAE(ames_test$SalePrice, y_hat_median), 4)
round(RMSLE(ames_test$SalePrice, y_hat_median), 4)

# Simple
round(MAE(ames_test$SalePrice, y_hat_simple), 4)
round(RMSLE(ames_test$SalePrice, y_hat_simple), 4)

# Full
round(MAE(ames_test$SalePrice, y_hat_full), 4)
round(RMSLE(ames_test$SalePrice, y_hat_full), 4)

# Backward
round(MAE(ames_test$SalePrice, y_hat_back), 4)
round(RMSLE(ames_test$SalePrice, y_hat_back), 4)
