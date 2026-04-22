# ----------------------------------------
# Title: LAB 7 (Ames Housing, regression)
#        GAM and MARS
# Author: Tommaso Rigon
# ----------------------------------------

rm(list = ls())

library(tidyverse)
library(tidymodels)
source("https://tommasorigon.github.io/datamining/code/routines.R", echo = TRUE)

# Data ---------------------------------------------------------------------------------------------
# The same preprocessing pipeline used in LAB 3 and LAB 4 is applied here.
# Near-zero variance predictors are removed; the outcome is log(SalePrice).
ames <- read_csv("https://tommasorigon.github.io/datamining/data/ames.csv")

main_rec <- recipe(SalePrice ~ ., data = ames) %>%
  step_nzv(all_predictors(), unique_cut = 10)

ames <- bake(prep(main_rec), new_data = ames) %>%
  mutate(log_SalePrice = log(SalePrice))

# Three-way split: 50% train / 25% validation / 25% test ----------------------------------------
# The validation set is used for hyperparameter selection throughout this lab.
# The test set is kept completely untouched until the very end.
set.seed(123)
split <- initial_validation_split(ames, prop = c(0.5, 0.25))

ames_tr  <- training(split)
ames_val <- validation(split)
ames_te  <- testing(split)

glimpse(ames_tr)

# Metric helpers -----------------------------------------------------------------------------------
# exp_mae is defined in routines.R and computes the MAE on the original dollar scale
# from log-scale predictions: mean(|exp(truth) - exp(estimate)|).
# We also define a compact wrapper to evaluate any fitted model on the validation set.

evaluate_val <- function(model, newdata = ames_val) {
  y_hat <- predict(model, newdata = newdata)
  augment_df <- newdata %>% mutate(.pred = y_hat)
  exp_mae(augment_df, truth = log_SalePrice, estimate = .pred)
}

# Benchmark: median prediction on the validation set (no predictors used) -------------------------
ames_val %>%
  mutate(.pred = log(median(ames_tr$SalePrice))) %>%
  exp_mae(truth = log_SalePrice, estimate = .pred)


# Linear baseline ----------------------------------------------------------------------------------
# A simple OLS model on a handful of informative predictors, used as a baseline throughout the lab.

m_lm_simple <- lm(log_SalePrice ~ Overall.Qual + Gr.Liv.Area + House.Age + Tot.Bath, data = ames_tr)
summary(m_lm_simple)

y_hat_lm_simple <- predict(m_lm_simple, newdata = ames_val)
ames_val %>%
  mutate(.pred = y_hat_lm_simple) %>%
  exp_mae(truth = log_SalePrice, estimate = .pred)


# Full OLS model (all available predictors after preprocessing) ------------------------------------

m_lm_full <- lm(log_SalePrice ~ . - SalePrice, data = ames_tr)
summary(m_lm_full)

y_hat_lm_full <- predict(m_lm_full, newdata = ames_val)
ames_val %>%
  mutate(.pred = y_hat_lm_full) %>%
  exp_mae(truth = log_SalePrice, estimate = .pred)


# GAM — Generalized Additive Models ---------------------------------------------------------------
# Reference: Wood (2017) "Generalized Additive Models: An Introduction with R", 2nd ed.
#
# A GAM extends the linear model by replacing each linear term beta_j * x_j with a
# smooth function f_j(x_j), estimated from the data:
#
#   E[Y | X] = alpha + f_1(x_1) + f_2(x_2) + ... + f_p(x_p)
#
# Each f_j is typically represented as a penalised regression spline. The amount of
# smoothing is controlled by a penalty parameter lambda_j, selected by GCV or REML.
# Categorical predictors enter the model as ordinary linear (parametric) terms.

library(mgcv)

# GAM — simple model -------------------------------------------------------------------------------
# We start with a GAM that mirrors the simple OLS baseline.
# Continuous predictors are given smooth terms s(); categorical predictors enter linearly.
# By default, mgcv uses thin-plate regression splines with automatic smoothness selection (REML).

m_gam_simple <- gam(
  log_SalePrice ~ s(Overall.Qual) + s(Gr.Liv.Area) + s(House.Age) + s(Tot.Bath),
  data   = ames_tr,
  method = "REML"
)
summary(m_gam_simple)

# The estimated degrees of freedom (edf) for each smooth indicate how non-linear it is:
# edf ~ 1 means the smooth is nearly linear; higher values signal strong non-linearity.

# Visualise the estimated smooth functions ---------------------------------------------------
# Each panel shows one smooth f_j(x_j) with a pointwise 95% confidence band.
# The rug at the bottom shows the distribution of observed x_j values.
par(mfrow = c(2, 2))
plot(m_gam_simple, residuals = TRUE, pch = 16, cex = 0.4,
     shade = TRUE, shade.col = "lightblue",
     pages = 0, scale = 0)

# Validation performance
y_hat_gam_simple <- predict(m_gam_simple, newdata = ames_val)
ames_val %>%
  mutate(.pred = y_hat_gam_simple) %>%
  exp_mae(truth = log_SalePrice, estimate = .pred)


# GAM — extended model with automatic variable selection -------------------------------------------
# We include more continuous predictors as smooth terms plus several categorical predictors.
# Setting select = TRUE adds an extra shrinkage penalty to each smooth: terms that are not
# needed can be shrunk all the way to zero, performing automatic variable selection.

m_gam_full <- gam(
  log_SalePrice ~ s(Overall.Qual) + s(Gr.Liv.Area) + s(House.Age) + s(Tot.Bath) +
    s(Total.Bsmt.SF)  +
    Kitchen.Qual + Neighborhood + MS.Zoning,
  data   = ames_tr,
  method = "REML",
  select = TRUE   # automatic smoothness-based variable selection
)
summary(m_gam_full)

# Visualise the smooth terms of the extended model -------------------------------------------------
par(mfrow = c(2, 4))
plot(m_gam_full, residuals = TRUE, pch = 16, cex = 0.3,
     shade = TRUE, shade.col = "lightblue",
     pages = 0, scale = 0)

# Validation performance
y_hat_gam_full <- predict(m_gam_full, newdata = ames_val)
ames_val %>%
  mutate(.pred = y_hat_gam_full) %>%
  exp_mae(truth = log_SalePrice, estimate = .pred)


# GAM — model comparison via AIC -------------------------------------------------------------------
# AIC penalises model complexity using the effective degrees of freedom.
# Lower AIC is preferred.

AIC(m_gam_simple, m_gam_full)


# MARS — Multivariate Adaptive Regression Splines -------------------------------------------------
# Reference: Friedman (1991) "Multivariate Adaptive Regression Splines", AoS 19(1):1-67.
#
# MARS builds a piecewise linear (or polynomial) model by searching for the best set of
# basis functions of the form:
#
#   h(x - t)_+ = max(0, x - t)   and   h(t - x)_+ = max(0, t - x)
#
# where t is a "knot" (changepoint) chosen from the data. These hinge functions can be
# multiplied together to capture interactions (controlled by the "degree" argument).
#
# The algorithm alternates between a forward pass (adding basis functions greedily) and
# a backward pruning pass (removing terms that do not improve a GCV criterion).
# The hyperparameter nprune controls the maximum number of terms kept after pruning.

library(earth)

# MARS — additive model (degree = 1, no interactions) ---------------------------------------------
# degree = 1 restricts MARS to additive hinge functions (no cross-product terms).
# This is the simplest MARS model and a natural comparison point for GAM.

m_mars1 <- earth(log_SalePrice ~ . - SalePrice, data = ames_tr, degree = 1)
summary(m_mars1)
print(m_mars1)

# Variable importance: the GCV contribution of each predictor across all basis functions
# that include it. Higher values indicate greater importance.
evimp(m_mars1)

par(mfrow = c(1, 1))
plot(evimp(m_mars1), cex.axis = 0.7)


# MARS — model with two-way interactions (degree = 2) ---------------------------------------------
# Allowing degree = 2 lets MARS form products of two hinge functions, capturing non-additive
# relationships between pairs of predictors.

m_mars2 <- earth(log_SalePrice ~ . - SalePrice, data = ames_tr, degree = 2)
summary(m_mars2)

evimp(m_mars2)


# MARS — tuning nprune on the validation set -------------------------------------------------------
# nprune sets the maximum number of terms (including the intercept) kept after pruning.
# A larger nprune allows more complex models; too large a value risks overfitting.
# We evaluate a grid of nprune values for both degree = 1 and degree = 2.

nprune_grid <- seq(5, 60, by = 5)

# Helper: fit MARS with given degree and nprune, return validation exp_mae
mars_val_mae <- function(deg, np) {
  m <- earth(log_SalePrice ~ . - SalePrice, data = ames_tr, degree = deg, nprune = np)
  y_hat <- predict(m, newdata = ames_val)
  ames_val %>%
    mutate(.pred = c(y_hat)) %>%
    exp_mae(truth = log_SalePrice, estimate = .pred) %>%
    pull(.estimate)
}

# Evaluate over the grid (this may take a minute)
val_results <- expand.grid(degree = c(1, 2), nprune = nprune_grid) %>%
  rowwise() %>%
  mutate(exp_mae = mars_val_mae(degree, nprune)) %>%
  ungroup()

# Plot validation MAE as a function of nprune for each degree
par(mfrow = c(1, 1))
plot(val_results$nprune[val_results$degree == 1],
     val_results$exp_mae[val_results$degree == 1],
     type = "b", pch = 16, cex = 0.7,
     xlab = "nprune", ylab = "Exp-MAE (validation)",
     main = "MARS: validation performance vs nprune",
     ylim = range(val_results$exp_mae))
lines(val_results$nprune[val_results$degree == 2],
      val_results$exp_mae[val_results$degree == 2],
      type = "b", pch = 17, cex = 0.7, col = "darkorange", lty = 2)
legend("topright", legend = c("degree = 1", "degree = 2"),
       col = c("black", "darkorange"), lty = c(1, 2), pch = c(16, 17))

# Optimal hyperparameters
best_mars <- val_results %>%
  slice_min(exp_mae, n = 1)
best_mars

best_degree <- best_mars$degree
best_nprune <- best_mars$nprune

cat("Best degree:", best_degree, " | Best nprune:", best_nprune, "\n")

# Fit the selected MARS model on the full training set
m_mars_best <- earth(
  log_SalePrice ~ . - SalePrice,
  data   = ames_tr,
  degree = best_degree,
  nprune = best_nprune
)
summary(m_mars_best)

# Validation performance of the best MARS model
y_hat_mars_best <- predict(m_mars_best, newdata = ames_val)
ames_val %>%
  mutate(.pred = c(y_hat_mars_best)) %>%
  exp_mae(truth = log_SalePrice, estimate = .pred)


# Final comparison on the test set ----------------------------------------------------------------
# All models are now evaluated on the held-out test set to obtain an unbiased estimate of
# generalisation performance. The test set has not been used at any earlier stage.

# Collect test-set predictions for every model
y_hat_te_median     <- rep(log(median(ames_tr$SalePrice)), nrow(ames_te))
y_hat_te_lm_simple  <- predict(m_lm_simple,  newdata = ames_te)
y_hat_te_lm_full    <- predict(m_lm_full,    newdata = ames_te)
y_hat_te_gam_simple <- predict(m_gam_simple, newdata = ames_te)
y_hat_te_gam_full   <- predict(m_gam_full,   newdata = ames_te)
y_hat_te_mars_best  <- c(predict(m_mars_best, newdata = ames_te))

# Build a named list for tidy evaluation
predictions_te <- list(
  Median       = y_hat_te_median,
  `OLS simple` = y_hat_te_lm_simple,
  `OLS full`   = y_hat_te_lm_full,
  `GAM simple` = y_hat_te_gam_simple,
  `GAM full`   = y_hat_te_gam_full,
  `MARS`       = y_hat_te_mars_best
)

results_te <- imap_dfr(predictions_te, function(y_hat, name) {
  ames_te %>%
    mutate(.pred = y_hat) %>%
    exp_mae(truth = log_SalePrice, estimate = .pred) %>%
    transmute(model = name, exp_mae = .estimate)
})

results_te %>% arrange(exp_mae)
