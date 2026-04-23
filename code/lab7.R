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
ames <- read_csv("https://tommasorigon.github.io/datamining/data/ames.csv")

main_rec <- recipe(SalePrice ~ ., data = ames) %>%
  step_nzv(all_predictors(), unique_cut = 10)

ames <- bake(prep(main_rec), new_data = ames) %>%
  mutate(log_SalePrice = log(SalePrice))

# Three-way split: 50% train / 25% validation / 25% test ----------------------------------------
set.seed(123)
split <- initial_validation_split(ames, prop = c(0.5, 0.25))

ames_tr  <- training(split)
ames_val <- validation(split)
ames_te  <- testing(split)

glimpse(ames_tr)

m_linear <- linear_reg() %>%
  set_engine("lm")

# The outcome is log_SalePrice; SalePrice is dropped from the predictor set.
base_recipe <- recipe(log_SalePrice ~ ., data = ames_tr) %>%
  step_rm(SalePrice) %>%
  step_dummy(all_factor_predictors())

# Shrinkage methods additionally require centring and scaling.
shrinkage_recipe <- base_recipe %>%
  step_normalize(all_predictors())

# Metric: exponentiated MAE on the original dollar scale (defined in routines.R)
my_metrics <- metric_set(exp_mae)


# Simple OLS -----------------------------------------------------------------------------------------

m_simple <- workflow() %>%
  add_recipe(recipe(log_SalePrice ~ Overall.Qual + Gr.Liv.Area + House.Age + Tot.Bath, data = ames_tr)) %>%
  add_model(m_linear) %>%
  fit(ames_tr)

tidy(m_simple)
augment(m_simple, new_data = ames_val) %>% exp_mae(truth = log_SalePrice, estimate = .pred)


# Full OLS -----------------------------------------------------------------------------------------

m_full <- workflow() %>%
  add_recipe(base_recipe) %>%
  add_model(m_linear) %>%
  fit(ames_tr)

print(tidy(m_full), n = 15)
augment(m_full, new_data = ames_val) %>% exp_mae(truth = log_SalePrice, estimate = .pred)



# GAM — Generalized Additive Models ---------------------------------------------------------------

library(mgcv)

# GAM — simple model -------------------------------------------------------------------------------

m_gam_simple <- gam(
  log_SalePrice ~ Overall.Qual + s(Gr.Liv.Area) + s(House.Age) + Tot.Bath,
  data   = ames_tr,
  method = "GCV.Cp"
)
summary(m_gam_simple)

m_gam <- gen_additive_mod(select_features = FALSE) %>% set_engine("mgcv") %>% set_mode("regression")

m_gam_simple <- workflow() %>%
  add_model(m_gam, formula = log_SalePrice ~ s(Overall.Qual, k = 3) + s(Gr.Liv.Area) + s(House.Age) + s(Tot.Bath)) %>%
  add_formula(log_SalePrice ~ Overall.Qual + Gr.Liv.Area + House.Age + Tot.Bath) %>%
  fit(data = ames_tr)

tidy(m_gam_simple)

augment(m_gam_simple, new_data = ames_val) %>% exp_mae(truth = log_SalePrice, estimate = .pred)

# GAM — extended model with automatic variable selection -------------------------------------------

smooth_vars <- c("Gr.Liv.Area", "House.Age", "Lot.Area")

# All predictors (adjust as needed)
all_vars <- c("Overall.Qual", "Gr.Liv.Area", "House.Age", "Tot.Bath", "Lot.Area", 
              "Neighborhood", "MS.Zoning", "Bldg.Type")

# Build the mgcv formula with s() for smooth terms
mgcv_terms <- ifelse(all_vars %in% smooth_vars, paste0("s(", all_vars, ")"), all_vars)
mgcv_formula <- as.formula(paste("log_SalePrice ~", paste(mgcv_terms, collapse = " + ")))

# Build the plain formula for add_formula()
plain_formula <- as.formula(paste("log_SalePrice ~", paste(all_vars, collapse = " + ")))

m_gam_full <- workflow() %>%
  add_model(
    gen_additive_mod() %>%
      set_engine("mgcv") %>%
      set_mode("regression"),
    formula = mgcv_formula
  ) %>%
  add_formula(plain_formula) %>%
  fit(data = ames_tr)

tidy(m_gam_full)
augment(m_gam_full, new_data = ames_val) %>% exp_mae(truth = log_SalePrice, estimate = .pred)


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
