# ---------------------------------------------
# Title: LAB 2 (Optimism and cross-validation)
# Author: Tommaso Rigon
# ---------------------------------------------

rm(list = ls())

library(tidymodels)
library(tidyverse)

data("trawl", package = "sm")
glimpse(trawl)

# Training, validation and test -----------------------------------------------------------------------

set.seed(123)

# Simple training and test
split <- initial_validation_split(trawl, prop = c(0.6, 0.2))

trawl_tr <- training(split)
trawl_val <- validation(split)
trawl_te <- testing(split) # kept untouched until the very end

head(trawl_tr)
head(trawl_val)

plot(trawl_tr$Longitude, trawl_tr$Score1, pch = 16)
plot(trawl_val$Longitude, trawl_val$Score1, pch = 16)

# Fitting a polynomial regression "the old way" vs better approach
m_degree3_raw <- lm(Score1 ~ Longitude + I(Longitude^2) + I(Longitude^3),
  data = trawl_tr
)

m_degree3_poly <- lm(Score1 ~ poly(Longitude, 3),
  data = trawl_tr
)

# Predictions on the validation set and RMSE
fit_degree3 <- predict(m_degree3_poly, newdata = trawl_val)
rmse_vec(trawl_val$Score1, fit_degree3)

# The tidymodel workflow ------------------------------------------------------------------------------

# Recipe (defined on training and validation data)
rec_poly_3 <- recipe(Score1 ~ Longitude, data = trawl_tr) %>%
  step_poly(Longitude, degree = 3)

bake(prep(rec_poly_3), new_data = NULL) # Training data
bake(prep(rec_poly_3), new_data = head(trawl_val)) # Validation data

# Tunable recipe
rec_poly <- recipe(Score1 ~ Longitude, data = trawl_tr) %>%
  step_poly(Longitude, degree = tune())

# Method for evaluating the loss: validation set
val_resample <- validation_set(split)

# Metrics
metric_list <- metric_set(rmse, mae)

# Model
m_linear <- linear_reg() %>%
  set_engine("lm")

# Grid of polynomial degrees
grid_poly <- tibble(degree = 1:15)

# Tuning on validation set (train + validation only)
poly_val <- tune_grid(
  m_linear,
  rec_poly,
  resamples = val_resample,
  metrics = metric_list,
  grid = grid_poly,
  control = control_grid(save_workflow = TRUE)
)

collect_metrics(poly_val)

autoplot(poly_val, metric = "rmse") + theme_bw()
autoplot(poly_val, metric = "mae") + theme_bw()

select_best(poly_val, metric = "rmse")
select_best(poly_val, metric = "mae")

# Select final best model (including validation set)
best_lm_val <- fit_best(poly_val, metric = "rmse", add_validation_set = TRUE)

# Cross-validation ------------------------------------------------------------------------------------

# Extended training, because now we are using cross-validation
trawl_tr2 <- bind_rows(trawl_tr, trawl_val)

cv_samples <- vfold_cv(trawl_tr2, v = 10)

# Create an object that will be used in the workflow
poly_cv <- tune_grid(m_linear,
  rec_poly,
  resamples = cv_samples, # This guarantees we use train and validation (not test!)
  metrics = metric_list,
  grid = grid_poly,
  control = control_grid(save_workflow = TRUE, verbose = TRUE)
)

collect_metrics(poly_cv)

autoplot(poly_cv, metric = "rmse") + theme_bw()
autoplot(poly_cv, metric = "mae") + theme_bw()

select_best(poly_cv, metric = "rmse")
select_best(poly_cv, metric = "mae")

best_lm_cv <- fit_best(poly_cv, metric = "rmse")

# Final errors
rmse_vec(truth = trawl_te$Score1, 
         estimate = predict(best_lm_val, new_data = trawl_te)$.pred)
rmse_vec(truth = trawl_te$Score1, 
         estimate = predict(best_lm_cv, new_data = trawl_te)$.pred)

# Graphical representation on the validation
seq_data <- tibble(Longitude = seq(142.5, 144, length.out = 200))
plot(trawl_te$Longitude, trawl_te$Score1, pch = 16)
lines(seq_data$Longitude, predict(best_lm_val, seq_data)$.pred, col = "red")
lines(seq_data$Longitude, predict(best_lm_cv, seq_data)$.pred)

