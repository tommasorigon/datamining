# --------------------------------------------------------------------------------
# Title: LAB 2 (Introduction to tidymodels, data splitting and cross-validation)
# Author: Tommaso Rigon
# --------------------------------------------------------------------------------

rm(list = ls())

library(tidymodels)

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

# The tidymodel workflow ------------------------------------------------------------------------------

# Model
m_linear <- linear_reg() %>%
  set_engine("lm")

# Fitting a linear model
m_poly_3 <- fit(m_linear, Score1 ~ poly(Longitude, 3), data = trawl_tr)
tidy(m_poly_3)

# Alternative syntax based on recipes
rec_poly_3 <- recipe(Score1 ~ Longitude, data = trawl_tr) %>%
  step_poly(Longitude, degree = 3)

rec_prep <- prep(rec_poly_3)
bake(rec_prep, new_data = NULL) # Training data
bake(rec_prep, new_data = head(trawl_val)) # Validation data

wf_poly_3 <- workflow() %>%
  add_model(m_linear) %>%
  add_recipe(rec_poly_3)

m_poly_3 <- wf_poly_3 %>%
  fit(data = trawl_tr)

tidy(m_poly_3)

augment(m_poly_3, new_data = trawl_val) %>% rmse(truth = Score1, estimate = .pred)
augment(m_poly_3, new_data = trawl_val) %>% mae(truth = Score1, estimate = .pred)

# Tunable recipe ----------------------------------------------------------------------------------------

rec_poly <- recipe(Score1 ~ Longitude, data = trawl_tr) %>%
  step_poly(Longitude, degree = tune())

wf_poly <- workflow() %>%
  add_model(m_linear) %>%
  add_recipe(rec_poly)

# Method for evaluating the loss: validation set
val_resample <- validation_set(split)

# Metrics
metric_list <- metric_set(rmse, mae)

# Grid of polynomial degrees
grid_poly <- tibble(degree = 1:15)

# Tuning on validation set (train + validation only)
poly_val <- tune_grid(
  wf_poly,
  resamples = val_resample,
  metrics = metric_list,
  grid = grid_poly,
  control = control_grid(save_workflow = TRUE)
)

collect_metrics(poly_val)

autoplot(poly_val, metric = "rmse") + theme_bw()
autoplot(poly_val, metric = "mae") + theme_bw()

show_best(poly_val, metric = "rmse")
show_best(poly_val, metric = "mae")

# Select final best model (including validation set)
best_param_val <- select_best(poly_val, metric = "rmse")
best_lm_val <- finalize_workflow(wf_poly, best_param_val) %>% fit(data = bind_rows(trawl_tr, trawl_val))
tidy(best_lm_val)

# Cross-validation ------------------------------------------------------------------------------------

# Extended training, because now we are using cross-validation
trawl_tr2 <- bind_rows(trawl_tr, trawl_val)

cv_samples <- vfold_cv(trawl_tr2, v = 10)

# Create an object that will be used in the workflow
poly_cv <- tune_grid(
  object = m_linear,
  preprocessor = rec_poly,
  resamples = cv_samples, # uses train + validation (not test)
  metrics = metric_list,
  grid = grid_poly,
  control = control_grid(save_workflow = TRUE, verbose = TRUE)
)

collect_metrics(poly_cv)

autoplot(poly_cv, metric = "rmse") + theme_bw()
autoplot(poly_cv, metric = "mae") + theme_bw()

show_best(poly_cv, metric = "rmse")
show_best(poly_cv, metric = "mae")

# Select final best model (including validation set)
best_param_cv <- select_best(poly_cv, metric = "rmse")
best_lm_cv <- finalize_workflow(wf_poly, best_param_cv) %>% fit(data = trawl_tr2)
tidy(best_lm_cv)

# Final errors --------------------------------------------------------------

augment(best_lm_val, new_data = trawl_te) %>% rmse(truth = Score1, estimate = .pred)
augment(best_lm_cv, new_data = trawl_te) %>% rmse(truth = Score1, estimate = .pred)

# Graphical representation on the test set
seq_data <- tibble(Longitude = seq(142.5, 144, length.out = 200))

plot(trawl_te$Longitude, trawl_te$Score1, pch = 16)
lines(seq_data$Longitude, predict(best_lm_val, seq_data)$.pred, col = "red")
lines(seq_data$Longitude, predict(best_lm_cv, seq_data)$.pred)
