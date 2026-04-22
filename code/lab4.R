# ----------------------------------------
# Title: LAB 4 (Ames Housing, regression)
#        Rewritten with tidymodels
# ----------------------------------------

rm(list = ls())
library(tidyverse)
library(tidymodels)
source("https://tommasorigon.github.io/datamining/code/routines.R", echo = TRUE)

# Data ---------------------------------------------------------------------------------------------
# log_SalePrice is the model outcome; SalePrice is kept for final evaluation
ames <- read_csv("https://tommasorigon.github.io/datamining/data/ames.csv")

main_rec <- recipe(SalePrice ~ ., data = ames) %>%
  step_nzv(all_predictors(), unique_cut = 10)

ames <- bake(prep(main_rec), new_data = ames) %>%
  mutate(log_SalePrice = log(SalePrice))

# Three-way split: 50 % train / 25 % validation / 25 % test ----------------------------------------
set.seed(123)
split <- initial_validation_split(ames, prop = c(0.5, 0.25))

ames_tr <- training(split)
ames_val <- validation(split)
ames_te <- testing(split) # kept untouched until the very end

# Recipes -------------------------------------------------------------------------------------------

# Model
m_linear <- linear_reg() %>%
  set_engine("lm")

# The outcome is log_SalePrice; SalePrice is dropped from the predictor set.
base_recipe <- recipe(log_SalePrice ~ ., data = ames_tr) %>%
  step_rm(SalePrice) %>%
  step_dummy(all_factor_predictors())

# Shrinkage methods additionally require centring and scaling
shrinkage_recipe <- base_recipe %>%
  step_normalize(all_predictors())

# Metrics (original dollar scale)
my_metrics <- metric_set(exp_mae)

# Benchmark: median prediction
ames_val %>%
  mutate(.pred = log(median(ames_tr$SalePrice))) %>%
  my_metrics(truth = log_SalePrice, estimate = .pred)

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

# PCR -----------------------------------------------------------------------------------------

val_resample <- validation_set(split)

wf_pcr <- workflow() %>%
  add_recipe(shrinkage_recipe %>% step_pca(all_predictors(), num_comp = tune())) %>%
  add_model(m_linear)

pcr_val <- tune_grid(
  wf_pcr,
  resamples = val_resample,
  grid = tibble(num_comp = 1:113),
  metrics = my_metrics,
  control = control_grid(save_workflow = TRUE, verbose = TRUE)
)

collect_metrics(pcr_val)

autoplot(pcr_val, metric = "exp_mae") + theme_bw()
show_best(pcr_val, metric = "exp_mae")

# Select final best model (including validation set)
best_pcr_val <- select_best(pcr_val, metric = "exp_mae")
best_pcr_val <- finalize_workflow(wf_pcr, best_pcr_val) %>% fit(data = ames_tr)

tidy(best_pcr_val)

# Ridge -----------------------------------------------------------------------------------------
wf_ridge <- workflow() %>%
  add_recipe(shrinkage_recipe) %>%
  add_model(linear_reg(penalty = tune(), mixture = 0) %>% set_engine("glmnet"))

ridge_val <- tune_grid(
  wf_ridge,
  resamples = val_resample,
  grid      = tibble(penalty = exp(seq(-6, 6, length.out = 100))),
  metrics   = my_metrics
)

collect_metrics(ridge_val)

autoplot(ridge_val, metric = "exp_mae") + theme_bw()
show_best(ridge_val, metric = "exp_mae")

# Select final best model (including validation set)
best_ridge_val <- select_best(ridge_val, metric = "exp_mae")
best_ridge_val <- finalize_workflow(wf_ridge, best_ridge_val) %>% fit(data = ames_tr)

print(tidy(best_ridge_val), n = 15)

# Lasso -----------------------------------------------------------------------------------------
wf_lasso <- workflow() %>%
  add_recipe(shrinkage_recipe) %>%
  add_model(linear_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet"))

lasso_val <- tune_grid(
  wf_lasso,
  resamples = val_resample,
  grid      = tibble(penalty = exp(seq(-10, 0, length.out = 100))),
  metrics   = my_metrics
)

collect_metrics(lasso_val)

autoplot(lasso_val, metric = "exp_mae") + theme_bw()
show_best(lasso_val, metric = "exp_mae")

# Select final best model (including validation set)
best_lasso_val <- select_best(lasso_val, metric = "exp_mae")
best_lasso_val <- finalize_workflow(wf_lasso, best_lasso_val) %>% fit(data = ames_tr)

print(tidy(best_lasso_val), n = 15)

# Elastic Net -----------------------------------------------------------------------------------------
wf_en <- workflow() %>%
  add_recipe(shrinkage_recipe) %>%
  add_model(linear_reg(penalty = tune(), mixture = 0.5) %>% set_engine("glmnet"))

en_val <- tune_grid(
  wf_en,
  resamples = val_resample,
  grid      = tibble(penalty = exp(seq(-10, 0, length.out = 100))),
  metrics   = my_metrics
)

collect_metrics(en_val)

autoplot(en_val, metric = "exp_mae") + theme_bw()
show_best(en_val, metric = "exp_mae")

# Select final best model (including validation set)
best_en_val <- select_best(en_val, metric = "exp_mae")
best_en_val <- finalize_workflow(wf_en, best_en_val) %>% fit(data = ames_tr)

print(tidy(best_en_val), n = 15)

# Random Forest -----------------------------------------------------------------------------------------

wf_rf <- workflow() %>%
  add_recipe(shrinkage_recipe) %>%
  add_model(rand_forest(trees = tune(), mtry = tune(), min_n = tune(), mode = "regression") %>% set_engine("ranger"))

rf_val <- tune_grid(
  wf_rf,
  resamples = val_resample,
  grid = tibble(expand.grid(trees = c(1000, 2000, 5000), mtry = c(5, 10, 30, 50), min_n = c(10, 20, 50))),
  metrics = my_metrics,
  control = control_grid(verbose = TRUE)
)

collect_metrics(rf_val)

autoplot(rf_val, metric = "exp_mae") + theme_bw()
show_best(rf_val, metric = "exp_mae")

# Select final best model (including validation set)
best_rf_val <- select_best(rf_val, metric = "exp_mae")
best_rf_val <- finalize_workflow(wf_rf, best_rf_val) %>% fit(data = ames_tr)


# Final comparison on the test set -----------------------------------------------------------------------------------------

fitted_models <- list(
  Simple        = m_simple,
  Full          = m_full,
  PCR           = best_pcr_val,
  Ridge         = best_ridge_val,
  Lasso         = best_lasso_val,
  `Elastic Net` = best_en_val,
  `Rand Forest` = best_rf_val
)

results <- imap_dfr(fitted_models, function(model, name) {
  augment(model, new_data = ames_te) %>%
    exp_mae(truth = log_SalePrice, estimate = .pred) %>%
    transmute(model = name, mae = .estimate)
})

results %>% arrange(mae)
