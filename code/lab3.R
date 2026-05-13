# ----------------------------------------
# Title: LAB 3 (Ames Housing, regression)
#        Rewritten with tidymodels
# Author: Tommaso Rigon
# ----------------------------------------

rm(list = ls())
library(tidyverse)
library(tidymodels)
source("https://tommasorigon.github.io/datamining/code/routines.R", echo = TRUE)

# Data ---------------------------------------------------------------------------------------------
# log_SalePrice is the model outcome; SalePrice is kept for reference only.
# ames <- read_csv("https://tommasorigon.github.io/datamining/data/ames.csv")
ames <- read_csv("../data/ames.csv")

main_rec <- recipe(SalePrice ~ ., data = ames) %>%
  step_nzv(all_predictors(), unique_cut = 10)

ames <- bake(prep(main_rec), new_data = ames) %>%
  mutate(log_SalePrice = log(SalePrice))

# Three-way split: 50% train / 25% validation / 25% test ----------------------------------------
# The validation set is used for hyperparameter selection.
# The test set is kept untouched until the very end.
set.seed(1234)
split <- initial_validation_split(ames, prop = c(0.5, 0.25))

ames_tr <- training(split)
ames_val <- validation(split)
ames_te <- testing(split)

val_resample <- validation_set(split)

# Recipes -------------------------------------------------------------------------------------------

m_linear <- linear_reg() %>%
  set_engine("lm")

# The outcome is log_SalePrice; SalePrice is dropped from the predictor set.
base_recipe <- recipe(log_SalePrice ~ ., data = ames_tr) %>%
  step_rm(SalePrice)

# Metric: exponentiated MAE on the original dollar scale (defined in routines.R)
my_metrics <- metric_set(exp_mae)

# Benchmark: median prediction on the validation set
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

# Full OLS -------------------------------------------------------------------------------------

m_full <- workflow() %>%
  add_recipe(base_recipe) %>%
  add_model(m_linear) %>%
  fit(ames_tr)

print(tidy(m_full), n = 150)
augment(m_full, new_data = ames_val) %>% exp_mae(truth = log_SalePrice, estimate = .pred)

# PCR -----------------------------------------------------------------------------------------

wf_pcr <- workflow() %>%
  add_recipe(base_recipe %>% step_dummy(all_factor_predictors()) %>% step_normalize(all_predictors()) %>% step_pca(all_predictors(), num_comp = tune())) %>%
  add_model(m_linear)

pcr_val <- tune_grid(
  wf_pcr,
  resamples = val_resample,
  grid      = tibble(num_comp = 1:120),
  metrics   = my_metrics,
  control   = control_grid(save_workflow = TRUE, verbose = TRUE)
)

collect_metrics(pcr_val)
autoplot(pcr_val, metric = "exp_mae") + theme_bw()
show_best(pcr_val, metric = "exp_mae")

# Fit the selected model on the training set only
best_pcr_val <- select_best(pcr_val, metric = "exp_mae")
best_pcr_val <- finalize_workflow(wf_pcr, best_pcr_val) %>% fit(data = ames_tr)

tidy(best_pcr_val)

# Ridge -----------------------------------------------------------------------------------------

lambda_grid <- exp(seq(-6, 6, length.out = 100))

wf_ridge <- workflow() %>%
  add_recipe(base_recipe %>% step_dummy(all_factor_predictors())) %>%
  add_model(linear_reg(penalty = tune(), mixture = 0) %>% set_engine("glmnet", path_values = lambda_grid))

ridge_val <- tune_grid(
  wf_ridge,
  resamples = val_resample,
  grid      = tibble(penalty = lambda_grid),
  metrics   = my_metrics
)

collect_metrics(ridge_val)
autoplot(ridge_val, metric = "exp_mae") + theme_bw()
show_best(ridge_val, metric = "exp_mae")

# Fit the selected model on the training set only
best_ridge_val <- select_best(ridge_val, metric = "exp_mae")
best_ridge_val <- finalize_workflow(wf_ridge, best_ridge_val) %>% fit(data = ames_tr)

print(tidy(best_ridge_val), n = 15)

# Lasso -----------------------------------------------------------------------------------------

lambda_grid <- exp(seq(-10, 0, length.out = 100))

wf_lasso <- workflow() %>%
  add_recipe(base_recipe %>% step_dummy(all_factor_predictors())) %>%
  add_model(linear_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet", path_values = lambda_grid))

lasso_val <- tune_grid(
  wf_lasso,
  resamples = val_resample,
  grid      = tibble(penalty = lambda_grid),
  metrics   = my_metrics
)

collect_metrics(lasso_val)
autoplot(lasso_val, metric = "exp_mae") + theme_bw()
show_best(lasso_val, metric = "exp_mae")

# Fit the selected model on the training set only
best_lasso_val <- select_best(lasso_val, metric = "exp_mae")
best_lasso_val <- finalize_workflow(wf_lasso, best_lasso_val) %>% fit(data = ames_tr)

# Fit the selected model on the training set only
pct_loss_lasso_val <- select_by_pct_loss(lasso_val,
  metric = "exp_mae",
  desc(penalty), limit = 10
)
pct_loss_lasso_val <- finalize_workflow(wf_lasso, pct_loss_lasso_val) %>% fit(data = ames_tr)

print(tidy(pct_loss_lasso_val), n = 25)
tidy(pct_loss_lasso_val) %>% filter(estimate > 0)

# Elastic Net (mixture = 0.5) --------------------------------------------------------------------

lambda_grid <- exp(seq(-10, 0, length.out = 100))

wf_en <- workflow() %>%
  add_recipe(base_recipe %>% step_dummy(all_factor_predictors())) %>%
  add_model(linear_reg(penalty = tune(), mixture = 0.5) %>% set_engine("glmnet", path_values = lambda_grid))

en_val <- tune_grid(
  wf_en,
  resamples = val_resample,
  grid      = tibble(penalty = lambda_grid),
  metrics   = my_metrics
)

collect_metrics(en_val)
autoplot(en_val, metric = "exp_mae") + theme_bw()
show_best(en_val, metric = "exp_mae")

# Fit the selected model on the training set only
best_en_val <- select_best(en_val, metric = "exp_mae")
best_en_val <- finalize_workflow(wf_en, best_en_val) %>% fit(data = ames_tr)

print(tidy(best_en_val), n = 15)

pct_loss_en_val <- select_by_pct_loss(en_val,
  metric = "exp_mae",
  desc(penalty), limit = 10
)
pct_loss_en_val <- finalize_workflow(wf_lasso, pct_loss_en_val) %>% fit(data = ames_tr)
print(tidy(pct_loss_en_val), n = 15)

tidy(pct_loss_en_val) %>% filter(estimate > 0)

# Final comparison on the test set -----------------------------------------------------------------------------------------

fitted_models <- list(
  Simple = m_simple,
  Full = m_full,
  PCR = best_pcr_val,
  Ridge = best_ridge_val,
  Lasso = best_lasso_val,
  `Simple Lasso` = pct_loss_lasso_val,
  `Elastic Net` = best_en_val,
  `Simple Elastic Net` = pct_loss_en_val)

results <- imap_dfr(fitted_models, function(model, name) {
  augment(model, new_data = ames_te) %>%
    exp_mae(truth = log_SalePrice, estimate = .pred) %>%
    transmute(model = name, mae = .estimate)
})

results %>% arrange(mae)
