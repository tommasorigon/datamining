# ----------------------------------------
# Title: LAB 8 (Juice data, classification)
#        GAM and MARS
# Author: Tommaso Rigon
# ----------------------------------------

rm(list = ls())

library(tidyverse)
library(tidymodels)

# Data and preprocessing from LAB 5 ---------------------------------------------------------------
juice <- read_table("https://tommasorigon.github.io/StatIII/data/juice.txt")

juice <- juice %>%
  mutate(choice = factor(choice), store = factor(store), id.cust = factor(id.cust)) %>%
  select(-c(StoreID, store7, buyCH))

set.seed(123)
split <- initial_split(juice, prop = 3 / 4)
juice_tr <- training(split)
juice_te <- testing(split)

base_recipe <- recipe(choice ~ ., data = juice_tr) %>%
  step_dummy(all_factor_predictors()) %>%
  step_zv(all_predictors())

my_metrics <- metric_set(roc_auc, mn_log_loss)
cv_samples <- vfold_cv(juice_tr, v = 10)

# GAM — simple model -------------------------------------------------------------------------------
m_gam <- gen_additive_mod(select_features = FALSE) %>%
  set_engine("mgcv") %>%
  set_mode("classification")

cv_gam_simple <- workflow() %>%
  add_model(m_gam, formula = choice ~ s(loyaltyCH, k = 5) + s(pricediff) + store) %>%
  add_formula(choice ~ loyaltyCH + pricediff + store) %>%
  fit_resamples(resamples = cv_samples, metrics = my_metrics)

collect_metrics(cv_gam_simple)

# MARS — tuning prod_degree and num_terms via 10-fold CV -------------------------------------------

wf_mars <- workflow() %>%
  add_recipe(base_recipe) %>%
  add_model(
    mars(prod_degree = tune(), num_terms = tune(), mode = "classification") %>%
      set_engine("earth")
  )

cv_mars <- tune_grid(
  wf_mars,
  resamples = cv_samples,
  grid      = expand_grid(prod_degree = c(1, 2), num_terms = c(2, 5, 10)),
  metrics   = my_metrics
)

collect_metrics(cv_mars)
autoplot(cv_mars, metric = "mn_log_loss") + theme_bw()
autoplot(cv_mars, metric = "roc_auc") + theme_bw()

show_best(cv_mars, metric = "mn_log_loss")
show_best(cv_mars, metric = "roc_auc")

best_cv_mars <- select_best(cv_mars, metric = "mn_log_loss")
best_cv_mars <- finalize_workflow(wf_mars, best_cv_mars) %>% fit(data = juice_tr)

# Final comparison on the test set -----------------------------------------------------------------

fitted_models <- list(
  GAM_simple = m_gam_simple,
  MARS       = best_cv_mars
)

results <- imap_dfr(fitted_models, function(model, name) {
  augment(model, new_data = juice_te) %>%
    my_metrics(truth = choice, .pred_CH) %>%
    mutate(model = name)
})

results %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  arrange(mn_log_loss)
