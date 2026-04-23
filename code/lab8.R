# ----------------------------------------
# Title: LAB 8 (Juice data, classification)
#        GAM and MARS
# Author: Tommaso Rigon
# ----------------------------------------

rm(list = ls())

library(tidyverse)
library(tidymodels)

# Data and preprocessing from LAB 5 ---------------------------------------------------------------
# See lab5.R for a full description of the dataset and the collinearity structure of the predictors.

juice <- read_table("https://tommasorigon.github.io/StatIII/data/juice.txt")

juice <- juice %>%
  mutate(choice = factor(choice), store = factor(store), id.cust = factor(id.cust)) %>%
  select(-c(StoreID, store7, buyCH))

set.seed(123)
split    <- initial_split(juice, prop = 3 / 4)
juice_tr <- training(split)
juice_te <- testing(split)

base_recipe <- recipe(choice ~ ., data = juice_tr) %>%
  step_dummy(all_factor_predictors()) %>%
  step_zv(all_predictors())

my_metrics   <- metric_set(roc_auc, mn_log_loss)
cv_samples   <- vfold_cv(juice_tr, v = 10)

# GAM — simple model -------------------------------------------------------------------------------
# Smooth terms are used for the two key continuous predictors.
# loyaltyCH and pricediff enter linearly as they did in the simple logistic model in LAB 5.
# k = 5 for loyaltyCH since it is bounded in [0, 1] and likely has limited curvature.

m_gam <- gen_additive_mod(select_features = FALSE) %>%
  set_engine("mgcv") %>%
  set_mode("classification")

m_gam_simple <- workflow() %>%
  add_model(
    m_gam,
    formula = choice ~ s(loyaltyCH, k = 5) + s(pricediff) + store
  ) %>%
  add_formula(choice ~ loyaltyCH + pricediff + store) %>%
  fit(data = juice_tr)

# Inspect the fitted smooth terms
extract_fit_engine(m_gam_simple) %>% summary()
extract_fit_engine(m_gam_simple) %>% plot(pages = 1)

# GAM — full model ---------------------------------------------------------------------------------

smooth_vars <- juice_tr %>%
  select(-c(choice, id.cust)) %>%
  select(where(is.numeric)) %>%
  names()

all_vars <- juice_tr %>%
  select(-c(choice)) %>%
  names()

mgcv_terms   <- ifelse(all_vars %in% smooth_vars, paste0("s(", all_vars, ")"), all_vars)
mgcv_formula <- as.formula(paste("choice ~", paste(mgcv_terms, collapse = " + ")))
plain_formula <- as.formula(paste("choice ~", paste(all_vars, collapse = " + ")))

m_gam_full <- workflow() %>%
  add_model(m_gam, formula = mgcv_formula) %>%
  add_formula(plain_formula) %>%
  fit(data = juice_tr)

extract_fit_engine(m_gam_full) %>% summary()

augment(m_gam_full, new_data = juice_tr) %>%
  my_metrics(truth = choice, .pred_CH)

# CV evaluation of GAM models ----------------------------------------------------------------------
# gen_additive_mod() does not support tune_grid(), so we use fit_resamples() with fixed specs.

cv_gam_simple <- workflow() %>%
  add_model(m_gam, formula = choice ~ s(loyaltyCH, k = 5) + s(pricediff) + store) %>%
  add_formula(choice ~ loyaltyCH + pricediff + store) %>%
  fit_resamples(resamples = cv_samples, metrics = my_metrics)

collect_metrics(cv_gam_simple)

cv_gam_full <- workflow() %>%
  add_model(m_gam, formula = mgcv_formula) %>%
  add_formula(plain_formula) %>%
  fit_resamples(resamples = cv_samples, metrics = my_metrics)

collect_metrics(cv_gam_full)

# MARS — exploratory fits --------------------------------------------------------------------------
# Direct earth() calls for quick exploration and variable importance inspection.
# base_recipe preprocessing (dummy encoding) is applied manually here via bake().

juice_tr_baked <- bake(prep(base_recipe), new_data = juice_tr)

m_mars1_expl <- earth(choice ~ ., data = juice_tr_baked, degree = 1, glm = list(family = binomial))
m_mars2_expl <- earth(choice ~ ., data = juice_tr_baked, degree = 2, glm = list(family = binomial))

summary(m_mars1_expl)
summary(m_mars2_expl)

# Variable importance: GCV contribution of each predictor across all basis functions containing it
evimp(m_mars1_expl)
plot(evimp(m_mars1_expl), cex.axis = 0.7)

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
  grid      = expand_grid(prod_degree = c(1, 2, 3), num_terms = c(5, 10, 20, 50)),
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
# Results from LAB 5 are included for reference. Re-fit those models from scratch if needed,
# or load them from a saved workspace.

fitted_models <- list(
  GAM_simple = m_gam_simple,
  GAM_full   = m_gam_full,
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