# ----------------------------------------
# Title: LAB 5 (Juice data, classification)
# Author: Tommaso Rigon
# ----------------------------------------

rm(list = ls())

# The data on fruit juice purchases are taken from Chapter 11 of
# Foster, Stine and Waterman "Business Analysis Using Regression".

# The dataset records 1070 fruit juice purchases of two brands (MM and CH)
# in US supermarkets. Variables:
#
# choice       purchased brand (factor, 2 levels: CH / MM)
# id.cust      customer identifier
# week         week of purchase
# priceCH      shelf price for brand CH (USD)
# priceMM      shelf price for brand MM (USD)
# discountCH   discount applied to CH (USD)
# discountMM   discount applied to MM (USD)
# loyaltyCH    loyalty indicator for CH  (= 1 - loyaltyMM)
# loyaltyMM    loyalty indicator for MM  (updated by +/- 20% of gap to 1/0 at each purchase)
# store        store identifier (factor, 5 levels)
# ...          other variables derived from the above

library(tidyverse)
library(tidymodels)

juice <- read_table("https://tommasorigon.github.io/StatIII/data/juice.txt")

glimpse(juice)

# buyCH and choice represent the same variable in different formats (factor vs int)
# store, StoreID, store7 refer to the same quantity
juice <- juice %>%
  mutate(choice = factor(choice), store = factor(store), id.cust = factor(id.cust)) %>%
  select(-c(StoreID, store7, buyCH))

# salepriceCH = priceCH - discountCH  =>  perfect collinearity among the three
plot(juice$priceCH - juice$discountCH, juice$salepriceCH)
plot(juice$priceMM - juice$discountMM, juice$salepriceMM)

# pricediff    = salepriceMM - salepriceCH  (collinear with its components)
# listpricediff = priceMM - priceCH         (idem)
plot(juice$salepriceMM - juice$salepriceCH, juice$pricediff)
plot(juice$priceMM - juice$priceCH, juice$listpricediff)

# pctdiscMM, pctdiscCH, specialCH are also potentially problematic


# Train / test split: 75% train / 25% test ----------------------------------------
# Model selection is performed via 10-fold CV on the training set (no separate validation set).

set.seed(123)
split <- initial_split(juice, prop = 3 / 4)

juice_tr <- training(split)
juice_te <- testing(split) # kept untouched until the very end


# Recipes -------------------------------------------------------------------------------------------

m_logit <- logistic_reg() %>%
  set_engine("glm")

# Base recipe: dummy-encode factors, remove zero-variance predictors
base_recipe <- recipe(choice ~ ., data = juice_tr) %>%
  step_dummy(all_factor_predictors()) %>%
  step_zv(all_predictors())

# Shrinkage methods additionally require centring and scaling
shrinkage_recipe <- base_recipe %>%
  step_normalize(all_predictors())

# Metrics
my_metrics <- metric_set(roc_auc, mn_log_loss)

# 10-fold cross-validation on the training set
cv_samples <- vfold_cv(juice_tr, v = 10)


# Simple GLM -----------------------------------------------------------------------------------------

wf_simple <- workflow() %>%
  add_recipe(recipe(choice ~ loyaltyCH + pricediff + store, data = juice_tr)) %>%
  add_model(m_logit)

cv_simple <- wf_simple %>%
  fit_resamples(resamples = cv_samples, metrics = my_metrics)

collect_metrics(cv_simple)

m_simple <- wf_simple %>% fit(data = juice_tr)
tidy(m_simple)


# Full GLM -----------------------------------------------------------------------------------------

wf_full <- workflow() %>%
  add_recipe(base_recipe) %>%
  add_model(m_logit)

cv_full <- wf_full %>%
  fit_resamples(resamples = cv_samples, metrics = my_metrics)

collect_metrics(cv_full)

m_full <- wf_full %>% fit(data = juice_tr)
tidy(m_full)


# PCR -----------------------------------------------------------------------------------------

wf_pcr <- workflow() %>%
  add_recipe(shrinkage_recipe %>% step_pca(all_predictors(), num_comp = tune())) %>%
  add_model(m_logit)

cv_pcr <- tune_grid(
  wf_pcr,
  resamples = cv_samples,
  grid      = tibble(num_comp = c(1:20, seq(from = 20, to = 90, by = 5))),
  metrics   = my_metrics,
  control   = control_grid(save_workflow = TRUE, verbose = TRUE)
)

collect_metrics(cv_pcr)

autoplot(cv_pcr, metric = "roc_auc") + theme_bw()
autoplot(cv_pcr, metric = "mn_log_loss") + theme_bw()

show_best(cv_pcr, metric = "roc_auc")
show_best(cv_pcr, metric = "mn_log_loss")

best_cv_pcr <- select_best(cv_pcr, metric = "mn_log_loss")
best_cv_pcr <- finalize_workflow(wf_pcr, best_cv_pcr) %>% fit(data = juice_tr)

tidy(best_cv_pcr)


# Ridge -----------------------------------------------------------------------------------------

wf_ridge <- workflow() %>%
  add_recipe(shrinkage_recipe) %>%
  add_model(logistic_reg(penalty = tune(), mixture = 0) %>% set_engine("glmnet"))

cv_ridge <- tune_grid(
  wf_ridge,
  resamples = cv_samples,
  grid      = tibble(penalty = exp(seq(-4, 5.5, length.out = 100))),
  metrics   = my_metrics
)

collect_metrics(cv_ridge)

autoplot(cv_ridge, metric = "roc_auc") + theme_bw()
autoplot(cv_ridge, metric = "mn_log_loss") + theme_bw()

show_best(cv_ridge, metric = "roc_auc")
show_best(cv_ridge, metric = "mn_log_loss")

best_cv_ridge <- select_best(cv_ridge, metric = "mn_log_loss")
best_cv_ridge <- finalize_workflow(wf_ridge, best_cv_ridge) %>% fit(data = juice_tr)

print(tidy(best_cv_ridge), n = 15)


# Lasso -----------------------------------------------------------------------------------------

wf_lasso <- workflow() %>%
  add_recipe(shrinkage_recipe) %>%
  add_model(logistic_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet"))

cv_lasso <- tune_grid(
  wf_lasso,
  resamples = cv_samples,
  grid      = tibble(penalty = exp(seq(-10, -2, length.out = 100))),
  metrics   = my_metrics
)

collect_metrics(cv_lasso)

autoplot(cv_lasso, metric = "roc_auc") + theme_bw()
autoplot(cv_lasso, metric = "mn_log_loss") + theme_bw()

show_best(cv_lasso, metric = "roc_auc")
show_best(cv_lasso, metric = "mn_log_loss")

best_cv_lasso <- select_best(cv_lasso, metric = "mn_log_loss")
best_cv_lasso <- finalize_workflow(wf_lasso, best_cv_lasso) %>% fit(data = juice_tr)

print(tidy(best_cv_lasso), n = 15)


# Elastic Net (mixture = 0.5) -----------------------------------------------------------------------------------------

wf_en <- workflow() %>%
  add_recipe(shrinkage_recipe) %>%
  add_model(logistic_reg(penalty = tune(), mixture = 0.5) %>% set_engine("glmnet"))

cv_en <- tune_grid(
  wf_en,
  resamples = cv_samples,
  grid      = tibble(penalty = exp(seq(-10, -2, length.out = 100))),
  metrics   = my_metrics
)

autoplot(cv_en, metric = "roc_auc") + theme_bw()
autoplot(cv_en, metric = "mn_log_loss") + theme_bw()

show_best(cv_en, metric = "roc_auc")
show_best(cv_en, metric = "mn_log_loss")

best_cv_en <- select_best(cv_en, metric = "mn_log_loss")
best_cv_en <- finalize_workflow(wf_en, best_cv_en) %>% fit(data = juice_tr)

print(tidy(best_cv_en), n = 20) # was erroneously printing best_cv_lasso


# Final comparison on the test set -----------------------------------------------------------------------------------------

fitted_models <- list(
  Simple        = m_simple,
  Full          = m_full,
  PCR           = best_cv_pcr,
  Ridge         = best_cv_ridge,
  Lasso         = best_cv_lasso,
  `Elastic Net` = best_cv_en
)

results <- imap_dfr(fitted_models, function(model, name) {
  augment(model, new_data = juice_te) %>%
    my_metrics(truth = choice, .pred_CH) %>%
    mutate(model = name)
})

results %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  arrange(mn_log_loss)
