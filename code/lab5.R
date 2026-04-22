# ----------------------------------------
# Title: LAB 3 (Juice data, classification)
# Author: Tommaso Rigon
# ----------------------------------------

rm(list = ls())

# The data on fruit juice purchases are taken from Chapter 11 of Foster, Stine and Waterman "Business Analysis Using Regression".

# The data refer to 1070 fruit juice purchases of two different brands (MM and CH) in certain US supermarkets, supplied with some contributory variables. The variables are

# Variable     Description

# choice       pre-chosen brand (factor, with 2 levels)
# id.cust      customer identification
# week         identifier of week of purchase
# priceCH      reference price for  brand CH (USD)
# priceMM      reference price for  brand MM (USD)
# discountCH   discount applied to  product CH (USD)
# discountMM   discount applied to product MM (USD)
# loyaltyCH    loyalty indicator for  product CH
# loyaltyMM    loyalty indicator for  product MM
# store        store identifier (factor, with 5 levels)
# ...          other variables obtained by combining the former

# Variable loyaltyMM is constructed starting from the value 0.5 and updating with every purchase by the same customer, with a value which increases by 20% of the current difference between the current value and 1, if the customer chose MM, and falls by 20% of the  difference between the current value and 0 if the customer chose CH. The corresponding variable loyaltyCH is given by  1-loyaltyMM.

library(tidyverse)
library(tidymodels)

juice <- read_table("https://tommasorigon.github.io/StatIII/data/juice.txt")

glimpse(juice)

# buyCH and choice represent the same variable in different formats (factor vs int). One or the other must be removed
# store, StoreID, store7 are referring to the same quantity
juice <- juice %>% 
  mutate(choice = factor(choice), store = factor(store), id.cust = factor(id.cust)) %>% 
  select(-c(StoreID, store7, buyCH))

# salepriceCH is the FINAL price, obtained as priceCH - discountCH, making these three variables COLLINEAR.
# We can keep them but we cannot use them together
plot(juice$priceCH - juice$discountCH, juice$salepriceCH)
plot(juice$priceMM - juice$discountMM, juice$salepriceMM)

# pricediff and listpricediff are also collinear variables, being equal to
# pricediff = salepriceMM - salepriceCH;
# listpricediff = priceMM - priceCH
plot(juice$salepriceMM - juice$salepriceCH, juice$pricediff)
plot(juice$priceMM - juice$priceCH, juice$listpricediff)

# pctdiscMM and pctdiscCH are also potentially problematic, albeit non-collinear. specialCH are "special weeks", again potentially problematic

# Three-way split: 50 % train / 25 % validation / 25 % test ----------------------------------------
set.seed(123)
split <- initial_split(juice, prop = 3/4)

juice_tr <- training(split)
juice_te <- testing(split) # kept untouched until the very end

# Recipes -------------------------------------------------------------------------------------------

# Model
m_logit <- logistic_reg() %>%
  set_engine("glm")

# The outcome is log_SalePrice; SalePrice is dropped from the predictor set.
base_recipe <- recipe(choice ~ ., data = juice_tr) %>%
  step_dummy(all_factor_predictors()) %>%
  step_zv(all_predictors())

# Shrinkage methods additionally require centring and scaling
shrinkage_recipe <- base_recipe %>%
  step_normalize(all_predictors())

# Metrics (original dollar scale)
my_metrics <- metric_set(roc_auc, mn_log_loss)

# Cross-validation
cv_samples <- vfold_cv(juice_tr, v = 10)

# Simple GLM -----------------------------------------------------------------------------------------

wf_simple <- workflow() %>%
  add_recipe(recipe(choice ~ loyaltyCH + pricediff + store, data = juice_tr)) %>%
  add_model(m_logit) 

cv_simple <- wf_simple %>%
  fit_resamples(resamples = cv_samples,
                metrics = my_metrics)

collect_metrics(cv_simple)

# Final model, using the entire validation set
m_simple <- wf_simple %>% fit(data = juice_tr)
tidy(m_simple)

# FULL GLM -----------------------------------------------------------------------------------------

wf_full <- workflow() %>%
  add_recipe(base_recipe) %>%
  add_model(m_logit) 

cv_full <- wf_full %>%
  fit_resamples(resamples = cv_samples,
                metrics = my_metrics)

collect_metrics(cv_full)

# Final model, using the entire validation set
m_full <- wf_full %>% fit(data = juice_tr)
tidy(m_full)

# PCR -----------------------------------------------------------------------------------------

wf_pcr <- workflow() %>%
  add_recipe(shrinkage_recipe %>% step_pca(all_predictors(), num_comp = tune())) %>%
  add_model(m_logit)

pcr_cv <- tune_grid(
  wf_pcr,
  resamples = cv_samples,
  grid = tibble(num_comp = c(1:20, seq(from = 20, to = 90, by = 5))),
  metrics = my_metrics,
  control = control_grid(save_workflow = TRUE, verbose = TRUE)
)

collect_metrics(pcr_cv)

autoplot(pcr_cv, metric = "roc_auc") + theme_bw()
autoplot(pcr_cv, metric = "mn_log_loss") + theme_bw()

show_best(pcr_cv, metric = "roc_auc")
show_best(pcr_cv, metric = "mn_log_loss")

# Select final best model (including validation set)
best_pcr_cv <- select_best(pcr_cv, metric = "mn_log_loss")
best_pcr_cv <- finalize_workflow(wf_pcr, best_pcr_cv) %>% fit(data = juice_tr)

tidy(best_pcr_cv)

# Ridge -----------------------------------------------------------------------------------------
wf_ridge <- workflow() %>%
  add_recipe(shrinkage_recipe) %>%
  add_model(logistic_reg(penalty = tune(), mixture = 0) %>% set_engine("glmnet"))

ridge_cv <- tune_grid(
  wf_ridge,
  resamples = cv_samples,
  grid      = tibble(penalty = exp(seq(-4, 5.5, length.out = 100))),
  metrics   = my_metrics
)

collect_metrics(ridge_cv)

autoplot(ridge_cv, metric = "roc_auc") + theme_bw()
autoplot(ridge_cv, metric = "mn_log_loss") + theme_bw()
show_best(ridge_cv, metric = "roc_auc")
show_best(ridge_cv, metric = "mn_log_loss")

# Select final best model (including validation set)
best_ridge_cv <- select_best(ridge_cv, metric = "mn_log_loss")
best_ridge_cv <- finalize_workflow(wf_ridge, best_ridge_cv) %>% fit(data = juice_tr)

print(tidy(best_ridge_cv), n = 15)

# Lasso -----------------------------------------------------------------------------------------
wf_lasso <- workflow() %>%
  add_recipe(shrinkage_recipe) %>%
  add_model(logistic_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet"))

lasso_cv <- tune_grid(
  wf_lasso,
  resamples = cv_samples,
  grid      = tibble(penalty = exp(seq(-10, -2, length.out = 100))),
  metrics   = my_metrics
)

collect_metrics(lasso_cv)

autoplot(lasso_cv, metric = "roc_auc") + theme_bw()
autoplot(lasso_cv, metric = "mn_log_loss") + theme_bw()

show_best(lasso_cv, metric = "roc_auc")
show_best(lasso_cv, metric = "mn_log_loss")

# Select final best model (including validation set)
best_lasso_cv <- select_best(lasso_cv, metric = "mn_log_loss")
best_lasso_cv <- finalize_workflow(wf_lasso, best_lasso_cv) %>% fit(data = juice_tr)

print(tidy(best_lasso_cv), n = 40)

# Elastic Net -----------------------------------------------------------------------------------------
wf_en <- workflow() %>%
  add_recipe(shrinkage_recipe) %>%
  add_model(logistic_reg(penalty = tune(), mixture = 0.5) %>% set_engine("glmnet"))

en_cv <- tune_grid(
  wf_en,
  resamples = cv_samples,
  grid      = tibble(penalty = exp(seq(-10, -2, length.out = 100))),
  metrics   = my_metrics
)

autoplot(en_cv, metric = "roc_auc") + theme_bw()
autoplot(en_cv, metric = "mn_log_loss") + theme_bw()

show_best(en_cv, metric = "roc_auc")
show_best(en_cv, metric = "mn_log_loss")

# Select final best model (including validation set)
best_en_cv <- select_best(en_cv, metric = "mn_log_loss")
best_en_cv <- finalize_workflow(wf_en, best_en_cv) %>% fit(data = juice_tr)

print(tidy(best_lasso_cv), n = 40)

# Final comparison on the test set -----------------------------------------------------------------------------------------

fitted_models <- list(
  Simple        = m_simple,
  Full          = m_full,
  PCR           = best_pcr_cv,
  Ridge         = best_ridge_cv,
  Lasso         = best_lasso_cv,
  `Elastic Net` = best_en_cv)

results <- imap_dfr(fitted_models, function(model, name) {
  augment(model, new_data = juice_te) %>%
    my_metrics(truth = choice, .pred_CH) %>% mutate(model = name)
})

results %>% pivot_wider(names_from = .metric, values_from = .estimate) %>% arrange(mn_log_loss)
