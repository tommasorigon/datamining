# ----------------------------------------
# Title: LAB 7 (Wisconsin Diabetic Retinopathy)
#        GAM and MARS
# Author: Tommaso Rigon
# ----------------------------------------

rm(list = ls())

library(tidyverse)
library(tidymodels)
library(mgcv)

# Data ---------------------------------------------------------------------------------------------

# The wesdr dataset records diabetic retinopathy progression in 669 patients
# from the Wisconsin Epidemiologic Study of Diabetic Retinopathy (Klein et al., 1988).
#
# Variables:
#   dur   - duration of diabetes at baseline (years)
#   gly   - glycosylated haemoglobin (HbA1c, %) — a marker of long-term glucose control
#   bmi   - body mass index (kg/m²)
#   ret   - retinopathy progression (0 = no, 1 = yes)  [binary response]
#
# Goal: model the probability of retinopathy progression as a function of
# the three continuous predictors using a GAM with binomial family.

library(gamair)
data(wesdr)

glimpse(wesdr)
summary(wesdr)

wesdr <- wesdr %>%
  mutate(ret = factor(ret, levels = c(0, 1), labels = c("no", "yes")))

# Train / test split (75% / 25%) -------------------------------------------------------------------

set.seed(123)
split <- initial_split(wesdr, prop = 3 / 4)
wesdr_tr <- training(split)
wesdr_te <- testing(split)

# Cross-validation samples (10-fold, stratified) for model selection
cv_samples <- vfold_cv(wesdr_tr, v = 10, strata = ret)

# Metrics: ROC-AUC and log-loss
my_metrics <- metric_set(roc_auc, mn_log_loss)

# ---- Exploratory analysis ------------------------------------------------------------------------

wesdr_tr %>%
  pivot_longer(c(dur, gly, bmi), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = variable, y = value, fill = ret)) +
  geom_boxplot() +
  labs(
    title = "Distribution of predictors by retinopathy progression",
    fill = "Progression"
  ) +
  theme_light()

# ---- Logistic regression (linear baseline) -------------------------------------------------------

m_glm <- logistic_reg() %>%
  set_engine("glm")

wf_glm <- workflow() %>%
  add_model(m_glm) %>%
  add_formula(ret ~ dur + gly + bmi)

cv_glm <- fit_resamples(wf_glm, resamples = cv_samples, metrics = my_metrics)
collect_metrics(cv_glm)

m_glm <- wf_glm %>% fit(data = wesdr_tr)
tidy(m_glm)

# ---- GAM — mgcv directly (for inspection) -------------------------------------------------------

m_gam_simple <- gam(ret ~ s(dur) + s(gly) + s(bmi), family = binomial, method = "REML", data = wesdr_tr)
summary(m_gam_simple)

plot(m_gam_simple,
  pages = 1, scheme = 1, shade = TRUE,
  shade.col = "lightblue", rug = TRUE,
  main = "Partial effects (log-odds scale)"
)

m_gam_full <- gam(
  ret ~ s(dur) + s(gly) + s(bmi) + ti(dur, gly) + ti(dur, bmi) + ti(gly, bmi),
  select = TRUE,
  data = wesdr_tr, family = binomial(), method = "REML"
)

summary(m_gam_full)
plot(m_gam_full, pages = 1, scheme = 1, zlim = c(-3, 3))

# ---- GAM — tidymodels workflow -------------------------------------------------------------------

m_gam <- gen_additive_mod(select_features = FALSE) %>%
  set_engine("mgcv") %>%
  set_mode("classification")

# Simple GAM: all three predictors as smooth terms (default k and REML)
wf_gam_simple <- workflow() %>%
  add_model(m_gam, formula = ret ~ s(dur) + s(gly) + s(bmi)) %>%
  add_formula(ret ~ dur + gly + bmi)

cv_gam_simple <- wf_gam_simple %>%
  fit_resamples(resamples = cv_samples, metrics = my_metrics)
collect_metrics(cv_gam_simple)

m_gam_simple <- wf_gam_simple %>% fit(data = wesdr_tr)

# GAM: all three predictors as smooth terms + interaction term
wf_gam_full <- workflow() %>%
  add_model(m_gam, formula = ret ~ s(dur) + s(gly) + s(bmi) + ti(gly, bmi)) %>%
  add_formula(ret ~ dur + gly + bmi)

cv_gam_full <- wf_gam_full %>%
  fit_resamples(resamples = cv_samples, metrics = my_metrics)
collect_metrics(cv_gam_full)

m_gam_full <- wf_gam_full %>% fit(data = wesdr_tr)

# ---- MARS — direct fit (for inspection) ----------------------------------------------------------

library(earth)

mars_fit <- earth(
  ret ~ dur + gly + bmi,
  data = wesdr_tr,
  glm = list(family = binomial),
  degree = 1
)

summary(mars_fit)
print(mars_fit)

# Variable importance: GCV contribution of each predictor across all basis functions.
evimp(mars_fit)

# MARS with two-way interactions (degree = 2)
mars_fit2 <- earth(
  ret ~ dur + gly + bmi,
  data   = wesdr_tr,
  glm    = list(family = binomial),
  degree = 2
)

summary(mars_fit2)
evimp(mars_fit2)

# ---- MARS — tuning prod_degree and num_terms via 10-fold CV --------------------------------------

base_recipe <- recipe(ret ~ dur + gly + bmi, data = wesdr_tr)

wf_mars <- workflow() %>%
  add_recipe(base_recipe) %>%
  add_model(
    mars(prod_degree = tune(), mode = "classification") %>%
      set_engine("earth")
  )

cv_mars <- tune_grid(
  wf_mars,
  resamples = cv_samples,
  grid      = expand_grid(prod_degree = c(1, 2, 3)),
  metrics   = my_metrics
)

collect_metrics(cv_mars)
autoplot(cv_mars, metric = "mn_log_loss") + theme_bw()
autoplot(cv_mars, metric = "roc_auc") + theme_bw()

show_best(cv_mars, metric = "mn_log_loss")
show_best(cv_mars, metric = "roc_auc")

best_cv_mars <- select_best(cv_mars, metric = "mn_log_loss")
best_cv_mars <- finalize_workflow(wf_mars, best_cv_mars) %>% fit(data = wesdr_tr)
best_cv_mars

# ---- Final model fit and test-set evaluation -----------------------------------------------------

fitted_models <- list(
  GLM = m_glm,
  GAM_simple = m_gam_simple,
  GAM_full = m_gam_full,
  MARS = best_cv_mars
)

results <- imap_dfr(fitted_models, function(model, name) {
  augment(model, new_data = wesdr_te) %>%
    my_metrics(truth = ret, .pred_no) %>%
    mutate(model = name)
})

results %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  arrange(mn_log_loss)
