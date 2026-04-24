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

wesdr <- wesdr %>% mutate(ret = factor(ret))

# Train / test split (75% / 25%) -------------------------------------------------------------------

set.seed(123)
split    <- initial_split(wesdr, prop = 3 / 4, strata = ret)
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
  labs(title = "Distribution of predictors by retinopathy progression",
       fill = "Progression") +
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

gam_fit <- gam(
  ret ~ s(dur) + s(gly) + s(bmi),
  family = binomial,
  method = "REML",
  data   = wesdr_tr
)

b <- gam(ret~s(dur,k=k)+s(gly,k=k)+s(bmi,k=k)+ti(dur,gly,k=k)+
           ti(dur,bmi,k=k)+ti(gly,bmi,k=k),select=TRUE,
         data=wesdr,family=binomial(),method="ML")
plot(b,pages=1,scheme=1,zlim=c(-3,3))

summary(gam_fit)

# The summary reports:
#   - approximate significance of each smooth (chi-squared test)
#   - estimated degrees of freedom (edf): edf ≈ 1 → nearly linear; edf >> 1 → strong non-linearity
#   - deviance explained and adjusted R² on the linear predictor scale

# Partial effect plots: each panel shows s(x_j) on the log-odds scale,
# centred so that the intercept absorbs the mean.
# The grey bands are ±2 SE pointwise confidence intervals.
# A rug at the bottom marks the observed data values.

plot(gam_fit, pages = 1, scheme = 1, shade = TRUE,
     shade.col = "lightblue", rug = TRUE,
     main = "Partial effects (log-odds scale)")

# Checking model assumptions:
#   - gam.check() plots residuals and prints a basis adequacy test.
#     If the k-index < 1 and p-value is small, increase k.

gam.check(gam_fit)

# Concurvity: the analogue of multicollinearity for smooth terms.
# Values close to 1 indicate that one smooth is nearly a function of the others.

concurvity(gam_fit, full = FALSE)

# ---- Testing linearity of individual terms -------------------------------------------------------

# Is the non-linear smooth for gly significantly better than a linear term?
# We compare nested models via an approximate chi-squared test (ML criterion).

gam_fit_ml  <- gam(ret ~ s(dur) + s(gly) + s(bmi),
                   family = binomial, method = "ML", data = wesdr_tr)
gam_lin_gly <- gam(ret ~ s(dur) + gly    + s(bmi),
                   family = binomial, method = "ML", data = wesdr_tr)
gam_lin_bmi <- gam(ret ~ s(dur) + s(gly) + bmi,
                   family = binomial, method = "ML", data = wesdr_tr)

anova(gam_lin_gly, gam_fit_ml, test = "Chisq")  # non-linearity of gly
anova(gam_lin_bmi, gam_fit_ml, test = "Chisq")  # non-linearity of bmi

# ---- GAM — tidymodels workflow -------------------------------------------------------------------

m_gam <- gen_additive_mod(select_features = FALSE) %>%
  set_engine("mgcv") %>%
  set_mode("classification")

# Simple GAM: all three predictors as smooth terms (default k and REML)
cv_gam_simple <- workflow() %>%
  add_model(m_gam, formula = ret ~ s(dur) + s(gly) + s(bmi)) %>%
  add_formula(ret ~ dur + gly + bmi) %>%
  fit_resamples(resamples = cv_samples, metrics = my_metrics)

collect_metrics(cv_gam_simple)

# GAM with restricted df: bmi enters linearly (motivated by anova tests above)
cv_gam_mixed <- workflow() %>%
  add_model(m_gam, formula = ret ~ s(dur) + s(gly) + bmi) %>%
  add_formula(ret ~ dur + gly + bmi) %>%
  fit_resamples(resamples = cv_samples, metrics = my_metrics)

collect_metrics(cv_gam_mixed)

# GAM with increased basis dimension for dur (retinopathy onset often non-monotone in time)
cv_gam_k <- workflow() %>%
  add_model(m_gam, formula = ret ~ s(dur, k = 15) + s(gly) + s(bmi)) %>%
  add_formula(ret ~ dur + gly + bmi) %>%
  fit_resamples(resamples = cv_samples, metrics = my_metrics)

collect_metrics(cv_gam_k)

# ---- MARS — direct fit (for inspection) ----------------------------------------------------------

library(earth)

# MARS with additive basis functions only (degree = 1, no interactions).
# earth() with glm = list(family = binomial) fits a logistic MARS model.
# The hinge functions it selects are piecewise-linear analogues of the GAM smooths above.

mars_fit <- earth(
  ret ~ dur + gly + bmi,
  data  = wesdr_tr,
  glm   = list(family = binomial),
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
    mars(prod_degree = tune(), num_terms = tune(), mode = "classification") %>%
      set_engine("earth")
  )

cv_mars <- tune_grid(
  wf_mars,
  resamples = cv_samples,
  grid      = expand_grid(prod_degree = c(1, 2), num_terms = 2:8),
  metrics   = my_metrics
)

collect_metrics(cv_mars)
autoplot(cv_mars, metric = "mn_log_loss") + theme_bw()
autoplot(cv_mars, metric = "roc_auc")     + theme_bw()

show_best(cv_mars, metric = "mn_log_loss")
show_best(cv_mars, metric = "roc_auc")

best_cv_mars <- select_best(cv_mars, metric = "mn_log_loss")
best_cv_mars <- finalize_workflow(wf_mars, best_cv_mars) %>% fit(data = wesdr_tr)

# ---- Cross-validation comparison -----------------------------------------------------------------

cv_results <- bind_rows(
  collect_metrics(cv_glm)        %>% mutate(model = "GLM (linear)"),
  collect_metrics(cv_gam_simple) %>% mutate(model = "GAM (all smooth)"),
  collect_metrics(cv_gam_mixed)  %>% mutate(model = "GAM (bmi linear)"),
  collect_metrics(cv_gam_k)      %>% mutate(model = "GAM (k=15 for dur)"),
  collect_metrics(cv_mars)  %>%
    filter(prod_degree == best_cv_mars$fit$actions$model$spec$args$prod_degree,
           num_terms   == best_cv_mars$fit$actions$model$spec$args$num_terms) %>%
    mutate(model = "MARS (tuned)")
)

cv_results %>%
  select(model, .metric, mean, std_err) %>%
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) %>%
  arrange(mean_mn_log_loss)

# ---- Final model fit and test-set evaluation -----------------------------------------------------

# Select the best GAM based on CV log-loss, refit on the full training set.

best_wf <- workflow() %>%
  add_model(m_gam, formula = ret ~ s(dur, k = 15) + s(gly) + s(bmi)) %>%
  add_formula(ret ~ dur + gly + bmi) %>%
  fit(data = wesdr_tr)

# Test-set performance: all three model classes side by side

fitted_models <- list(
  GLM  = workflow() %>%
    add_model(m_glm) %>%
    add_formula(ret ~ dur + gly + bmi) %>%
    fit(data = wesdr_tr),
  GAM  = best_wf,
  MARS = best_cv_mars
)

results <- imap_dfr(fitted_models, function(model, name) {
  augment(model, new_data = wesdr_te) %>%
    my_metrics(truth = ret, .pred_yes) %>%
    mutate(model = name)
})

results %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  arrange(mn_log_loss)

# Confusion matrix for the best GAM at the 0.5 threshold
augment(best_wf, new_data = wesdr_te) %>%
  mutate(.pred_class = factor(ifelse(.pred_yes > 0.5, "yes", "no"), levels = c("no","yes"))) %>%
  conf_mat(truth = ret, estimate = .pred_class)

# Overlaid ROC curves for all three models
roc_data <- imap_dfr(fitted_models, function(model, name) {
  augment(model, new_data = wesdr_te) %>%
    roc_curve(truth = ret, .pred_yes) %>%
    mutate(model = name)
})

ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity, colour = model)) +
  geom_line(linewidth = 1) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(GLM = "grey40", GAM = "steelblue", MARS = "tomato")) +
  labs(title = "ROC curves — test set", colour = NULL) +
  theme_bw()

# ---- Visualising predicted probabilities ---------------------------------------------------------

# Marginal effect of dur holding gly and bmi at their median values.
# This is the GAM analogue of an "adjusted prediction" plot.

gam_final <- extract_fit_engine(best_wf)

pred_grid_dur <- tibble(
  dur = seq(min(wesdr$dur), max(wesdr$dur), length.out = 200),
  gly = median(wesdr_tr$gly),
  bmi = median(wesdr_tr$bmi)
)

pred_grid_dur <- pred_grid_dur %>%
  mutate(pred = predict(gam_final, newdata = ., type = "response"))

ggplot(pred_grid_dur, aes(x = dur, y = pred)) +
  geom_line(colour = "steelblue", linewidth = 1.2) +
  geom_rug(data = wesdr_tr, aes(x = dur, y = NULL), sides = "b", alpha = 0.3) +
  labs(x = "Duration of diabetes (years)",
       y = "P(retinopathy progression)",
       title = "Marginal effect of duration (gly and bmi at median)") +
  theme_bw()

# Joint effect of dur and gly on a grid (bmi at median) — colour-coded probability surface.

pred_grid_2d <- expand_grid(
  dur = seq(min(wesdr$dur), max(wesdr$dur), length.out = 60),
  gly = seq(min(wesdr$gly), max(wesdr$gly), length.out = 60),
  bmi = median(wesdr_tr$bmi)
) %>%
  mutate(pred = predict(gam_final, newdata = ., type = "response"))

ggplot(pred_grid_2d, aes(x = dur, y = gly, fill = pred)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdYlBu", direction = -1,
                       name = "P(progression)") +
  labs(x = "Duration (years)", y = "HbA1c (%)",
       title = "Predicted probability surface (BMI at median)") +
  theme_bw()