# ----------------------------------------
# Title: LAB 7 (Ames Housing, regression)
#        GAM and MARS
# Author: Tommaso Rigon
# ----------------------------------------

rm(list = ls())

library(tidyverse)
library(tidymodels)
source("https://tommasorigon.github.io/datamining/code/routines.R", echo = TRUE)

# Data ---------------------------------------------------------------------------------------------

ames <- read_csv("https://tommasorigon.github.io/datamining/data/ames.csv")

main_rec <- recipe(SalePrice ~ ., data = ames) %>%
  step_nzv(all_predictors(), unique_cut = 10)

ames <- bake(prep(main_rec), new_data = ames) %>%
  mutate(log_SalePrice = log(SalePrice))

# Three-way split: 50% train / 25% validation / 25% test ----------------------------------------

set.seed(123)
split <- initial_validation_split(ames, prop = c(0.5, 0.25))

ames_tr  <- training(split)
ames_val <- validation(split)
ames_te  <- testing(split)

val_resample <- validation_set(split)

m_linear <- linear_reg() %>%
  set_engine("lm")

# The outcome is log_SalePrice; SalePrice is dropped from the predictor set.

base_recipe <- recipe(log_SalePrice ~ ., data = ames_tr) %>%
  step_rm(SalePrice) %>%
  step_dummy(all_factor_predictors())

# Shrinkage methods additionally require centring and scaling.
shrinkage_recipe <- base_recipe %>%
  step_normalize(all_predictors())

# Metric: exponentiated MAE on the original dollar scale (defined in routines.R)
my_metrics <- metric_set(exp_mae)

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

# GAM — simple model -------------------------------------------------------------------------------

library(mgcv)

m_gam_simple <- gam(
  log_SalePrice ~ s(Overall.Qual, k = 3) + s(Gr.Liv.Area) + s(House.Age) + Tot.Bath,
  data   = ames_tr,
  method = "GCV.Cp"
)
summary(m_gam_simple)

m_gam <- gen_additive_mod(select_features = FALSE) %>% set_engine("mgcv") %>% set_mode("regression")

m_gam_simple <- workflow() %>%
  add_model(m_gam, formula = log_SalePrice ~ s(Overall.Qual, k = 3) + s(Gr.Liv.Area) + s(House.Age) + s(Tot.Bath)) %>%
  add_formula(log_SalePrice ~ Overall.Qual + Gr.Liv.Area + House.Age + Tot.Bath) %>%
  fit(data = ames_tr)

tidy(m_gam_simple)
augment(m_gam_simple, new_data = ames_val) %>% exp_mae(truth = log_SalePrice, estimate = .pred)

# GAM — extended model with automatic variable selection -------------------------------------------

smooth_vars <- c("Gr.Liv.Area", "House.Age", "Lot.Area", "Total.Bsmt.SF", "Bsmt.Unf.SF")

# All predictors (adjust as needed)
all_vars <- ames_tr %>% select(-c(log_SalePrice, SalePrice)) %>% names()

# Build the mgcv formula with s() for smooth terms
mgcv_terms <- ifelse(all_vars %in% smooth_vars, paste0("s(", all_vars, ")"), all_vars)
mgcv_formula <- as.formula(paste("log_SalePrice ~", paste(mgcv_terms, collapse = " + ")))

# Build the plain formula for add_formula()
plain_formula <- as.formula(paste("log_SalePrice ~", paste(all_vars, collapse = " + ")))

m_gam_full <- workflow() %>%
  add_model(m_gam,
    formula = mgcv_formula
  ) %>%
  add_formula(plain_formula) %>%
  fit(data = ames_tr)

tidy(m_gam_full)
augment(m_gam_full, new_data = ames_val) %>% exp_mae(truth = log_SalePrice, estimate = .pred)

# MARS — additive model (degree = 1, no interactions) ---------------------------------------------

library(earth)

m_mars1 <- earth(log_SalePrice ~ . - SalePrice, data = ames_tr, degree = 1, )
summary(m_mars1)
print(m_mars1)

# Variable importance: the GCV contribution of each predictor across all basis functions
# that include it. Higher values indicate greater importance.
evimp(m_mars1)

par(mfrow = c(1, 1))
plot(evimp(m_mars1), cex.axis = 0.7)

# MARS — model with two-way interactions (degree = 2) ---------------------------------------------

m_mars2 <- earth(log_SalePrice ~ . - SalePrice, data = ames_tr, degree = 2)
summary(m_mars2)

evimp(m_mars2)


# MARS — tuning nprune on the validation set -------------------------------------------------------

wf_mars <- workflow() %>%
  add_recipe(base_recipe) %>%
  add_model(mars(prod_degree = tune(), num_terms = tune()) %>% set_engine("earth") %>% set_mode("regression"))

cv_mars <- tune_grid(
  wf_mars,
  resamples = val_resample,
  grid      = expand_grid(prod_degree = c(1, 2, 3), num_terms = c(10, 50, 100)),
  metrics   = my_metrics
)

collect_metrics(cv_mars)

autoplot(cv_mars, metric = "exp_mae") + theme_bw()
show_best(cv_mars, metric = "exp_mae")

best_cv_mars <- select_best(cv_mars, metric = "exp_mae")
best_cv_mars <- finalize_workflow(wf_mars, best_cv_mars) %>% fit(data = ames_tr)

# Final comparison on the test set -----------------------------------------------------------------------------------------

fitted_models <- list(
  Simple        = m_simple,
  Full          = m_full,
  GAM_simple   = m_gam_simple,
  GAM_full   = m_gam_full,
  MARS = best_cv_mars
)

results <- imap_dfr(fitted_models, function(model, name) {
  augment(model, new_data = ames_te) %>%
    exp_mae(truth = log_SalePrice, estimate = .pred) %>%
    transmute(model = name, mae = .estimate)
})

results %>% arrange(mae)
