# ----------------------------------------
# Title: LAB 4 (Ames Housing, regression)
#        Rewritten with tidymodels
# ----------------------------------------

rm(list = ls())
library(tidyverse)
library(tidymodels)

# ── 1. Data ───────────────────────────────────────────────────────────────────
# log_SalePrice is the model outcome; SalePrice is kept for final evaluation
ames <- read_csv("../data/ames.csv") |>
  mutate(log_SalePrice = log(SalePrice))

# ── 2. Three-way split: 50 % train / 25 % validation / 25 % test ─────────────
set.seed(123)
ames_split <- initial_validation_split(ames, prop = c(0.5, 0.25))

ames_train      <- training(ames_split)
ames_validation <- validation(ames_split)
ames_test       <- testing(ames_split)

glimpse(ames_train)

# ── 3. EDA plots ──────────────────────────────────────────────────────────────
numeric_vars <- c("Gr.Liv.Area", "Total.Bsmt.SF", 
                  "Porch.SF", "Tot.Bath", "House.Age")
factor_vars  <- c("Overall.Qual", "Bsmt.Qual", "Exter.Qual",
                  "Kitchen.Qual", "MS.Zoning", "Roof.Style", "Neighborhood")

ames_train |>
  select(SalePrice, all_of(numeric_vars)) |>
  pivot_longer(-SalePrice) |>
  ggplot(aes(value, SalePrice)) +
  geom_point(alpha = 0.4, size = 0.8) +
  facet_wrap(~name, scales = "free_x") +
  labs(title = "SalePrice vs numeric predictors") + 
  theme_light()

ames_train |>
  select(SalePrice, all_of(factor_vars)) |>
  mutate(across(all_of(factor_vars), as.factor)) |>
  pivot_longer(-SalePrice) |>
  ggplot(aes(value, SalePrice)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "SalePrice vs categorical predictors") + 
  theme_light()

# ── 4. Recipes ────────────────────────────────────────────────────────────────
# The outcome is log_SalePrice; SalePrice is dropped from the predictor set.
base_recipe <- recipe(log_SalePrice ~ ., data = ames_train) |>
  step_rm(SalePrice) |>
  step_novel(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

# Shrinkage methods additionally require centring and scaling
shrinkage_recipe <- base_recipe |>
  step_normalize(all_numeric_predictors())

# ── 5. Metrics (original dollar scale) ───────────────────────────────────────
my_metrics <- metric_set(mae, rmse)

# augment() adds .pred (log scale) to the data; we exponentiate before metrics
eval_on_validation <- function(wf) {
  augment(wf, ames_validation) |>
    mutate(.pred = exp(.pred)) |>
    my_metrics(truth = SalePrice, estimate = .pred)
}

# ── 6. Benchmark: median prediction ──────────────────────────────────────────
ames_validation |>
  mutate(.pred = median(ames_train$SalePrice)) |>
  my_metrics(truth = SalePrice, estimate = .pred)

# ── 7. Simple OLS ────────────────────────────────────────────────────────────
wf_simple <- workflow() |>
  add_recipe(
    recipe(log_SalePrice ~ Overall.Qual + Gr.Liv.Area + House.Age + Tot.Bath,
           data = ames_train)
  ) |>
  add_model(linear_reg()) |>
  fit(ames_train)

eval_on_validation(wf_simple)

# ── 8. Full OLS ───────────────────────────────────────────────────────────────
wf_full <- workflow() |>
  add_recipe(base_recipe) |>
  add_model(linear_reg()) |>
  fit(ames_train)

eval_on_validation(wf_full)

# ── 9. PCR ────────────────────────────────────────────────────────────────────
val_rset <- validation_set(ames_split)

wf_pcr <- workflow() |>
  add_recipe(shrinkage_recipe |> step_pca(all_numeric_predictors(), num_comp = tune())) |>
  add_model(linear_reg())

pcr_tune <- tune_grid(
  wf_pcr,
  resamples = val_rset,
  grid      = tibble(num_comp = 1:200),
  metrics   = my_metrics
)

autoplot(pcr_tune, metric = "mae") + theme_light()

wf_pcr_final <- finalize_workflow(wf_pcr, select_best(pcr_tune, metric = "rmse")) |>
  fit(ames_train)

eval_on_validation(wf_pcr_final)

# ── 10. Ridge ─────────────────────────────────────────────────────────────────
wf_ridge <- workflow() |>
  add_recipe(shrinkage_recipe) |>
  add_model(linear_reg(penalty = tune(), mixture = 0) |> set_engine("glmnet"))

ridge_tune <- tune_grid(
  wf_ridge,
  resamples = val_rset,
  grid      = tibble(penalty = exp(seq(-6, 6, length.out = 100))),
  metrics   = metric_set(rmse)
)

autoplot(ridge_tune) + theme_light()

wf_ridge_final <- finalize_workflow(wf_ridge, select_best(ridge_tune, metric = "rmse")) |>
  fit(ames_train)

eval_on_validation(wf_ridge_final)

# ── 11. Lasso ─────────────────────────────────────────────────────────────────
wf_lasso <- workflow() |>
  add_recipe(shrinkage_recipe) |>
  add_model(linear_reg(penalty = tune(), mixture = 1) |> set_engine("glmnet"))

lasso_tune <- tune_grid(
  wf_lasso,
  resamples = val_rset,
  grid      = tibble(penalty = exp(seq(-10, 0, length.out = 100))),
  metrics   = metric_set(rmse)
)

autoplot(lasso_tune) + theme_light()

wf_lasso_final <- finalize_workflow(wf_lasso, select_best(lasso_tune, metric = "rmse")) |>
  fit(ames_train)

eval_on_validation(wf_lasso_final)

# ── 12. Elastic Net ───────────────────────────────────────────────────────────
wf_en <- workflow() |>
  add_recipe(shrinkage_recipe) |>
  add_model(linear_reg(penalty = tune(), mixture = 0.5) |> set_engine("glmnet"))

en_tune <- tune_grid(
  wf_en,
  resamples = val_rset,
  grid      = tibble(penalty = exp(seq(-10, 0, length.out = 100))),
  metrics   = metric_set(rmse)
)

wf_en_final <- finalize_workflow(wf_en, select_best(en_tune, metric = "rmse")) |>
  fit(ames_train)

eval_on_validation(wf_en_final)

# ── 13. Random Forest ─────────────────────────────────────────────────────────
wf_rf <- workflow() |>
  add_recipe(base_recipe) |>
  add_model(rand_forest(trees = 2000, mtry = 10) |>
              set_engine("ranger") |>
              set_mode("regression")) |>
  fit(ames_train)

eval_on_validation(wf_rf)

# ── 14. Final comparison on the test set ──────────────────────────────────────
fitted_models <- list(
  Median        = NULL,
  Simple        = wf_simple,
  Full          = wf_full,
  PCR           = wf_pcr_final,
  Ridge         = wf_ridge_final,
  Lasso         = wf_lasso_final,
  `Elastic Net` = wf_en_final,
  `Rand Forest` = wf_rf
)

results <- imap_dfr(fitted_models, function(wf, name) {
  preds <- if (is.null(wf)) {
    rep(median(ames_train$SalePrice), nrow(ames_test))
  } else {
    exp(predict(wf, ames_test)$.pred)
  }
  tibble(model = name, truth = ames_test$SalePrice, estimate = preds)
})

# Summary table
results |>
  group_by(model) |>
  my_metrics(truth = truth, estimate = estimate) |>
  pivot_wider(names_from = .metric, values_from = .estimate) |>
  arrange(rmse)

# Error box-plot
results |>
  mutate(error = estimate - truth) |>
  ggplot(aes(reorder(model, abs(error), median), error)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = NULL, y = "Prediction error ($)", title = "Test-set errors by model")