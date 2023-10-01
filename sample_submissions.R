rm(list = ls())

library(tidyverse)
library(tidymodels)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/University/Didattica/Data Mining/dataset/2023_2024")

taxi <- read_csv("training.csv")
taxi_final <- read_csv("test.csv")

taxi_final <- taxi_final %>% mutate(across(where(is_character), as_factor))
taxi <- taxi %>% mutate(across(where(is_character), as_factor))
taxi<- taxi %>% mutate(log_tip = log(tip_amount)) %>% select(-c(ID, tip_amount, tip_perc))

rec <- recipe(log_tip ~ pickup_hour  + pickup_month + pickup_week + pickup_doy + pickup_wday + length_time + pickup_BoroCode  + dropoff_BoroCode + pickup_longitude + pickup_latitude + dropoff_longitude + dropoff_latitude + vendor_id + passenger_count + trip_distance + fare_amount, data = taxi) %>% step_dummy(all_factor())

set.seed(123)
split <- initial_split(taxi)
taxi_train <- training(split)
taxi_validation <- testing(split)

set.seed(123)
CV_splits <- vfold_cv(taxi_train, v = 5)

control_settings <- control_grid(save_pred = TRUE, verbose = TRUE)


metric_list <- metric_set(rmse, rsq)

m_lasso <- linear_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet")
m_lasso_grid <- tibble(penalty = 10^seq(-7, -2, length.out = 15))

wf_lasso <- workflow() %>%
  add_model(m_lasso) %>%
  add_recipe(rec)

fit_lasso <- wf_lasso %>% tune_grid(CV_splits,
                                    grid = m_lasso_grid,
                                    metrics = metric_list,
                                    control = control_settings)

show_best(fit_lasso, metric = "rsq")
best_lasso <- select_best(fit_lasso, metric = "rsq")

autoplot(fit_lasso, metric = "rsq") + theme_bw()

# the last model
optimal_lasso <- linear_reg(penalty = 0.0000001, mixture = 1) %>% set_engine("glmnet")

wf_optimal_lasso <- workflow() %>%
  add_model(optimal_lasso) %>%
  add_recipe(rec)

fit_last_lasso <- wf_optimal_lasso %>% last_fit(split, metrics = metric_list)

collect_metrics(fit_last_lasso)

fit_lasso_out <- wf_optimal_lasso %>% fit(taxi)
out_lasso <- data.frame(ID = 1:nrow(taxi_final), tip_perc = c(exp(predict(fit_lasso_out, taxi_final)$.pred)) / taxi_final$fare_amount)

write.csv(out_lasso, "out_lasso.csv", row.names  = FALSE)

# RF --------------------------------------------------------------------------
m_rf_grid <- tibble(expand.grid(trees = c(100, 200, 300, 500, 1000, 2000), 
                                mtry = c(2, 3, 4, 5, 10, 20, 40)))

m_rf <- rand_forest(mtry = tune(), trees = tune()) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

wf_rf <- workflow() %>%
  add_model(m_rf) %>%
  add_recipe(rec)

fit_rf <- wf_rf %>% tune_grid(CV_splits,
                              grid = m_rf_grid,
                              metrics = metric_list,
                              control = control_settings
)
```

```{r}
autoplot(fit_rf, metric = "roc_auc") + theme_bw()
show_best(fit_rf, metric = "roc_auc")
best_rf <- select_best(fit_rf, metric = "roc_auc")
```

```{r}
best_pred_rf <- collect_predictions(fit_rf) %>%
  filter(.config == best_rf$.config) %>%
  mutate(.pred_class_adj = factor(.pred_spam > 1 / (1 + 0.4))) %>%
  mutate(.pred_class_adj = fct_recode(.pred_class_adj, spam = "TRUE", non_spam = "FALSE"))

Cost_Challenge(best_pred_rf$.pred_class_adj, best_pred_rf$response)
```

```{r}
# the last model
optimal_rf <- rand_forest(mtry = 3, trees = 2000) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

wf_optimal_rf <- workflow() %>%
  add_model(optimal_rf) %>%
  add_recipe(rec)

library(vip)
fit_last_rf <- wf_optimal_rf %>% last_fit(split, metrics = metric_list)
fit_last_rf %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 20) + theme_bw()
```

```{r}
collect_metrics(fit_last_logit)

tab <- collect_predictions(fit_last_rf) %>%
  mutate(.pred_class_adj = factor(.pred_spam > 1 / (1 + 0.25))) %>%
  mutate(.pred_class_adj = fct_recode(.pred_class_adj, spam = "TRUE", non_spam = "FALSE"))
Cost_Challenge(tab$.pred_class_adj, tab$response)
```

```{r}
m_boost_grid <- tibble(expand.grid(trees = (1:20)*100, 
                                   mtry = c(5, 10), 
                                   learn_rate = seq(0.005, 0.05, length = 5)))

m_boost <- boost_tree(
  mode = "classification",
  engine = "xgboost",
  mtry = tune(),
  trees = tune(),
  learn_rate = tune(),
  sample_size = 0.8
)

wf_boost <- workflow() %>%
  add_model(m_boost) %>%
  add_recipe(rec)

fit_boost <- wf_boost %>% tune_grid(CV_splits,
                                    grid = m_boost_grid,
                                    metrics = metric_list,
                                    control = control_settings
)
```

```{r}
show_best(fit_boost, metric = "roc_auc")
autoplot(fit_boost, metric = "roc_auc") + theme_bw()
best_boost <- select_best(fit_boost, metric = "roc_auc")
```

```{r}
best_pred_boost <- collect_predictions(fit_boost) %>%
  filter(.config == best_boost$.config) %>%
  mutate(.pred_class_adj = factor(.pred_spam > 1 / (1 + 0.3))) %>%
  mutate(.pred_class_adj = fct_recode(.pred_class_adj, spam = "TRUE", non_spam = "FALSE"))

table(best_pred_boost$.pred_class_adj, best_pred_boost$response)
Cost_Challenge(best_pred_boost$.pred_class_adj, best_pred_boost$response)
```

```{r}
# the last model
optimal_boost <- boost_tree(
  mode = "classification",
  engine = "xgboost",
  mtry = 5,
  trees = 900,
  learn_rate = 0.0275,
  sample_size = 0.8
)

wf_optimal_boost <- workflow() %>%
  add_model(optimal_boost) %>%
  add_recipe(rec)

fit_last_boost <- wf_optimal_boost %>% last_fit(split, metrics = metric_list)

collect_metrics(fit_last_boost)

tab <- collect_predictions(fit_last_boost) %>%
  mutate(.pred_class_adj = factor(.pred_spam > 1 / (1 + 0.3))) %>%
  mutate(.pred_class_adj = fct_recode(.pred_class_adj, spam = "TRUE", non_spam = "FALSE"))

Cost_Challenge(tab$.pred_class_adj, tab$response)
```

```{r}
m_boost2_grid <- tibble(expand.grid(trees = (1:20)*100, 
                                    mtry = c(5, 10), 
                                    learn_rate = seq(0.005, 0.05, length = 5)))

m_boost2 <- boost_tree(
  mode = "classification",
  engine = "lightgbm",
  mtry = tune(),
  trees = tune(),
  learn_rate = tune(),
  sample_size = 0.8
)

wf_boost2 <- workflow() %>%
  add_model(m_boost2) %>%
  add_recipe(rec)

fit_boost2 <- wf_boost2 %>% tune_grid(CV_splits,
                                      grid = m_boost2_grid,
                                      metrics = metric_list,
                                      control = control_settings
)
```

```{r}
show_best(fit_boost2, metric = "roc_auc")
autoplot(fit_boost2, metric = "roc_auc") + theme_bw()
best_boost2 <- select_best(fit_boost2, metric = "roc_auc")
```

```{r}
best_pred_boost2 <- collect_predictions(fit_boost2) %>%
  filter(.config == best_boost2$.config) %>%
  mutate(.pred_class_adj = factor(.pred_spam > 1 / (1 + 0.3))) %>%
  mutate(.pred_class_adj = fct_recode(.pred_class_adj, spam = "TRUE", non_spam = "FALSE"))

table(best_pred_boost2$.pred_class_adj, best_pred_boost2$response)
Cost_Challenge(best_pred_boost2$.pred_class_adj, best_pred_boost2$response)
```

```{r}
# the last model
optimal_boost2 <- boost_tree(
  mode = "classification",
  engine = "lightgbm",
  mtry = 5,
  trees = 300,
  learn_rate = 0.0275,
  sample_size = 0.8
)

wf_optimal_boost2 <- workflow() %>%
  add_model(optimal_boost2) %>%
  add_recipe(rec)

fit_last_boost2 <- wf_optimal_boost2 %>% last_fit(split, metrics = metric_list)

collect_metrics(fit_last_boost2)

tab <- collect_predictions(fit_last_boost2) %>%
  mutate(.pred_class_adj = factor(.pred_spam > 1 / (1 + 0.2))) %>%
  mutate(.pred_class_adj = fct_recode(.pred_class_adj, spam = "TRUE", non_spam = "FALSE"))

Cost_Challenge(tab$.pred_class_adj, tab$response)
```

## Final stuff

```{r}
collect_metrics(fit_last_logit)
collect_metrics(fit_last_rf)
collect_metrics(fit_last_boost)
collect_metrics(fit_last_boost2)
```

```{r}
fit_out <- wf_optimal_boost2 %>% fit(spam)

out <- predict(fit_out, spam_pred, type = "prob") %>% mutate(.pred_class_adj = factor(.pred_spam > 1 / (1 + 0.2))) %>% mutate(.pred_class_adj = fct_recode(.pred_class_adj, "2" = "TRUE", "1" = "FALSE")) %>% select(.pred_class_adj)

write_csv(out, "boost2.txt", col_names  = FALSE)
```
