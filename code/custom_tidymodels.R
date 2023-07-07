library(tidymodels)

best_subset <- function(formula, data, nbest = 1, nvmax = 8) {
  leaps::regsubsets(
    formula, data, nbest = nbest, nvmax = nvmax,
    method = "exhaustive")
}

best_subset(formula = mpg ~ ., data = mtcars)
leaps::regsubsets(x = mpg ~ ., data = mtcars)


set_model_engine("linear_reg", "regression", eng = "best_subset")
set_dependency("linear_reg", eng = "best_subset", pkg = "leaps")

set_model_arg(
  model = "linear_reg",
  eng = "best_subset",
  parsnip = "n_param",
  original = "nvmax",
  func = list(pkg = "foo", fun = "bar"),
  has_submodel = FALSE
)

set_model_arg(
  model = "linear_reg",
  eng = "best_subset",
  parsnip = "nbest",
  original = "nbest",
  func = list(pkg = "foo", fun = "bar"),
  has_submodel = FALSE
)

show_model_info("linear_reg")

set_fit(
  model = "linear_reg",
  eng = "best_subset",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(fun = "best_subset"),
    defaults = list()
  )
)

set_encoding(
  model = "linear_reg",
  eng = "best_subset",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "linear_reg",
  eng = "best_subset",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = expr(object$fit),
        newdata = expr(new_data),
        type = "response"
      )
  )
)

# testing:
linear_reg(penalty = 2) %>%
  set_engine("glmnet") %>%
  fit(mpg ~ ., data = mtcars)

