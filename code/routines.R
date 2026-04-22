# The official version of this function is bugged - fixed with an inefficient (!) tweak;
# Hopefully the original function coef.regsubsets will be fixed soon.
coef.regsubsets <- function(object, id, data){
  form <- as.formula(object[["call"]][[2]])
  s <- summary(object)
  y <- model.response(model.frame(form, data))
  X <- model.matrix(form, data)
  xvars <- names(which(s$which[id, ]))
  Xvars <- X[, xvars]
  beta_hat <- c(solve(crossprod(Xvars), crossprod(Xvars, y)))
  names(beta_hat) <- xvars
  beta_hat
}

# Coding time. Regsubsets does not have a "predict" method, we need to do it ourselves
predict.regsubsets <- function(object, data, newdata, id, ...) {
  form <- as.formula(object[["call"]][[2]])
  
  # Compute the design matrix
  X <- model.matrix(form, newdata)
  # Identify the correct beta coefficients
  beta_hat <- coef(object, id = id, data)
  xvars <- names(beta_hat)
  
  # Making the predictions
  pred_mat <- X[, xvars] %*% beta_hat
  
  # Housekeeping
  pred <- as.numeric(pred_mat)
  names(pred) <- rownames(X)
  pred
}

# ---- Generic methods ------------------------------------------

library(yardstick)

# --- 1. The vector-level function (the actual computation) ---
exp_mae_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  # Check inputs
  check_numeric_metric(truth, estimate, case_weights = NULL)
  
  if (na_rm) {
    result <- stats::complete.cases(truth, estimate)
    truth    <- truth[result]
    estimate <- estimate[result]
  }
  
  mean(abs(exp(truth) - exp(estimate)))
}

# --- 2. The generic + register direction ---
exp_mae <- function(data, ...) UseMethod("exp_mae")
exp_mae <- new_numeric_metric(exp_mae, direction = "minimize")

# --- 3. The data.frame method ---
exp_mae.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  numeric_metric_summarizer(
    name     = "exp_mae",        # <-- was metric_nm in old API
    fn       = exp_mae_vec,      # <-- was metric_fn in old API
    data     = data,
    truth    = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm    = na_rm
  )
}