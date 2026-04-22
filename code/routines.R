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