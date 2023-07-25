rm(list = ls())
X <- matrix(runif(100 * 5, 0, 1), 100, 5)
# X <- cbind(X, X[, 1] + rnorm(10, sd = 1e-6))

XtX <- crossprod(X)
beta <- rnorm(5)
y <- rnorm(100, X %*% beta, )

kappa(X, exact = TRUE)^2
kappa(XtX, exact = TRUE)

factorizationQR = function(X){
  X <- as.matrix(X)
  p <- ncol(X)
  n <- nrow(X)
  Q <- matrix(0, n, p)
  R <- matrix(0, p, p)
  for (j in 1:p){
    Zj = X[,j]
    if (j > 1) {
      for (k in 1:(j-1)){
        R[k,j] = crossprod(Q[,k], X[,j])
        Zj = Zj - R[k,j] * Q[,k]
      }
    }
    R[j,j] <- sqrt(crossprod(Zj))
    Q[,j] <- Zj / R[j,j]
  }
  return(list(Q=Q, R=R))
}

ols_manual <- function(X, y) {
  solve(t(X) %*% X) %*% t(X) %*% y
}

ols_solve <- function(X, y) {
  solve(crossprod(X), crossprod(X, y))
}

ols_qr <- function(X, y) {
  qr_obj <- qr(X)
  qr.solve(qr_obj, y)
  # Q <- qr.Q(qr_obj)
  # R <- qr.R(qr_obj)
  # Qty <- crossprod(Q, y)
  # betahat <- backsolve(R, Qty)
  # betahat
}

ols_chol <- function(X, y) {
  XtX <- crossprod(X)
  Xty <- crossprod(X, y)
  L <- chol(XtX)
  betahat <- backsolve(L, forwardsolve(t(L), Xty))
  betahat
}

ols_eig <- function(X, y){
  XtX <- crossprod(X)
  Xty <- crossprod(X, y)
  eig <- eigen(XtX, symmetric = TRUE)
  crossprod(t(eig$vectors) / sqrt(eig$values)) %*% Xty
}

set.seed(1991)
n <- 50
p <- 5
b0 <- c(3, 1.5, 0, -1.5, -3)
beta <- rep(b0, each = p / 5)
X <- matrix(rnorm(n * (p), 0, sqrt(1 / n)), n, p)
y <- rnorm(n, X %*% beta, 1)

ols_manual(X, y)
ols_solve(X, y)
ols_qr(X, y)
ols_chol(X, y)
ols_eig(X, y)


library(microbenchmark)
micro_out <- microbenchmark(
  manual = ols_manual(X, y),
  solve = ols_solve(X, y),
  QR = ols_qr(X, y),
  Cholesky = ols_chol(X, y),
  eig = ols_chol(X, y)
)

kable(summary(micro_out))


solve(XtX)

qr_obj <- qr(X, LAPACK = FALSE)
chol2inv(qr_obj$qr)


X - qr.X(qr(X, LAPACK = TRUE))

beta
lm(y ~ X-1)

ols_manual(X, y)
ols_solve(X, y)
ols_qr(X, y)
ols_chol(X, y)
ols_eig(X, y)
