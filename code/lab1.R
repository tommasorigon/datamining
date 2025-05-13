#' ---
#' title: "LAB 1 (Computations for linear models)"
#' author: "Tommaso Rigon"
#' ---
#' 
#+ setup, include=FALSE
knitr::opts_chunk$set(collapse = TRUE)

#+ lab, include=TRUE, echo = TRUE, results = FALSE
rm(list = ls())

# The dataset can be downloaded here: https://tommasorigon.github.io/datamining/data/auto.txt
auto <- read.table("../data/auto.txt", header = TRUE)

# Select the variables we need
auto <- subset(auto, select = c(city.distance, engine.size, n.cylinders, curb.weight, fuel))

# Create a new variable (cylinders)
auto$cylinders2 <- factor(auto$n.cylinders == 2)

# This is the final model we obtained in Unit A, after a long modelling process
m_final <- lm(log(city.distance) ~ I(log(engine.size)) + I(log(curb.weight)) + fuel + cylinders2, data = auto)

# Summary of the results
summary(m_final)

# Design matrix obtained from the model
X <- model.matrix(log(city.distance) ~ I(log(engine.size)) + I(log(curb.weight)) + fuel + cylinders2, data = auto)
y <- log(auto$city.distance)

# Optional. I remove the column names to improve the HTML readibility
colnames(X) <- NULL

dim(X)

# Least squares, the naive way.
# This provides the correct numbers in simple examples, but it is inefficient AND numerically inaccurate
solve(t(X) %*% X) %*% t(X) %*% y

# Sufficients statistics for this model
XtX <- crossprod(X)
Xty <- crossprod(X, y)
round(XtX, digits = 1)
round(Xty, digits = 2)

# The next algorithm make use of normal equations, but it does not know that XtX is positive definite
ols_solve <- function(X, y) {
  XtX <- crossprod(X)
  Xty <- crossprod(X, y)
  solve(XtX, Xty)
}

ols_solve(X, y)

library(microbenchmark) # Needs to be installed

# Measure the speed of execution
times <- microbenchmark(
  matrix_inversion = solve(t(X) %*% X) %*% t(X) %*% y,
  linear_system = ols_solve(X, y), times = 1000
)

# Summary of the timings
times
boxplot(times)

R <- chol(XtX)
round(R, 3)

# Confirm that this is the appropriate Cholesky decomposition
round(XtX, 3)
round(t(R) %*% R, 3)

# Ordinary least squares with Cholesky
ols_chol <- function(X, y) {
  XtX <- crossprod(X)
  Xty <- crossprod(X, y)
  R <- chol(XtX)
  beta_hat <- backsolve(R, forwardsolve(t(R), Xty))
  beta_hat
}

ols_chol(X, y)

# Measure the speed of execution
times <- microbenchmark(
  matrix_inversion = solve(t(X) %*% X) %*% t(X) %*% y,
  linear_system = ols_solve(X, y),
  cholesky = ols_chol(X, y),
  times = 1000
)

# Summary of the timings
times
boxplot(times)

# However, the Cholesky decomposition is giving us much more.
# To get the variance of the estimates we need to compute the inverse of XtX

# Traditional inverse (not ideal)
solve(XtX)
# Cholesky inverse
chol2inv(R)

microbenchmark(matrix_inversion = solve(XtX), cholesky = chol2inv(R))

# This specific linear regression model is fairly well-conditioned
# Any method (even the naive ones) are going to work just fine
kappa(t(X) %*% X, exact = TRUE)

# Note that this coincide with:
kappa(X, exact = TRUE)^2

# Re-implementation via Gram-Schmidt
factorizationQR <- function(X) {
  p <- ncol(X)
  n <- nrow(X)
  Q <- matrix(0, n, p)
  R <- matrix(0, p, p)
  for (j in 1:p) {
    Zj <- X[, j]
    if (j > 1) {
      for (k in 1:(j - 1)) {
        R[k, j] <- crossprod(Q[, k], X[, j])
        Zj <- Zj - R[k, j] * Q[, k]
      }
    }
    R[j, j] <- sqrt(crossprod(Zj))
    Q[, j] <- Zj / R[j, j]
  }
  return(list(Q = Q, R = R))
}

# Let us compute the QR factorization
QR <- factorizationQR(X)

# This is an orthogonal matrix
round(crossprod(QR$Q), 3)

# This coincide with the Cholesky
round(QR$R, 3)
round(R, 3)

ols_QR <- function(X, y) {
  qr_obj <- factorizationQR(X)
  Q <- qr_obj$Q
  R <- qr_obj$R
  Qty <- crossprod(Q, y)
  beta_hat <- backsolve(R, Qty)
  beta_hat
}
ols_QR(X, y)

# Be careful, here pivoting is performed
# This means the QR might be different with that of factorizationQR
QR_obj <- qr(X)

ols_QR <- function(X, y) {
  qr_obj <- qr(X)
  qr.coef(qr_obj, y)
}

ols_QR(X, y)

# Measure the speed of execution
times <- microbenchmark(
  matrix_inversion = solve(t(X) %*% X) %*% t(X) %*% y,
  linear_system = ols_solve(X, y),
  cholesky = ols_chol(X, y),
  QR = ols_QR(X, y),
  times = 1000
)

# Summary of the timings
times
boxplot(times)

# Estimated coefficients
qr.coef(QR_obj, y)

# Predicted values
head(predict(m_final))
head(qr.fitted(QR_obj, y))

# Residuals
head(residuals(m_final))
head(qr.resid(QR_obj, y))

# Influence points
head(influence(m_final)$hat)
head(rowSums(qr.Q(QR_obj)^2))

# Inverse of XtX
solve(XtX)
chol2inv(qr.R(QR_obj))

a <- 1e-7
X <- cbind(c(1, a, 0), c(1, 0, a))
y <- c(1, 0, -1)

manual <- rbind((1 - a) / (-2 * a^2 - a^4) + (-1 - a^2) / (-2 * a^2 - a^4), 1 / (-2 * a^2 - a^4) + ((1 - a) * (-1 - a^2)) / (-2 * a^2 - a^4))

print(ols_solve(X, y), digits = 12)
print(ols_chol(X, y), digits = 12)
print(ols_QR(X, y), digits = 12)
print(manual, digits = 12)

# Additional information

# Source of the data: https://dati.mit.gov.it/hfs/patenti_Lombardia.csv
# Documentation: http://dati.mit.gov.it/catalog/dataset/patenti

# Author:	Direzione generale per la motorizzazione - Div7 - Centro elaborazione dati motorizzazione
# Last update:	21 December 2022, 17:16 (UTC+01:00)
# Created:	20 February 2022, 18:21 (UTC+01:00)
# Temporal extension (end)	31 December 2019

# DATASET DOWNLOAD (~200MB)
# The dataset "drives2.csv" can be downloaded here: https://drive.google.com/file/d/17Fiz1MIFDNNBzs6T9EKWVLhHB1IMZYwB/view?usp=share_link

library(readr)
drivers <- read_csv("../data/drivers2.csv", n_max = 10000)

# Design matrix obtained from the model
X <- model.matrix(hazard ~ poly(age, 10) + poly(experience, 10) + habilit + gender + city, data = drivers)
y <- drivers$hazard

dim(X)

# Measure the speed of execution
times <- microbenchmark(
  matrix_inversion = solve(t(X) %*% X) %*% t(X) %*% y,
  linear_system = ols_solve(X, y),
  cholesky = ols_chol(X, y),
  QR = ols_QR(X, y),
  times = 50
)

# Summary of the timings
times

library(biglm)

drivers_chunk1 <- read_csv("../data/drivers2.csv",
  n_max = 100000, skip = 0, col_names = TRUE
)
name_cols <- colnames(drivers_chunk1)

m_big <- biglm(hazard ~ poly(age, 10) + poly(experience, 10) + habilit + gender + city,
  data = drivers_chunk1
)
summary(m_big)

rm(drivers_chunk1)
gc() # Free unused R memory

drivers_chunk2 <- read_csv("../data/drivers2.csv",
  n_max = 100000, skip = 100000, col_names = FALSE
)
colnames(drivers_chunk2) <- name_cols

m_big <- update(m_big, drivers_chunk2)
summary(m_big)

rm(drivers_chunk2)
gc() # Free unused R memory

drivers_chunk3 <- read_csv("../data/drivers2.csv",
  n_max = 100000, skip = 200000, col_names = FALSE
)
colnames(drivers_chunk3) <- name_cols

m_big <- update(m_big, drivers_chunk3)
summary(m_big)

print(object.size(m_big), units = "KB")

