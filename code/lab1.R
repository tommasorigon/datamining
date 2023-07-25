#' ---
#' title: "Lab 1 - Data mining"
#' author: "Tommaso Rigon"
#' ---

rm(list = ls())

# Informazioni addizionali

# Source of the data: https://dati.mit.gov.it/hfs/patenti_Lombardia.csv
# Documentation: http://dati.mit.gov.it/catalog/dataset/patenti

# Author:	Direzione generale per la motorizzazione - Div7 - Centro elaborazione dati motorizzazione
# Last update:	21 December 2022, 17:16 (UTC+01:00)
# Created:	20 febbraio 2022, 18:21 (UTC+01:00)
# Temporal extension (end)	31 December 2019

library(tidyverse)
library(lubridate)

# Use n_max = 1000 for most preliminary operations
drivers <- read_csv("../data/drivers.csv", col_types = "iiffffffcfccfd")

# Change the name of the columns
colnames(drivers) <- c("id", "birth", "town", "city", "region", "state", "gender", "category", "date", "habilit", "date_habilit", "expiration", "date_foreigners", "points")

# Change the format of the date
drivers <- drivers %>% mutate(date = ymd_hms(date), experience = 2019 - year(date), age = 2019 - birth) 

# Select patent B and other things
drivers <- drivers %>% filter(category == "B", is.na(state), !is.na(points)) %>% filter(experience > 0)

# Remove irrelevant columns
drivers <- drivers %>% select(-c(id, category, state, date_foreigners, expiration, date_habilit, birth, date, region))

drivers <- drivers %>% mutate(hazard = sqrt(-(points - 30)))

glimpse(drivers)
summary(drivers)


# Start simple, use a subset!
set.seed(12)
drivers_sub <- sample_n(drivers, 50000, replace = FALSE)

ggplot(data = drivers_sub, aes(x = experience, y = hazard)) + geom_point() +theme_bw()
ggplot(data = drivers_sub, aes(x = age, y = hazard)) + geom_point() +theme_bw()

drivers_sub <- na.omit(drivers_sub)
m <- lm(hazard ~ poly(age, 10) + habilit + gender + poly(experience, 10) + town, data = drivers_sub)
summary(m)


X <- model.matrix(hazard ~ poly(age, 10) + habilit + gender + poly(experience, 10) + town, data = drivers_sub)
XtX <- crossprod(X)
y <- drivers_sub$hazard

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

system.time(ols_manual(X, y))
system.time(ols_solve(X, y))
system.time(ols_qr(X, y))
system.time(ols_chol(X, y))
system.time(ols_eig(X, y))


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

library(QR)
Q <- QR(X, complete = TRUE)$Q
crossprod(Q)
