## ----r------------------------------------------------------------------------
library(ISLR)
library(tidyverse)
data(Hitters)
Hitters <- na.omit(Hitters)
Hitters <- mutate(Hitters, Salary = log10(Salary))


## ----r------------------------------------------------------------------------
library(leaps)
fit <- regsubsets(Salary ~ ., data = Hitters, method = "exhaustive", nbest = 40, nvmax = 20)
sum1 <- summary(fit)
plot(rowSums(sum1$which), sum1$bic)


## ----r------------------------------------------------------------------------
library(tidymodels)
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))
fit <- regsubsets(Sale_Price ~ ., data = ames, method = "seqrep", nbest = 10, nvmax = 300)
sum1 <- summary(fit)
plot(rowSums(sum1$which), sum1$bic)
