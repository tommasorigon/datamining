## ----r------------------------------------------------------------------------
library(plotly)
p <- plot_ly(z = volcano, type = "surface")
p


## ----r------------------------------------------------------------------------
library(ISLR)
library(tidyverse)
data(Hitters)
Hitters <- na.omit(Hitters)
Hitters <- mutate(Hitters, Salary = log10(Salary), Years = log(Years))


## ----r------------------------------------------------------------------------
library(leaps)
fit <- regsubsets(Salary ~ ., data = Hitters, method = "exhaustive", nbest = 1, nvmax = 20)
sum1 <- summary(fit)

plot(rowSums(sum1$which), sum1$cp)

fit <- regsubsets(Salary ~ ., data = Hitters, method = "backward", nbest = 1, nvmax = 20)
sum1 <- summary(fit)
# plot(rowSums(sum1$which), sum1$cp)
