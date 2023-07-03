#| message: false
rm(list = ls())
library(ISLR)
library(tidyverse)
data(Hitters)
Hitters <- na.omit(Hitters)
Hitters <- mutate(Hitters, logYears = log10(Years), logSalary = log10(Salary)) %>% select(-c(Salary, Years))
glimpse(Hitters)



library(leaps)
fit <- regsubsets(logSalary ~ ., data = Hitters, method = "exhaustive", nbest = 1, nvmax = 20)
sum1 <- summary(fit)

plot(rowSums(sum1$which), sum1$cp)

fit <- regsubsets(logSalary ~ ., data = Hitters, method = "backward", nbest = 1, nvmax = 20)
sum1 <- summary(fit)
# plot(rowSums(sum1$which), sum1$cp)
