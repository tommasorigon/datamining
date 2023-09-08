#| message: false
rm(list = ls())
library(ISLR)
library(tidyverse)
data(Hitters)
Hitters <- na.omit(Hitters)
glimpse(Hitters)



# Hitters <- mutate(Hitters,
#   TotalScore = (RBI + Assists + Walks - Errors) / AtBat,
#   logAHits = log1p(CHits / Years),
#   logAAtBat = log1p(CAtBat / Years),
#   logARuns = log1p(CRuns / Years),
#   logARBI = log1p(CRBI / Years),
#   logAWalks = log1p(CWalks / Years),
#   logAHmRun = log1p(CHmRun / Years),
#   RatioHits = Hits / CHits,
#   RatioAtBat = AtBat / CAtBat,
#   RatioRuns = Runs / CRuns,
#   RatioRBI = RBI / CRBI,
#   RatioWalks = Walks / CWalks,
#   RatioHmRun = HmRun / (CHmRun + 1),
#   logYears = log(Years),
#   logSalary = log(Salary)
# ) %>% select(-c(Salary, Years))
#
# Data splitting
set.seed(123)
id_train <- sample(1:nrow(Hitters), size = floor(0.75 * nrow(Hitters)), replace = FALSE)
id_test <- setdiff(1:nrow(Hitters), id_train)
Hitters_train <- Hitters[id_train, ]
Hitters_test <- Hitters[id_test, ]
