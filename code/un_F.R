#| message: false
rm(list = ls())
library(ggplot2)
library(ggthemes)
library(sm)

data(trawl)
dplyr::glimpse(trawl)



# Data splitting
set.seed(1234)
id_train <- sample(1:nrow(trawl), size = floor(0.5 * nrow(trawl)), replace = FALSE)
id_test <- setdiff(1:nrow(trawl), id_train)
trawl_train <- trawl[id_train, ]
trawl_test <- trawl[id_test, ]
