rm(list = ls())
library(tidyverse)
ames <- read_csv("../data/AmesHousing.csv")
ames

glimpse(ames)

table(ames$`MS Zoning`)

# From the documentation:
# "C (all)" = Commercial sales
# "I (all)" = Industrial sales
# "A (agr)" = Agricultural sales
# "FV" = Floating village sales
ames <- filter(ames, !(`MS Zoning` %in% c("C (all)", "I (all)", "A (agr)", "FV")))

table(ames$`Sale Condition`)

# Only normal sales
ames <- filter(ames, `Sale Condition` == "Normal")
# The variable can be dropped
ames <- select(ames, -c(`Sale Condition`, `Sale Type`))

ames <- select(ames, -c(Order, PID))

summary(ames$SalePrice)

par(mfrow = c(1, 2))
hist(ames$SalePrice, xlab = "Price", main = "Histogram of SalePrice")
hist(log(ames$SalePrice), xlab = "Price", main = "Histogram of the logarithm of SalePrice")

freq_missing <- apply(ames, 2, function(x) sum(is.na(x))) # Number of missing values
sort(freq_missing, decreasing = TRUE)
