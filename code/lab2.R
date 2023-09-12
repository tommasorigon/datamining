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
# The variables can be dropped
ames <- select(ames, -c(`Sale Condition`, `Sale Type`))

# Drop the variables
ames <- select(ames, -c(Order, PID))

summary(ames$SalePrice)

par(mfrow = c(1, 2))
hist(ames$SalePrice, xlab = "Price", main = "SalePrice")
hist(log(ames$SalePrice), xlab = "Price", main = "Logarithm of SalePrice")

# Compute the frequency of the missing values for each variable
freq_missing <- apply(ames, 2, function(x) sum(is.na(x))) # Number of missing values
sort(freq_missing, decreasing = TRUE)

table(ames$Alley, useNA = "always")
ames$Alley[is.na(ames$Alley)] <- "No alley access"

ames$`Bsmt Exposure`[is.na(ames$`Bsmt Exposure`)] <- "No basement"
ames$`Bsmt Cond`[is.na(ames$`Bsmt Cond`)] <- "No basement"
ames$`BsmtFin Type 1`[is.na(ames$`BsmtFin Type 1`)] <- "No basement"
ames$`BsmtFin Type 2`[is.na(ames$`BsmtFin Type 2`)] <- "No basement"
ames$`Bsmt Qual`[is.na(ames$`Bsmt Qual`)] <- "No basement"

ames$`Bsmt Full Bath`[is.na(ames$`Bsmt Full Bath`)] <- 0
ames$`Bsmt Half Bath`[is.na(ames$`Bsmt Half Bath`)] <- 0

table(ames$Electrical, useNA = "always")
ames$Electrical[is.na(ames$Electrical)] <- "SBrkr"

table(ames$Fence, useNA = "always")
ames$`Fence`[is.na(ames$`Fence`)] <- "No fence"

table(ames$`Fireplace Qu`, useNA = "always")
ames$`Fireplace Qu`[is.na(ames$`Fireplace Qu`)] <- "No fireplace"

ames$`Garage Cond`[is.na(ames$`Garage Cond`)] <- "No garage"
ames$`Garage Finish`[is.na(ames$`Garage Finish`)] <- "No garage"
ames$`Garage Qual`[is.na(ames$`Garage Qual`)] <- "No garage"
ames$`Garage Type`[is.na(ames$`Garage Type`)] <- "No garage"

ames <- select(ames, -c(`Garage Yr Blt`))

ames$`Lot Frontage`[is.na(ames$`Lot Frontage`)] <- 0

ames$`Mas Vnr Type`[is.na(ames$`Mas Vnr Type`)] <- "None"
ames$`Mas Vnr Area`[is.na(ames$`Mas Vnr Area`)] <- 0

table(ames$`Misc Feature`, useNA = "always")
ames$`Misc Feature`[is.na(ames$`Misc Feature`)] <- "No additional feature"
ames$`Misc Feature`[ames$`Misc Feature` == "TenC"] <- "Othr"

table(ames$`Pool QC`, useNA = "always")
ames$`Pool QC`[is.na(ames$`Pool QC`)] <- "No"
ames$`Pool QC`[ames$`Pool QC` %in% c("TA", "Ex", "Gd", "Fa")] <- "Yes"

ames$`Porch Sq Feet` <- ames$`Open Porch SF` + ames$`Enclosed Porch` + ames$`3Ssn Porch` + ames$`Screen Porch`

ames$`Tot Bathrooms` <- ames$`Full Bath` + 0.5 * ames$`Half Bath` + ames$`Bsmt Full Bath` + 0.5 * ames$`Bsmt Half Bath`

ames$`House Age` <- ames$`Yr Sold` - ames$`Year Remod/Add`

# Most of the information is already included in House Age
ames <- select(ames, -c(`Mo Sold`, `Yr Sold`, `Year Remod/Add`, `Year Built`))
# Most of the information is already included in Porch Sq Feet
ames <- select(ames, -c(`Open Porch SF`, `Enclosed Porch`, `3Ssn Porch`, `Screen Porch`))
# Most of the information is already included in Tot Bathrooms
ames <- select(ames, -c(`Full Bath`, `Half Bath`, `Bsmt Full Bath`, `Bsmt Half Bath`))
# Almost no information is present in these variables
ames <- select(ames, -c(`Pool Area`, Utilities))

# Manual subdivision in training / test
set.seed(123)
# Randomly allocate the id of the variables into training and test
id_train <- sort(sample(1:nrow(ames), size = floor(2 / 3 * nrow(ames)), replace = FALSE))
id_test <- setdiff(1:nrow(ames), id_train)

# Create two different datasets
ames_train <- ames[id_train, ]
ames_test <- ames[id_test, ]

write.csv(data.frame(ames_train), "../data/ames_train.csv", row.names = FALSE)
write.csv(data.frame(ames_test), "../data/ames_test.csv", row.names = FALSE)

rm(list = ls())
ames_train <- read.table("../data/ames_train.csv", header = TRUE, sep = ",")
ames_test <- read.table("../data/ames_train.csv", header = TRUE, sep = ",")

par(mfrow = c(1, 1))
plot(ames_train$Gr.Liv.Area, ames_train$SalePrice,
  xlab = "Ground living area", ylab = "Sale Price", pch = 16, cex = 0.8
)

plot(ames_train$Total.Bsmt.SF, ames_train$SalePrice,
  xlab = "Total Basement SF", ylab = "Sale Price", pch = 16, cex = 0.8
)

plot(ames_train$Garage.Area, ames_train$SalePrice,
  xlab = "Garage Area", ylab = "Sale Price", pch = 16, cex = 0.8
)

plot(ames_train$Porch.Sq.Feet, ames_train$SalePrice,
  xlab = "Porch Square Feet", ylab = "Sale Price", pch = 16, cex = 0.8
)

plot(ames_train$Tot.Bathrooms, ames_train$SalePrice,
  xlab = "Tot Bathrooms", ylab = "Sale Price", pch = 16, cex = 0.8
)

boxplot(SalePrice ~ Overall.Qual, data = ames_train)

boxplot(SalePrice ~ Bsmt.Qual, data = ames_train)

boxplot(SalePrice ~ Exter.Qual, data = ames_train)

boxplot(SalePrice ~ Kitchen.Qual, data = ames_train)

plot(ames_train$House.Age, ames_train$SalePrice,
  xlab = "House Age (Years)", ylab = "Sale Price", pch = 16, cex = 0.8
)

boxplot(SalePrice ~ MS.Zoning, data = ames_train)

boxplot(SalePrice ~ Roof.Style, data = ames_train)

boxplot(SalePrice ~ Neighborhood, data = ames_train)

y_hat_median <- rep(median(ames_train$SalePrice), nrow(ames_test)) # Prediction

MAE <- function(y, y_fit) {
  mean(abs(y - y_fit))
}

RMSLE <- function(y, y_fit) {
  sqrt(mean((log(y) - log(y_fit))^2))
}

round(MAE(ames_test$SalePrice, y_hat_median), 4)
round(RMSLE(ames_test$SalePrice, y_hat_median), 4)
