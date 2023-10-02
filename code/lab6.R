# LAB 6 (Ames Housing) --------------------------------------------------------------------------
# Course: Data Mining
# Author: Tommaso Rigon

# Predictive analysis ----------------------------------------------------------------------------

rm(list = ls())
ames <- read.table("../data/ames.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

# Training, validation and test set ----------------------------------------------------------------------------

# Manual subdivision in training / test
set.seed(123)
# Randomly allocate the id of the variables into training and test
id_train <- sort(sample(1:nrow(ames), size = floor(0.5 * nrow(ames)), replace = FALSE))
id_validation_test <- setdiff(1:nrow(ames), id_train)
# Now we allocate the validation test
id_validation <- sort(sample(id_validation_test, size = floor(0.5 * length(id_validation_test)), replace = FALSE))
# And finally the test set
id_test <- setdiff(id_validation_test, id_validation)

# Create two different datasets
ames_train <- ames[id_train, ]
ames_validation <- ames[id_validation, ]
ames_test <- ames[id_test, ]