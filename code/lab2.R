rm(list = ls())
library(tidyverse)
ames <- read_csv("../data/AmesHousing.csv", )
ames

glimpse(ames)
skimr::skim(ames)

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

table(ames$`Bsmt Exposure`, ames$`Bsmt Cond`, useNA = "always")
id_no_bsmt <- apply(cbind(
  is.na(ames$`Bsmt Exposure`), is.na(ames$`Bsmt Cond`),
  is.na(ames$`BsmtFin Type 1`), is.na(ames$`BsmtFin Type 2`),
  is.na(ames$`Bsmt Qual`)
), 1, any)

ames$`Bsmt Exposure`[id_no_bsmt] <- "No basement"
ames$`Bsmt Cond`[id_no_bsmt] <- "No basement"
ames$`BsmtFin Type 1`[id_no_bsmt] <- "No basement"
ames$`BsmtFin Type 2`[id_no_bsmt] <- "No basement"
ames$`Bsmt Qual`[id_no_bsmt] <- "No basement"

ames$`Bsmt Full Bath`[is.na(ames$`Bsmt Full Bath`)] <- 0
ames$`Bsmt Half Bath`[is.na(ames$`Bsmt Half Bath`)] <- 0

table(ames$Electrical, useNA = "always")
ames$Electrical[is.na(ames$Electrical)] <- "SBrkr"

table(ames$Fence, useNA = "always")
ames$`Fence`[is.na(ames$`Fence`)] <- "No fence"

table(ames$`Fireplace Qu`, useNA = "always")
ames$`Fireplace Qu`[is.na(ames$`Fireplace Qu`)] <- "No fireplace"

table(ames$`Garage Cond`, ames$`Garage Type`, useNA = "always")
id_no_garage <- apply(cbind(
  is.na(ames$`Garage Cond`), is.na(ames$`Garage Finish`),
  is.na(ames$`Garage Qual`), is.na(ames$`Garage Type`)
), 1, any)

ames$`Garage Cond`[id_no_garage] <- "No garage"
ames$`Garage Finish`[id_no_garage] <- "No garage"
ames$`Garage Qual`[id_no_garage] <- "No garage"
ames$`Garage Type`[id_no_garage] <- "No garage"

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
ames_train <- read.table("../data/ames_train.csv",
  header = TRUE, sep = ",",
  stringsAsFactors = TRUE
)
ames_test <- read.table("../data/ames_train.csv",
  header = TRUE, sep = ",",
  stringsAsFactors = TRUE
)

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

m_simple <- lm(SalePrice ~ Gr.Liv.Area + Overall.Qual + House.Age + Tot.Bathrooms, data = ames_train)
summary(m_simple)

y_hat_simple <- predict(m_simple, newdata = ames_test)

# Perform a small correction:
y_hat_simple <- pmax(y_hat_simple, 30000)

round(MAE(ames_test$SalePrice, y_hat_simple), 4)
round(RMSLE(ames_test$SalePrice, y_hat_simple), 4)

m_simple_log <- lm(log(SalePrice) ~ Gr.Liv.Area + Overall.Qual + House.Age + Tot.Bathrooms, data = ames_train)
summary(m_simple_log)

# Re-obtain the original scale
y_hat_simple_log <- exp(predict(m_simple_log, newdata = ames_test))

round(MAE(ames_test$SalePrice, y_hat_simple_log), 4)
round(RMSLE(ames_test$SalePrice, y_hat_simple_log), 4)

# Here I compute some basic quantities
X_train <- model.matrix(SalePrice ~ ., data = ames_train)[, -1]
y_train <- ames_train$SalePrice
n <- nrow(X_train)
p <- ncol(X_train) # This does not include the intercept
c(n, p)

m_full <- lm(SalePrice ~ ., data = ames_train)
summary(m_full)

# 4 collinearities are due to "no basement", 3 collinearities are due to "no garage"

# Moreover, note that at the basement
head(cbind(
  ames_train$Bsmt.Unf.SF + ames_train$BsmtFin.SF.1 + ames_train$BsmtFin.SF.2,
  ames_train$Total.Bsmt.SF
))

# And that at the ground floors
head(cbind(ames_train$X1st.Flr.SF + ames_train$X2nd.Flr.SF + ames_train$Low.Qual.Fin.SF, ames_train$Gr.Liv.Area))

library(leaps)
fit_forward <- regsubsets(SalePrice ~ .,
  data = ames_train, method = "forward",
  nbest = 1, nvmax = 200
)
sum_forward <- summary(fit_forward)

fit_backward <- regsubsets(SalePrice ~ .,
  data = ames_train, method = "backward",
  nbest = 1, nvmax = 200
)
sum_backward <- summary(fit_backward)

which(sum_forward$which[1, ]) # Model with one covariate
which(sum_forward$which[2, ]) # Model with two covariates
which(sum_forward$which[3, ]) # Model with three covariates
which(sum_forward$which[4, ]) # Model with four covariates

which(sum_backward$which[1, ]) # Model with one covariate
which(sum_backward$which[2, ]) # Model with two covariates
which(sum_backward$which[3, ]) # Model with three covariates
which(sum_backward$which[4, ]) # Model with four covariates
which(sum_backward$which[5, ]) # Model with four covariates
which(sum_backward$which[6, ]) # Model with four covariates
which(sum_backward$which[7, ]) # Model with four covariates

library(broom)
fit_forward_summary <- fit_forward %>%
  tidy() %>%
  rowwise() %>%
  mutate(p = sum(c_across(MS.SubClass:House.Age)), .keep = "unused") %>%
  ungroup()

fit_backward_summary <- fit_backward %>%
  tidy() %>%
  rowwise() %>%
  mutate(p = sum(c_across(MS.SubClass:House.Age)), .keep = "unused") %>%
  ungroup()

par(mfrow = c(1, 2))
plot(fit_forward_summary$p, fit_forward_summary$r.squared,
  type = "b", cex = 0.8, pch = 16, ylab = "R-squared", xlab = "p",
  main = "Forward regression"
)
plot(fit_forward_summary$p, fit_forward_summary$mallows_cp,
  type = "b", cex = 0.8, pch = 16, ylab = "Mallows' Cp", xlab = "p",
  main = "Forward regression"
)

plot(fit_backward_summary$p, fit_backward_summary$r.squared,
  type = "b", cex = 0.8, pch = 16, ylab = "R-squared", xlab = "p",
  main = "Backward regression"
)
plot(fit_backward_summary$p, fit_backward_summary$mallows_cp,
  type = "b", cex = 0.8, pch = 16, ylab = "Mallows' Cp", xlab = "p",
  main = "Backward regression"
)
abline(v = which.min(fit_backward_summary$mallows_cp), lty = "dashed")

which.min(fit_backward_summary$mallows_cp)
which(sum_backward$which[which.min(fit_backward_summary$mallows_cp), ])

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object[["call"]][[2]])

  # Compute the design matrix
  X <- model.matrix(form, newdata)
  # Identify the correct beta coefficients
  beta_hat <- coef(object, id = id)
  xvars <- names(beta_hat)

  # Making the predictions
  pred_mat <- X[, xvars] %*% beta_hat

  # Housekeeping
  pred <- as.numeric(pred_mat)
  names(pred) <- rownames(X)
  pred
}

predict(fit_backward, newdata = ames_train, id = 103)

library(rsample)

p_max <- 200
set.seed(123)
V <- 10
cv_fold <- vfold_cv(ames_train, v = V)
resid_subs <- matrix(0, nrow(ames_train), p_max + 1)
resid_log_subs <- matrix(0, nrow(ames_train), p_max + 1)

for (k in 1:V) {
  # Estimation of the null model
  fit_null <- lm(SalePrice ~ 1, data = data.frame(analysis(cv_fold$splits[[k]])))
  # Forward and backward regression
  fit_cv <- regsubsets(SalePrice ~ .,
    data = analysis(cv_fold$splits[[k]]),
    method = "backward", nbest = 1, nvmax = p_max
  )

  # Hold-out quantities
  y_k <- assessment(cv_fold$splits[[k]])$SalePrice

  # Residuals for the null model
  y_hat_null <- pmax(30000, predict(fit_null, assessment(cv_fold$splits[[k]])))
  resid_subs[complement(cv_fold$splits[[k]]), 1] <- y_k - y_hat_null
  resid_log_subs[complement(cv_fold$splits[[k]]), 1] <- log(y_k) - log(y_hat_null)

  # Residuals of the best models for different values of p
  for (j in 2:(p_max + 1)) {
    y_hat <- pmax(30000, predict(fit_cv, assessment(cv_fold$splits[[k]]), j - 1))
    resid_subs[complement(cv_fold$splits[[k]]), j] <- y_k - y_hat
    resid_log_subs[complement(cv_fold$splits[[k]]), j] <- log(y_k) - log(y_hat)
  }
}

data_cv <- data.frame(
  p = 0:p_max,
  MAE = apply(resid_subs, 2, function(x) mean(abs(x))),
  RMSLE = apply(resid_log_subs^2, 2, function(x) mean(x)),
  SE = apply(resid_log_subs^2, 2, function(x) sd(x) / sqrt(n))
)

se_rule <- data_cv$RMSLE[which.min(data_cv$RMSLE)] + data_cv$SE[which.min(data_cv$RMSLE)]
p_optimal <- which(data_cv$RMSLE < se_rule)[1]

plot(data_cv$p, data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE", xlab = "p")
abline(v = p_optimal, lty = "dashed")

plot(data_cv$p, data_cv$RMSLE, type = "b", pch = 16, cex = 0.4, ylab = "RMSLE", xlab = "p")
abline(v = p_optimal, lty = "dashed")

library(leaps)
fit_forward <- regsubsets(log(SalePrice) ~ .,
  data = ames_train, method = "forward",
  nbest = 1, nvmax = p - 10
)
sum_forward <- summary(fit_forward)

fit_backward <- regsubsets(log(SalePrice) ~ .,
  data = ames_train, method = "backward",
  nbest = 1, nvmax = p - 10
)
sum_backward <- summary(fit_backward)

which(sum_forward$which[1, ]) # Model with one covariate
which(sum_forward$which[2, ]) # Model with two covariates
which(sum_forward$which[3, ]) # Model with three covariates
which(sum_forward$which[4, ]) # Model with four covariates

which(sum_backward$which[1, ]) # Model with one covariate
which(sum_backward$which[2, ]) # Model with two covariates
which(sum_backward$which[3, ]) # Model with three covariates
which(sum_backward$which[4, ]) # Model with four covariates
which(sum_backward$which[5, ]) # Model with four covariates
which(sum_backward$which[6, ]) # Model with four covariates
which(sum_backward$which[7, ]) # Model with four covariates
which(sum_backward$which[8, ]) # Model with four covariates

library(rsample)

p_max <- 200
set.seed(123)
V <- 10
cv_fold <- vfold_cv(ames_train, v = V)
resid_subs <- matrix(0, nrow(ames_train), p_max + 1)
resid_log_subs <- matrix(0, nrow(ames_train), p_max + 1)

for (k in 1:V) {
  # Estimation of the null model
  fit_null <- lm(log(SalePrice) ~ 1, data = data.frame(analysis(cv_fold$splits[[k]])))
  # Forward and backward regression
  fit_backward <- regsubsets(log(SalePrice) ~ .,
    data = analysis(cv_fold$splits[[k]]),
    method = "backward", nbest = 1, nvmax = p_max
  )

  # Hold-out quantities
  y_k <- assessment(cv_fold$splits[[k]])$SalePrice

  # Residuals for the null model
  y_hat_null <- exp(predict(fit_null, assessment(cv_fold$splits[[k]])))
  resid_subs[complement(cv_fold$splits[[k]]), 1] <- y_k - y_hat_null
  resid_log_subs[complement(cv_fold$splits[[k]]), 1] <- log(y_k) - log(y_hat_null)

  # Residuals of the best models for different values of p
  for (j in 2:(p_max + 1)) {
    y_hat <- exp(predict(fit_backward, assessment(cv_fold$splits[[k]]), j - 1))
    resid_subs[complement(cv_fold$splits[[k]]), j] <- y_k - y_hat
    resid_log_subs[complement(cv_fold$splits[[k]]), j] <- log(y_k) - log(y_hat)
  }
}

data_cv <- data.frame(
  p = 0:p_max,
  MAE = apply(resid_subs, 2, function(x) mean(abs(x))),
  RMSLE = apply(resid_log_subs^2, 2, function(x) mean(x)),
  SE = apply(resid_log_subs^2, 2, function(x) sd(x) / sqrt(n))
)


se_rule <- data_cv$RMSLE[which.min(data_cv$RMSLE)] + data_cv$SE[which.min(data_cv$RMSLE)]
p_optimal <- which(data_cv$RMSLE < se_rule)[1]

plot(data_cv$p, data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE", xlab = "p")
abline(v = p_optimal, lty = "dashed")

plot(data_cv$p, data_cv$RMSLE, type = "b", pch = 16, cex = 0.4, ylab = "RMSLE", xlab = "p")
abline(v = p_optimal, lty = "dashed")

y_hat_backward_log <- exp(predict(fit_backward, newdata = ames_test, id = p_optimal))

y_hat_full <- pmax(30000, predict(m_full, newdata = ames_test))

# Simple log
round(MAE(ames_test$SalePrice, y_hat_simple_log), 4)
round(RMSLE(ames_test$SalePrice, y_hat_simple_log), 4)

# Full
round(MAE(ames_test$SalePrice, y_hat_full), 4)
round(RMSLE(ames_test$SalePrice, y_hat_full), 4)

# Backward
round(MAE(ames_test$SalePrice, y_hat_backward), 4)
round(RMSLE(ames_test$SalePrice, y_hat_backward), 4)
