# LAB 6 (Ames Housing) --------------------------------------------------------------------------
# Course: Data Mining
# Author: Tommaso Rigon

# Predictive analysis ----------------------------------------------------------------------------

rm(list = ls())
ames <- read.table("../data/ames.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

## Changing perspective - A classification problem
ames$Luxury <- factor(ames$SalePrice > 120000)
ames <- subset(ames, select = -c(SalePrice))

table(ames$Luxury)

# Training, validation and test set ----------------------------------------------------------------------------

# Manual subdivision in training / test
set.seed(123)
# Randomly allocate the id of the variables into training and test
id_train <- sort(sample(1:nrow(ames), size = floor(0.75 * nrow(ames)), replace = FALSE))
id_test <- setdiff(1:nrow(ames), id_train)

# Create two different datasets
ames_train <- ames[id_train, ]
ames_test <- ames[id_test, ]

# A first simple model --------------------------------------------------------------------------
m_simple <- glm(Luxury ~ Overall.Qual + Gr.Liv.Area + House.Age + Tot.Bathrooms, 
                data = ames_train, family = "binomial")
summary(m_simple)

# Ridge regression ----------------------------------------------------------------------

library(glmnet)

# The lambda parameter can be then conveniently selected via cross-validation
X_shrinkage <- model.matrix(Luxury ~ ., data = ames_train)[, -1]
y_shrinkage <- ames_train$Luxury

# We need to set alpha = 0 to use the ridge
fit_ridge <- glmnet(X_shrinkage, y_shrinkage, alpha = 0, family = "binomial")

par(mfrow = c(1, 1))
plot(fit_ridge, xvar = "lambda")

## Cross-validation for ridge regression
ridge_cv <- cv.glmnet(X_shrinkage, y_shrinkage, alpha = 0, family = "binomial")
par(mfrow = c(1, 1))
plot(ridge_cv)

ridge_cv$lambda.min
ridge_cv$lambda.1se

# MSLE for lambda.min and lambda.1se
ridge_cv$cvm[ridge_cv$index]

## Lasso -----------------------------------------------------------------------------------------

# We need to set alpha = 1 to use the lasso
fit_lasso <- glmnet(X_shrinkage, y_shrinkage, alpha = 1, family = "binomial")

# Coefficient path
plot(fit_lasso, xvar = "lambda")

## Cross-validation for ridge regression
lasso_cv <- cv.glmnet(X_shrinkage, y_shrinkage, alpha = 1, family = "binomial")
par(mfrow = c(1, 1))
plot(lasso_cv)

lasso_cv$lambda.min
lasso_cv$lambda.1se

# MSLE for lambda.min and lambda.1se
lasso_cv$cvm[lasso_cv$index]

# GAM ----------------------------------------------------------------------------------------------------

library(mgcv)
m_gam_simple <- gam(Luxury ~ s(Overall.Qual) + s(Gr.Liv.Area) + s(House.Age) + s(Tot.Bathrooms), 
                data = ames_train, family = "binomial")
summary(m_gam_simple)
plot(m_gam_simple, scale = 0, se = FALSE)

m_gam <- gam(Luxury ~ s(Overall.Qual) + s(Gr.Liv.Area) + s(House.Age) + s(Tot.Bathrooms) + s(Porch.Sq.Feet) + s(Wood.Deck.SF) + Kitchen.Qual  + s(Garage.Area), 
                    data = ames_train, family = "binomial", select = TRUE)
summary(m_gam)
plot(m_gam, scale = 0, se = FALSE)

# MARS ----------------------------------------------------------------------------------------------------

library(earth)
m_mars1 <- earth(Luxury ~ ., data = ames_train, glm=list(family=binomial), degree = 1)
summary(m_mars1)

m_mars2 <- earth(Luxury ~ ., data = ames_train, glm=list(family=binomial), degree = 2)
summary(m_mars2)

m_mars3 <- earth(Luxury ~ ., data = ames_train, glm=list(family=binomial), degree = 1, penalty = 5)
summary(m_mars3)

# Random forests
library(randomForest)
m_rf <- randomForest(Luxury ~ ., data = ames_train)
m_rf

# Final choice --------------------------------------------------------------------------------------------

# Simple
y_hat_simple <- predict(m_simple, newdata = ames_test, type = "response")

# Ridge
y_hat_ridge <- c(predict(ridge_cv, newx = model.matrix(Luxury ~ ., data = ames_test)[, -1], type = "response"))

# Lasso
y_hat_lasso <- c(predict(lasso_cv, newx = model.matrix(Luxury ~ ., data = ames_test)[, -1], type = "response"))

# GAM
y_hat_gam_simple <- c(predict(m_gam_simple, newdata = ames_test, type = "response"))

y_hat_gam <- c(predict(m_gam, newdata = ames_test, type = "response"))

# MARS1
y_hat_mars1 <- c(predict(m_mars1, newdata = ames_test, type = "response"))

# MARS2
y_hat_mars2 <- c(predict(m_mars2, newdata = ames_test, type = "response"))

# MARS3
y_hat_mars3 <- c(predict(m_mars3, newdata = ames_test, type = "response"))

# RF
y_hat_rf <- c(predict(m_rf, newdata = ames_test, type = "prob")[, 2])


# Final summary of the results ----------------------------------------------

metrics <- function(y, probs, cutoff = 0.5){
  tab <- table(probs > cutoff, y)
  acc <- sum(diag(tab)) / sum(tab)
  fp <- tab[2, 1] / sum(tab[, 1])
  fn <- tab[1, 2] / sum(tab[, 2])
  bin <- sum(log(probs[y == TRUE])) + sum(log(1 - probs[y == FALSE]))
  metrics <- c(acc, fp, fn, bin, AUC::auc(AUC::roc(probs, y)))
  names(metrics) <- c("Accuracy", "False-Positive", "False-Negative", "Binomial loss", "AUC")
  metrics
}

# Target variable
y_test <- ames_test$Luxury

cutoff <- 0.5
metrics(y = y_test, probs = y_hat_simple, cutoff = cutoff)
metrics(y = y_test, probs = y_hat_ridge, cutoff = cutoff)
metrics(y = y_test, probs = y_hat_lasso, cutoff = cutoff)
metrics(y = y_test, probs = y_hat_gam_simple, cutoff = cutoff)
metrics(y = y_test, probs = y_hat_gam, cutoff = cutoff)
metrics(y = y_test, probs = y_hat_mars1, cutoff = cutoff)
metrics(y = y_test, probs = y_hat_mars2, cutoff = cutoff)
metrics(y = y_test, probs = y_hat_mars3, cutoff = cutoff)
metrics(y = y_test, probs = y_hat_rf, cutoff = cutoff)

table(y_hat_lasso >  cutoff, y_test)

library(ROCR)
pred_lasso <- prediction(y_hat_lasso, y_test)
perf_lasso <- performance(pred_lasso, "tpr","fpr")

pred_simple <- prediction(y_hat_simple, y_test)
perf_simple <- performance(pred_simple, "tpr","fpr")

pred_gam <- prediction(y_hat_gam, y_test)
perf_gam <- performance(pred_gam, "tpr","fpr")

pred_mars3 <- prediction(y_hat_mars3, y_test)
perf_mars3 <- performance(pred_mars3, "tpr","fpr")

plot(perf_lasso, col = "darkblue")
abline(c(0,1), lty = "dotted")
plot(perf_gam, col = "darkgreen", add = TRUE)
plot(perf_mars3, col = "black", add = TRUE, lty = "dotted")
plot(perf_simple, col = "darkorange", add = TRUE, lty = "dashed")

