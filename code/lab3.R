# ----------------------------------------
# Title: LAB 3 (Juice data, classification)
# Author: Tommaso Rigon
# ----------------------------------------

rm(list = ls())

# The data on fruit juice purchases are taken from Chapter 11 of Foster, Stine and Waterman "Business Analysis Using Regression".

# The data refer to 1070 fruit juice purchases of two different brands (MM and CH) in certain US supermarkets, supplied with some contributory variables. The variables are

# Variable     Description

# choice       pre-chosen brand (factor, with 2 levels)
# id.cust      customer identification
# week         identifier of week of purchase
# priceCH      reference price for  brand CH (USD)
# priceMM      reference price for  brand MM (USD)
# discountCH   discount applied to  product CH (USD)
# discountMM   discount applied to product MM (USD)
# loyaltyCH    loyalty indicator for  product CH
# loyaltyMM    loyalty indicator for  product MM
# store        store identifier (factor, with 5 levels)
# ...          other variables obtained by combining the former

# Variable loyaltyMM is constructed starting from the value 0.5 and updating with every purchase by the same customer, with a value which increases by 20% of the current difference between the current value and 1, if the customer chose MM, and falls by 20% of the  difference between the current value and 0 if the customer chose CH. The corresponding variable loyaltyCH is given by  1-loyaltyMM.

juice <- read.table("https://tommasorigon.github.io/StatIII/data/juice.txt", header = TRUE, stringsAsFactors = TRUE)
str(juice)

# QUESTION 1. We are interested in predicting the preference of customers towards CH and MM as a function of relevant covariates. What kind of model could be appropriate? What covariates do you think might be useful?

# QUESTION 2: let us consider the following "wrong" approach, which causes an error. Why do you think it happens?

# A WRONG idea - the "automatic" data scientist
m_wrong <- glm(choice ~ ., family = binomial, data = juice)
summary(m_wrong)

# QUESTION 3: Clean the data and provide some descriptive analysis that may be helpful to understand the relationship between variables and the response.

# COMMENT: in the dataset there are (i) irrelevant variables (ii) leaker variables such as buyCH (iii) redundant variables.

# buyCH and choice represent the same variable in different formats (factor vs int). One or the other must be removed
juice <- subset(juice, select = -c(buyCH))

# loyaltyCH and loyaltyMM are collinear, we cannot use both.

# id.cust is difficult to use, because there a lot of customers and very few observations per id
juice <- subset(juice, select = -c(id.cust))

# store, StoreID, store7 are referring to the same quantity
juice <- subset(juice, select = -c(StoreID, store7))
juice$store <- as.factor(juice$store)

# salepriceCH is the FINAL price, obtained as priceCH - discountCH, making these three variables COLLINEAR.
# We can keep them but we cannot use them together
plot(juice$priceCH - juice$discountCH, juice$salepriceCH)
plot(juice$priceMM - juice$discountMM, juice$salepriceMM)

# pricediff and listpricediff are also collinear variables, being equal to
# pricediff = salepriceMM - salepriceCH;
# listpricediff = priceMM - priceCH
plot(juice$salepriceMM - juice$salepriceCH, juice$pricediff)
plot(juice$priceMM - juice$priceCH, juice$listpricediff)

# pctdiscMM and pctdiscCH are also potentially problematic, albeit non-collinear. specialCH are "special weeks", again potentially problematic

# QUESTION 4: identify a good model using a subset of the total number variables. Can we use all the available variables? Why not?

# Let us consider a FORWARD approach for building this model. The backward does not work (!)
m0 <- glm(choice ~ 1, family = binomial, data = juice)
summary(m0)

# Variables we would like to POTENTIALLY consider
scope <- choice ~ week + priceCH + priceMM + discountCH + discountMM + specialCH + specialMM + loyaltyCH + loyaltyMM + salepriceMM + salepriceCH + pricediff + pctdiscMM + pctdiscCH + listpricediff + store

# Which variable is the most "relevant", meaning that reduced the deviance the most?
add1(m0, scope = scope, test = "LRT")

# Loyalty is the most relevance variable. Let us include it (one of them, because they are obviously collinear)
m1 <- glm(choice ~ loyaltyMM, family = binomial, data = juice)
summary(m1)

add1(m1, scope = scope, test = "LRT")
m2 <- glm(choice ~ loyaltyMM + pricediff, family = binomial, data = juice)
summary(m2)

add1(m2, scope = scope, test = "LRT")

m3 <- glm(choice ~ loyaltyMM + pricediff + store, family = binomial, data = juice)
summary(m3)
add1(m3, scope = scope, test = "LRT")

# Fully automatic procedures

# Forward selection (k = 2 is the AIC)
m_forward <- step(m0, scope = scope, direction = "forward", k = 2)
summary(m_forward)

# Stepwise selection
m_step <- step(m0, scope = scope, direction = "both", k = 2)
summary(m_step)

# QUESTION 5: evaluate the goodness of fit of the estimated model.

# Diagnostic plots are not usuful for binary data
par(mfrow = c(2, 2))
plot(m3, which = 1:4) # As usual, these graphs are not particularly informative
par(mfrow = c(1, 1))

pred_m3 <- predict(m3, type = "response")
class_m3 <- as.factor(pred_m3 > 0.5)

# Confusion matrix
table(class_m3, juice$choice)

# Accuracy of the model is about 83%:
sum(diag(table(juice$choice, class_m3))) / nrow(juice)

# Error is 1 - accuracy, is about 16%:
1 - sum(diag(table(juice$choice, class_m3))) / nrow(juice)

# Calibration plot
breaks <- seq(from = 0, to = 1, length = 20)
class_m3 <- cut(pred_m3, breaks = breaks)

pred_avg <- tapply(pred_m3, class_m3, mean)
prop <- tapply(as.numeric(juice$choice) - 1, class_m3, mean)

plot(prop, pred_avg, pch = 16, xlab = "Empirical proportions (binned)", ylab = "Predicted proportions (binned)")
rug(pred_m3)
abline(c(0, 1), lty = "dotted")

# There are also specialized approaches, such as the ROC curve and the lift curve. You will study them in other courses such as Data Mining.
