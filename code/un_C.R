#| message: false
rm(list = ls())
library(ISLR)
library(tidyverse)
data(Hitters)
Hitters <- na.omit(Hitters)
glimpse(Hitters)



Hitters <- mutate(Hitters,
  TotalScore = (RBI + Assists + Walks - Errors) / AtBat,
  logAHits = log1p(CHits / Years),
  logAAtBat = log1p(CAtBat / Years),
  logARuns = log1p(CRuns / Years),
  logARBI = log1p(CRBI / Years),
  logAWalks = log1p(CWalks / Years),
  logAHmRun = log1p(CHmRun / Years),
  RatioHits = Hits / CHits,
  RatioAtBat = AtBat / CAtBat,
  RatioRuns = Runs / CRuns,
  RatioRBI = RBI / CRBI,
  RatioWalks = Walks / CWalks,
  RatioHmRun = HmRun / (CHmRun + 1),
  logYears = log(Years),
  logSalary = log(Salary)
) %>% select(-c(Salary, Years))

# Data splitting
set.seed(123)
id_train <- sample(1:nrow(Hitters), size = floor(0.75 * nrow(Hitters)), replace = FALSE)
id_test <- setdiff(1:nrow(Hitters), id_train)
Hitters_train <- Hitters[id_train, ]
Hitters_test <- Hitters[id_test, ]


#| fig-width: 15
#| fig-height: 7
#| fig-align: center
library(ggcorrplot)
corr <- cor(subset(Hitters_train, select = -c(logSalary, Division, League, NewLeague))) # Remove logSalary
ggcorrplot(corr,
  hc.order = TRUE,
  outline.col = "white",
  ggtheme = ggplot2::theme_bw,
  colors = c("#fc7d0b", "white", "#1170aa")
)



library(leaps)
n <- nrow(Hitters_train)
which_vars <- which(colnames(Hitters_train) %in% c("logSalary", "Division", "League", "NewLeague"))
Hitters_train[, -which_vars] <- scale(Hitters_train[, -which_vars])

fit_forward <- regsubsets(logSalary ~ ., data = Hitters_train, method = "forward", nbest = 1, nvmax = 33)
sum_forward <- summary(fit_forward)

sum_forward$p <- rowSums(sum_forward$which)
sum_forward$aic <- n * log(2 * pi * sum_forward$rss / n) + n + 2 * (sum_forward$p + 1)
sum_forward$bic <- n * log(2 * pi * sum_forward$rss / n) + n + log(n) * (sum_forward$p + 1)
sum_forward$gcv <- (sum_forward$rss / n) / (1 - sum_forward$p / n)^2

fit_backward <- regsubsets(logSalary ~ ., data = Hitters_train, method = "backward", nbest = 1, nvmax = 33)
sum_backward <- summary(fit_backward)

sum_backward$p <- rowSums(sum_backward$which)
sum_backward$aic <- n * log(2 * pi * sum_backward$rss / n) + n + 2 * (sum_backward$p + 1)
sum_backward$bic <- n * log(2 * pi * sum_backward$rss / n) + n + log(n) * (sum_backward$p + 1)
sum_backward$gcv <- (sum_backward$rss / n) / (1 - sum_backward$p / n)^2


#| fig-width: 10
#| fig-height: 3.5
#| fig-align: center

library(ggplot2)
library(ggthemes)
data_ic <- data.frame(
  p = c(sum_forward$p, sum_backward$p),
  BIC = c(sum_forward$bic, sum_backward$bic),
  AIC = c(sum_forward$aic, sum_backward$aic),
  GCV = c(sum_forward$gcv, sum_backward$gcv),
  step = rep(c("Forward", "Backward"), each = length(sum_forward$p))
)
data_ic <- reshape2::melt(data_ic, id = c("p", "step"))
colnames(data_ic) <- c("p", "Stepwise", "Criterion", "value")

ggplot(data = data_ic, aes(x = p, y = value, col = Stepwise)) +
  geom_point() +
  geom_line() +
  facet_wrap(. ~ Criterion, scales = "free") +
  theme_light() +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Model complexity (p)") +
  ylab("Error terms") #+ ylim(c(9e-05, 6e-4))



fit_best <- regsubsets(logSalary ~ ., data = Hitters_train, method = "exhaustive", nbest = 20, nvmax = 33)
sum_best <- summary(fit_best)

sum_best$p <- rowSums(sum_best$which)
sum_best$aic <- n * log(2 * pi * sum_best$rss / n) + n + 2 * (sum_best$p + 1)
sum_best$bic <- n * log(2 * pi * sum_best$rss / n) + n + log(n) * (sum_best$p + 1)
sum_best$gcv <- (sum_best$rss / n) / (1 - sum_best$p / n)^2


#| fig-width: 10
#| fig-height: 3.5
#| fig-align: center

library(ggplot2)
library(ggthemes)
data_ic <- data.frame(p = sum_best$p, BIC = sum_best$bic, AIC = sum_best$aic, GCV = sum_best$gcv)
data_ic <- reshape2::melt(data_ic, id = c("p"))
colnames(data_ic) <- c("p", "Criterion", "value")

ggplot(data = data_ic, aes(x = p, y = value, col = Criterion)) +
  geom_point() +
  # geom_line() +
  facet_wrap(. ~ Criterion, scales = "free") +
  theme_light() +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Model complexity (p)") +
  ylab("Information Criterion")



library(DT)
p <- ncol(sum_best$which)
tab <- data.frame(OLS = rep(0, p), best_subset = rep(0, p))
rownames(tab) <- colnames(sum_best$which)

tab$OLS <- coef(lm(logSalary ~ ., data = Hitters_train))
tab$best_subset[sum_best$which[which.min(sum_best$bic), ]] <- coef(fit_best, which.min(sum_best$bic))

datatable(tab[-1, ], colnames = c("OLS", "Best subset"), options = list(
  pageLength = 13,
  dom = "pt",
  order = list(list(0, "asc"))
)) %>%
  formatRound(columns = 1:2, digits = 2) %>%
  formatStyle(
    columns = 0, fontWeight = "bold"
  ) %>%
  formatStyle(
    columns = 1:2,
    backgroundColor = styleInterval(0, c("#FED8B1", "#DBE9FA"))
  ) %>%
  formatStyle(
    columns = 1:2,
    backgroundColor = styleEqual(0, c("white"))
  )



# I use cor = FALSE because the variables have been standardized
pr <- princomp(model.matrix(logSalary ~ ., data = Hitters_train)[, -1], cor = FALSE)

X <- model.matrix(logSalary ~ ., data = Hitters_train)
y <- Hitters_train$logSalary
Z <- pr$scores
V <- as.matrix(pr$loadings)
n <- length(y)

# Main chunk of code; fitting several models and storing some relevant quantities
ncomp_list <- 1:ncol(Z)

# Initialization
data_goodness <- data.frame(ncomp = ncomp_list, BIC = NA, AIC = NA, GCV = NA)

# Code execution
for (ncomp in ncomp_list) {
  # Fitting a model with
  # Equivalent to lm(y ~ Z)
  Z_comp <- matrix(Z[, 1:ncomp], ncol = ncomp)
  V_comp <- matrix(V[, 1:ncomp], ncol = ncomp)
  gamma <- apply(Z_comp, 2, function(x) crossprod(x, y)) / apply(Z_comp, 2, function(x) crossprod(x))
  # beta <- c(mean(y), V_comp %*% gamma) # Equivalent to coef()
  # fit <- lm(logSalary ~ Z_comp, data = Hitters_train)
  # y_hat <- fitted(fit)
  y_hat <- c(mean(y) + Z_comp %*% gamma)
  sigma2 <- mean((y - y_hat)^2)
  # Training goodness of fit
  data_goodness$BIC[ncomp] <- n * log(2 * pi * sigma2) + n + log(n) * (ncomp + 2)
  data_goodness$AIC[ncomp] <- n * log(2 * pi * sigma2) + n + 2 * (ncomp + 2)
  data_goodness$GCV[ncomp] <- sigma2 / (1 - (ncomp + 1) / n)^2
}


#| fig-width: 10
#| fig-height: 3.5
#| fig-align: center

# Organization of the results for graphical purposes
data_bv <- data.frame(p = ncomp_list + 1, BIC = data_goodness$BIC, AIC = data_goodness$AIC, GCV = data_goodness$GCV)
data_bv <- reshape2::melt(data_bv, id = "p")
levels(data_bv$variable) <- c("BIC", "AIC", "GCV")
colnames(data_bv) <- c("p", "Criterion", "value")

ggplot(data = data_bv, aes(x = p, y = value, col = Criterion)) +
  geom_line() +
  geom_point() +
  facet_wrap(. ~ Criterion, scales = "free") +
  theme_light() +
  theme(legend.position = "none") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Model complexity (p)") +
  ylab("Error term")



library(DT)
tab <- data.frame(OLS = rep(0, p), best_subset = rep(0, p), PCR = rep(0, p))
rownames(tab) <- colnames(sum_best$which)

tab$OLS <- coef(lm(logSalary ~ ., data = Hitters_train))
tab$best_subset[sum_best$which[which.min(sum_best$bic), ]] <- coef(fit_best, which.min(sum_best$bic))

Z_comp <- Z[, 1:21]
V_comp <- V[, 1:21]
gamma <- apply(Z_comp, 2, function(x) crossprod(x, y)) / apply(Z_comp, 2, function(x) crossprod(x))
tab$PCR <- c(mean(y), V_comp %*% gamma)


datatable(tab[-1, ], colnames = c("OLS", "Best subset", "PCR"), options = list(
  pageLength = 13,
  dom = "pt",
  order = list(list(0, "asc"))
)) %>%
  formatRound(columns = 1:3, digits = 3) %>%
  formatStyle(
    columns = 0, fontWeight = "bold"
  ) %>%
  formatStyle(
    columns = 1:3,
    backgroundColor = styleInterval(0, c("#FED8B1", "#DBE9FA"))
  ) %>%
  formatStyle(
    columns = 1:3,
    backgroundColor = styleEqual(0, c("white"))
  )



library(glmnet)
asd <- cv.glmnet(X, y, family = "gaussian", alpha = 0.5, lambda = exp(seq(-6, 1, length = 100)), nfolds = 263, grouped = FALSE)
asd
plot(asd)

lasso.mod <- glmnet(X, y, alpha = 0.5)
plot(lasso.mod, "lambda", label = TRUE)
