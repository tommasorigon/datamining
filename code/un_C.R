#| message: false
rm(list = ls())
library(ISLR)
library(tidyverse)
data(Hitters)
Hitters <- na.omit(Hitters)
glimpse(Hitters)



Hitters <- mutate(Hitters,
  logAHits = log1p(CHits / Years),
  logAAtBat = log1p(CAtBat / Years),
  logARuns = log1p(CRuns / Years),
  logARBI = log1p(CRBI / Years),
  logAWalks = log1p(CWalks / Years),
  logAHmRun = log1p(CHmRun / Years),
  logYears = log(Years),
  logSalary = log(Salary)
) %>% select(-c(Salary, Years))


#| fig-width: 9
#| fig-height: 5
#| fig-align: center
library(ggcorrplot)
corr <- cor(Hitters[, -c(14, 13, 18, 26)]) # Remove logSalary
ggcorrplot(corr,
  hc.order = TRUE,
  outline.col = "white",
  ggtheme = ggplot2::theme_bw,
  colors = c("#fc7d0b", "white", "#1170aa")
)



library(leaps)
n <- nrow(Hitters)

Hitters[, -c(14, 13, 18, 26)] <- scale(Hitters[, -c(14, 13, 18, 26)])

fit_forward <- regsubsets(logSalary ~ ., data = Hitters, method = "forward", nbest = 1, nvmax = 26)
sum_forward <- summary(fit_forward)

sum_forward$p <- rowSums(sum_forward$which)
sum_forward$aic <- sum_forward$bic - log(n) * sum_forward$p + 2 * sum_forward$p

fit_backward <- regsubsets(logSalary ~ ., data = Hitters, method = "backward", nbest = 1, nvmax = 26)
sum_backward <- summary(fit_backward)

sum_backward$p <- rowSums(sum_backward$which)
sum_backward$aic <- sum_backward$bic - log(n) * sum_backward$p + 2 * sum_backward$p


#| fig-width: 8
#| fig-height: 4.5
#| fig-align: center

library(ggplot2)
library(ggthemes)
data_ic <- data.frame(p = c(sum_forward$p, sum_backward$p), AIC = c(sum_forward$aic, sum_backward$aic), BIC = c(sum_forward$bic, sum_backward$bic), step = rep(c("Forward", "backward"), each = length(sum_forward$p)))
data_ic <- reshape2::melt(data_ic, id = c("p", "step"))
colnames(data_ic) <- c("p", "Stepwise", "IC", "value")

ggplot(data = data_ic, aes(x = p, y = value, col = IC, linetype = Stepwise)) +
  geom_point() +
  geom_line() +
  theme_light() +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Model complexity (p)") +
  ylab("Information Criterion") #+ ylim(c(9e-05, 6e-4))



fit_best <- regsubsets(logSalary ~ ., data = Hitters, method = "exhaustive", nbest = 20, nvmax = 26)
sum_best <- summary(fit_best)

sum_best$p <- rowSums(sum_best$which)
sum_best$aic <- sum_best$bic - log(n) * sum_best$p + 2 * sum_best$p


#| fig-width: 8
#| fig-height: 4.5
#| fig-align: center

library(ggplot2)
library(ggthemes)
data_ic <- data.frame(p = sum_best$p, AIC = sum_best$aic, BIC = sum_best$bic)
data_ic <- reshape2::melt(data_ic, id = c("p"))
colnames(data_ic) <- c("p", "IC", "value")

ggplot(data = data_ic, aes(x = p, y = value, col = IC)) +
  geom_point() +
  # geom_line() +
  theme_light() +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Model complexity (p)") +
  ylab("Information Criterion") +
  ylim(c(-260, -100))



library(DT)
p <- ncol(sum_best$which)
tab <- data.frame(OLS = rep(0, p), BIC_best = rep(0, p), AIC_best = rep(0, p))
rownames(tab) <- colnames(sum_backward$which)

tab$OLS <- coef(lm(logSalary ~ ., data = Hitters))
tab$BIC_best[sum_best$which[which.min(sum_best$bic), ]] <- coef(fit_best, which.min(sum_best$bic))
tab$AIC_best[sum_best$which[which.min(sum_best$aic), ]] <- coef(fit_best, which.min(sum_best$aic))

datatable(tab, colnames = c("OLS", "BIC best", "AIC best"), options = list(
  pageLength = 13,
  dom = "pt",
  order = list(list(0, "asc"))
)) %>%
  formatRound(columns = 1:3, digits = 2) %>%
  formatStyle(
    columns = 0, fontWeight = "bold"
  ) %>%
  formatStyle(
    columns = 1:5,
    backgroundColor = styleInterval(0, c("#FED8B1", "#DBE9FA"))
  ) %>%
  formatStyle(
    columns = 1:5,
    backgroundColor = styleEqual(0, c("white"))
  )
