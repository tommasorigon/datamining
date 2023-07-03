#| message: false
rm(list = ls())
library(ISLR)
library(tidyverse)
data(Hitters)
Hitters <- na.omit(Hitters)
Hitters <- mutate(Hitters, logYears = log10(Years), logSalary = log10(Salary)) %>% select(-c(Salary, Years))
glimpse(Hitters)


#| message: false
library(plotly)
logYears <- seq(from = min(Hitters$logYears), to = max(Hitters$logYears), length = 100)
Hits <- seq(from = min(Hitters$Hits), to = max(Hitters$Hits), length = 100)
logSalary <- matrix(predict(lm(logSalary ~ Hits + logYears, data = Hitters),
  newdata = data.frame(expand.grid(logYears = logYears, Hits = Hits, logSalary = NA))
), ncol = length(logYears))


#| fig-width: 6
#| fig-height: 4
#| fig-align: center
#| warning: false
#| message: false
plot_ly() %>%
  add_surface(
    x = ~logYears, y = ~Hits, z = ~logSalary, colors = "Reds",
    contours = list(
      x = list(show = TRUE, start = 0, end = 3, size = 0.3, color = "white"),
      y = list(show = TRUE, start = 0, end = 200, size = 25, color = "white")
    )
  ) %>%
  add_markers(x = ~ Hitters$logYears, y = ~ Hitters$Hits, z = ~ Hitters$logSalary, size = 0.15, marker = list(color = "black"), showlegend = FALSE) %>%
  layout(
    scene = list(
      camera = list(
        eye = list(x = 0.9, y = -2, z = 0.3)
      )
    )
  ) %>%
  hide_colorbar()



library(leaps)
fit <- regsubsets(logSalary ~ ., data = Hitters, method = "exhaustive", nbest = 1, nvmax = 20)
sum1 <- summary(fit)

plot(rowSums(sum1$which), sum1$cp)

fit <- regsubsets(logSalary ~ ., data = Hitters, method = "backward", nbest = 1, nvmax = 20)
sum1 <- summary(fit)
# plot(rowSums(sum1$which), sum1$cp)
