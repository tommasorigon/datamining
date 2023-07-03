
library(ISLR)
library(tidyverse)
data(Hitters)
Hitters <- na.omit(Hitters)
Hitters <- mutate(Hitters, Salary = log10(Salary), Years = log(Years))



library(plotly)
Years <- seq(from = min(Hitters$Years), to = max(Hitters$Years), length = 100)
Hits <- seq(from = min(Hitters$Hits), to = max(Hitters$Hits), length = 100)
Salary <- matrix(predict(lm(Salary ~ Hits + Years, data = Hitters),
  newdata = data.frame(expand.grid(Years = Years, Hits = Hits, Salary = NA))
), ncol = length(Years))


#| fig-width: 6
#| fig-height: 4
#| fig-align: center
#| warning: false
#| message: false
plot_ly(
  x = ~Years, y = ~Hits, z = ~Salary, type = "surface", colors = "Reds",
  contours = list(
    x = list(show = TRUE, start = 0, end = 3, size = 0.3, color = "white"),
    y = list(show = TRUE, start = 0, end = 200, size = 25, color = "white")
  )
) %>%
  add_markers(x = ~ Hitters$Years, y = ~ Hitters$Hits, z = ~ Hitters$Salary, size = 0.1) %>%
  layout(
    showlegend = FALSE,
    scene = list(
      camera = list(
        eye = list(x = 0.9, y = -2, z = 0.3)
      )
    )
  )



library(leaps)
fit <- regsubsets(Salary ~ ., data = Hitters, method = "exhaustive", nbest = 1, nvmax = 20)
sum1 <- summary(fit)

plot(rowSums(sum1$which), sum1$cp)

fit <- regsubsets(Salary ~ ., data = Hitters, method = "backward", nbest = 1, nvmax = 20)
sum1 <- summary(fit)
# plot(rowSums(sum1$which), sum1$cp)
