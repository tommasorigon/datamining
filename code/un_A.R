#| warning: false
#| message: false
#| fig-width: 5
#| fig-height: 4.5
library(tidyverse)
library(broom)
library(knitr)
library(ggplot2)
library(GGally)
library(ggthemes)

rm(list = ls())
# The dataset can be also downloaded here: https://tommasorigon.github.io/datamining/data/auto.txt
auto <- read.table("../data/auto.txt", header = TRUE) %>% select(city.distance, engine.size, n.cylinders, curb.weight, fuel)

p0 <- ggpairs(auto,
  columns = 1:4, aes(colour = fuel),
  lower = list(continuous = wrap("points", size = 0.9)),
  upper = list(continuous = wrap("points", size = 0.9)),
  diag = "blank"
) +
  theme_light() +
  scale_color_tableau(palette = "Color Blind") +
  xlab("") +
  ylab("")
p0


#| fig-width: 4
#| fig-height: 3.7
ggplot(data = auto, aes(x = engine.size, y = city.distance, col = fuel)) +
  geom_point() +
  theme_light() +
  scale_color_tableau(palette = "Color Blind") +
  theme(legend.position = "top") +
  xlab("Engine size (L)") +
  ylab("Urban distance (km/L)")



m1 <- lm(city.distance ~ engine.size + I(engine.size^2) + I(engine.size^3) + fuel, data = auto)
kable(tidy(m1, conf.int = FALSE), digits = 3)



kable(glance(m1)[c(1, 3, 10)])


#| fig-width: 7.8
#| fig-height: 4.55
#| fig-align: center
augmented_m1 <- augment(m1)
ggplot(data = augmented_m1, aes(x = engine.size, y = city.distance, col = fuel)) +
  geom_point() +
  geom_line(aes(y = .fitted)) +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Engine size (L)") +
  ylab("Urban distance (km/L)")


#| fig-width: 7.8
#| fig-height: 4.55
#| fig-align: center
ggplot(data = augmented_m1, aes(x = .fitted, y = .resid, col = fuel)) +
  geom_point() +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Fitted values") +
  ylab("Residuals")



m2 <- lm(log(city.distance) ~ I(log(engine.size)) + fuel, data = auto)
kable(tidy(m2, conf.int = FALSE), digits = 3)


#| fig-width: 7.8
#| fig-height: 4.55
#| fig-align: center
augmented_m2 <- augment(m2, data = auto)
ggplot(data = augmented_m2, aes(x = engine.size, y = city.distance, col = fuel)) +
  geom_point() +
  geom_line(aes(y = exp(.fitted))) +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Engine size (L)") +
  ylab("Urban distance (km/L)")


#| fig-width: 7.8
#| fig-height: 4.55
#| fig-align: center
ggplot(data = augmented_m2, aes(x = .fitted, y = .resid, col = fuel)) +
  geom_point() +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Fitted values") +
  ylab("Residuals")



r.squared.original <- 1 - sum(mean((auto$city.distance - exp(predict(m2)))^2)) / sum(mean((auto$city.distance - mean(auto$city.distance))^2))
kable(data.frame(r.squared.original = r.squared.original, glance(m2)[c(1, 3, 10)]))



auto$cylinders2 <- factor(auto$n.cylinders == 2)
m3 <- lm(log(city.distance) ~ I(log(engine.size)) + I(log(curb.weight)) + fuel + cylinders2, data = auto)
kable(tidy(m3, conf.int = FALSE), digits = 3)


#| fig-width: 7.8
#| fig-height: 4.55
#| fig-align: center
augmented_m3 <- augment(m3, data = auto)
ggplot(data = augmented_m3, aes(x = .fitted, y = .resid, col = fuel)) +
  geom_point() +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Fitted values") +
  ylab("Residuals")



r.squared.original <- 1 - sum(mean((auto$city.distance - exp(predict(m3)))^2)) / sum(mean((auto$city.distance - mean(auto$city.distance))^2))
kable(data.frame(r.squared.original = r.squared.original, glance(m3)[c(1, 3, 10)]))


#| fig-width: 5
#| fig-height: 4.7
rm(list = ls())
# The dataset can be also downloaded here: https://tommasorigon.github.io/datamining/data/heart.txt
heart <- read.table("../data/heart.txt", header = TRUE, sep = ",", row.names = 1) %>% select(-c(adiposity, typea))
heart$chd <- factor(heart$chd)

ggplot(data = heart, aes(x = ldl, y = age, col = chd)) +
  geom_point() +
  theme_light() +
  scale_color_tableau(palette = "Color Blind") +
  theme(legend.position = "top") +
  xlab("Cumulative tobacco (kg)") +
  ylab("Low density lipoprotein cholesterol")


#| warning: false
#| message: false
#| fig-width: 11
#| fig-height: 6
#| fig-align: center

p0 <- ggpairs(heart,
  columns = c(1:3, 5:7), aes(colour = chd),
  lower = list(continuous = wrap("points", size = 0.2)),
  upper = list(continuous = wrap("points", size = 0.2)),
  diag = "blank"
) +
  theme_light() +
  scale_color_tableau(palette = "Color Blind") +
  xlab("") +
  ylab("")
p0



m1 <- glm(chd ~ ., data = heart, family = "binomial")
kable(tidy(m1, conf.int = FALSE), digits = 3)



p_hat <- predict(m1, type = "response")
y_hat <- p_hat > 0.5
tab <- addmargins(table(y_hat, heart$chd))
rownames(tab) <- c("Predicted 0", "Predicted 1", "Predicted total")
colnames(tab) <- c("Actual 0", "Actual 1", "Actual total")
kable(tab)


#| message: false
#| #| fig-width: 7.8
#| fig-height: 6.0
#| fig-align: center
library(pROC)
roc_heart <- roc(response = heart$chd, predictor = p_hat)
p <- ggroc(roc_heart, legacy.axes = TRUE) + ggtitle(paste("Receiver Operating Characteristic Curve (ROC) - AUC: ", round(auc(roc_heart), 2))) + ylab("Sensitivity") + xlab("1 - specificity") + geom_segment(
  aes(x = 0, xend = 1, y = 0, yend = 1),
  color = "grey", linetype = "dashed"
) + theme_bw() + geom_vline(aes(xintercept = 1 - 255 / (255 + 47)), linetype = "dotted") + geom_vline(aes(xintercept = 0.5), linetype = "dotted")

p
