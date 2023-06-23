## ----r------------------------------------------------------------------------
knitr::purl("un_A1.qmd", output = "../code/un_A1.R")
styler:::style_file("../code/un_A1.R")


## ----r------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(GGally)
library(ggthemes)

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


## ----r------------------------------------------------------------------------
ggplot(data = auto, aes(x = engine.size, y = city.distance, col = fuel)) +
  geom_point() +
  theme_light() +
  scale_color_tableau(palette = "Color Blind") +
  theme(legend.position = "top") +
  xlab("Engine size (L)") +
  ylab("Urban distance (km/L)")


## ----r------------------------------------------------------------------------
library(broom)
library(knitr)
m1 <- lm(city.distance ~ engine.size + I(engine.size^2) + I(engine.size^3) + fuel, data = auto)
kable(tidy(m1, conf.int = FALSE), digits = 3)


## ----r------------------------------------------------------------------------
kable(glance(m1)[c(1, 3, 10)])


## ----r------------------------------------------------------------------------
augmented_m1 <- augment(m1)
ggplot(data = augmented_m1, aes(x = engine.size, y = city.distance, col = fuel)) +
  geom_point() +
  geom_line(aes(y = .fitted)) +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Engine size (L)") +
  ylab("Urban distance (km/L)")


## ----r------------------------------------------------------------------------
ggplot(data = augmented_m1, aes(x = .fitted, y = .resid, col = fuel)) +
  geom_point() +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Fitted values") +
  ylab("Residuals")


## ----r------------------------------------------------------------------------
m2 <- lm(log(city.distance) ~ I(log(engine.size)) + fuel, data = auto)
kable(tidy(m2, conf.int = FALSE), digits = 3)


## ----r------------------------------------------------------------------------
augmented_m2 <- augment(m2, data = auto)
ggplot(data = augmented_m2, aes(x = engine.size, y = city.distance, col = fuel)) +
  geom_point() +
  geom_line(aes(y = exp(.fitted))) +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Engine size (L)") +
  ylab("Urban distance (km/L)")


## ----r------------------------------------------------------------------------
ggplot(data = augmented_m2, aes(x = .fitted, y = .resid, col = fuel)) +
  geom_point() +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Fitted values") +
  ylab("Residuals")


## ----r------------------------------------------------------------------------
r.squared.original <- 1 - sum(mean((auto$city.distance - exp(predict(m2)))^2)) / sum(mean((auto$city.distance - mean(auto$city.distance))^2))
kable(data.frame(r.squared.original = r.squared.original, glance(m2)[c(1, 3, 10)]))


## ----r------------------------------------------------------------------------
auto$cylinders2 <- factor(auto$n.cylinders == 2)
m3 <- lm(log(city.distance) ~ I(log(engine.size)) + I(log(curb.weight)) + fuel + cylinders2, data = auto)
kable(tidy(m3, conf.int = FALSE), digits = 3)


## ----r------------------------------------------------------------------------
augmented_m3 <- augment(m3, data = auto)
ggplot(data = augmented_m3, aes(x = .fitted, y = .resid, col = fuel)) +
  geom_point() +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Fitted values") +
  ylab("Residuals")


## ----r------------------------------------------------------------------------
r.squared.original <- 1 - sum(mean((auto$city.distance - exp(predict(m3)))^2)) / sum(mean((auto$city.distance - mean(auto$city.distance))^2))
kable(data.frame(r.squared.original = r.squared.original, glance(m3)[c(1, 3, 10)]))
