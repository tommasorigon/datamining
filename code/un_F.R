rm(list = ls())
library(ggplot2)
library(ggthemes)
library(sm)

data(trawl)

trawl <- na.omit(trawl)
trawl$Year <- factor(trawl$Year)
levels(trawl$Year) <- c("1992", "1993")
trawl$Zone <- factor(trawl$Zone)
levels(trawl$Zone) <- c("Open", "Closed")
# Data splitting
set.seed(1234)
id_train <- sample(1:nrow(trawl), size = floor(0.80 * nrow(trawl)), replace = FALSE)
id_test <- setdiff(1:nrow(trawl), id_train)
trawl_train <- trawl[id_train, ]
trawl_test <- trawl[id_test, ]

library(broom)
m_linear <- lm(Score1 ~ Latitude + Longitude + Depth + Zone + Year, data = trawl_train)
knitr::kable(tidy(summary(m_linear)), digits = 3)

library(ggthemes)
ggplot(data = trawl_train, aes(x = Longitude, y = Score1, col = Year)) +
  geom_point(size = 1) +
  geom_smooth(se = FALSE, span = 0.4, col = "#a3acb9", linetype = "dashed") +
  scale_color_tableau(palette = "Color Blind") +
  theme_minimal() +
  theme(legend.position = "top") +
  xlab("Longitude of the sampling position") +
  ylab("Catch score")

library(mgcv)
m_gam <- gam(Score1 ~ s(Longitude, bs = "tp") + s(Latitude, bs = "tp") + s(Depth, bs = "tp") + Zone + Year,
  data = trawl_train, method = "REML"
)
knitr::kable(tidy(m_gam, parametric = TRUE), digits = 3)
knitr::kable(tidy(m_gam, parametric = FALSE), digits = 3)

library(gratia)
data_plot <- smooth_estimates(m_gam, smooth = "s(Longitude)")

ggplot(data = data_plot, aes(x = Longitude, y = .estimate)) +
  geom_line(linewidth = 1, col = "#3232AA") +
  geom_point(data = add_partial_residuals(m_gam, data = trawl_train), aes(x = Longitude, y = `s(Longitude)`), size = 0.7) +
  theme_minimal() +
  xlab("Longitude of the sampling position") +
  ylab("Partial effect")

data_plot <- smooth_estimates(m_gam, smooth = "s(Latitude)")

ggplot(data = data_plot, aes(x = Latitude, y = .estimate)) +
  geom_line(linewidth = 1, col = "#3232AA") +
  geom_point(data = add_partial_residuals(m_gam, data = trawl_train), aes(x = Latitude, y = `s(Latitude)`), size = 0.7) +
  theme_minimal() +
  xlab("Latitude of the sampling position") +
  ylab("Partial effect")

data_plot <- smooth_estimates(m_gam, smooth = "s(Depth)")

ggplot(data = data_plot, aes(x = Depth, y = .estimate)) +
  geom_line(linewidth = 1, col = "#3232AA") +
  geom_point(data = add_partial_residuals(m_gam, data = trawl_train), aes(x = Depth, y = `s(Depth)`), size = 0.7) +
  theme_minimal() +
  xlab("Depth of the sampling position") +
  ylab("Partial effect")

y_test <- trawl_test$Score1

y_null <- mean(trawl_test$Score1)
y_hat_linear <- predict(m_linear, newdata = trawl_test)
y_hat_gam <- predict(m_gam, newdata = trawl_test)
# y_hat_mars_deg1 <- predict(m_mars_deg1, newdata = trawl_test)
# y_hat_mars_deg2 <- predict(m_mars_deg2, newdata = trawl_test)

tab_results <- rbind(
  c(
    mean(abs(y_test - y_null)),
    mean(abs(y_test - y_hat_linear)),
    mean(abs(y_test - y_hat_gam)) # ,
    #    mean(abs(y_test - y_hat_mars_deg1)),
    #    mean(abs(y_test - y_hat_mars_deg2))
  ),
  sqrt(c(
    mean(abs(y_test - y_null)^2),
    mean(abs(y_test - y_hat_linear)^2),
    mean(abs(y_test - y_hat_gam)^2) # ,
    #    mean(abs(y_test - y_hat_mars_deg1)^2),
    #    mean(abs(y_test - y_hat_mars_deg2)^2)
  ))
)
colnames(tab_results) <- c("Null model", "Linear model", "GAM")
rownames(tab_results) <- c("MAE", "RMSE")
knitr::kable(tab_results, digits = 3)
