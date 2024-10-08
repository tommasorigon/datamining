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
  geom_line(linewidth = 1, col = "#1170aa") +
  geom_point(data = add_partial_residuals(m_gam, data = trawl_train), aes(x = Longitude, y = `s(Longitude)`), size = 0.7) +
  theme_minimal() +
  xlab("Longitude of the sampling position") +
  ylab("Partial effect")

data_plot <- smooth_estimates(m_gam, smooth = "s(Latitude)")

ggplot(data = data_plot, aes(x = Latitude, y = .estimate)) +
  geom_line(linewidth = 1, col = "#1170aa") +
  geom_point(data = add_partial_residuals(m_gam, data = trawl_train), aes(x = Latitude, y = `s(Latitude)`), size = 0.7) +
  theme_minimal() +
  xlab("Latitude of the sampling position") +
  ylab("Partial effect")

data_plot <- smooth_estimates(m_gam, smooth = "s(Depth)")

ggplot(data = data_plot, aes(x = Depth, y = .estimate)) +
  geom_line(linewidth = 1, col = "#1170aa") +
  geom_point(data = add_partial_residuals(m_gam, data = trawl_train), aes(x = Depth, y = `s(Depth)`), size = 0.7) +
  theme_minimal() +
  xlab("Depth of the sampling position") +
  ylab("Partial effect")

x_seq <- seq(from = 0, to = 1, length = 200)
data_plot <- data.frame(
  x = x_seq,
  y = c(pmax(0, x_seq - 0.5), pmax(0, 0.5 - x_seq)),
  basis = rep(c("Basis 1", "Basis 2"), each = length(x_seq))
)
ggplot(data = data_plot, aes(x = x, y = y, col = basis, linetype = basis)) +
  geom_line() +
  scale_color_tableau(palette = "Color Blind") +
  theme_minimal() +
  geom_vline(xintercept = 0.5, linetype = "dotted") +
  theme(legend.position = "none") +
  xlab("x") +
  ylab("Basis function")

library(earth)
m_mars_deg1 <- earth(Score1 ~ Zone + Year + Latitude + Longitude + Depth, data = trawl_train, degree = 1, pmethod = "exhaustive", penalty = 3, nk = 21)
summary(m_mars_deg1, style = "pmax")
# plotmo(m_mars_deg1)

m_mars_deg2 <- earth(Score1 ~ Zone + Year + Latitude + Longitude + Depth,
  data = trawl_train, degree = 2,
  pmethod = "exhaustive", penalty = 4, trace = TRUE, nk = 21
)
summary(m_mars_deg2)
# plotmo(m_mars_deg2)

library(pdp)
partial_linear <- partial(m_linear, pred.var = c("Longitude", "Year"), grid.resolution = 40)
partial_gam <- partial(m_gam, pred.var = c("Longitude", "Year"), grid.resolution = 40)
partial_mars_deg1 <- partial(m_mars_deg1, pred.var = c("Longitude", "Year"), grid.resolution = 40)
partial_mars_deg2 <- partial(m_mars_deg2, pred.var = c("Longitude", "Year"), grid.resolution = 40)

ggplot(data = trawl_train, aes(x = Longitude, y = Score1, col = Year)) +
  geom_point(size = 1) +
  facet_grid(. ~ Year) +
  geom_line(data = partial_linear, aes(x = Longitude, y = yhat)) +
  scale_color_tableau(palette = "Color Blind") +
  theme_light() +
  theme(legend.position = "none") +
  xlab("Longitude of the sampling position") +
  ylab("Catch score")

ggplot(data = trawl_train, aes(x = Longitude, y = Score1, col = Year)) +
  geom_point(size = 1) +
  facet_grid(. ~ Year) +
  geom_line(data = partial_gam, aes(x = Longitude, y = yhat)) +
  scale_color_tableau(palette = "Color Blind") +
  theme_light() +
  theme(legend.position = "none") +
  xlab("Longitude of the sampling position") +
  ylab("Catch score")

ggplot(data = trawl_train, aes(x = Longitude, y = Score1, col = Year)) +
  geom_point(size = 1) +
  facet_grid(. ~ Year) +
  geom_line(data = partial_mars_deg1, aes(x = Longitude, y = yhat)) +
  scale_color_tableau(palette = "Color Blind") +
  theme_light() +
  theme(legend.position = "none") +
  xlab("Longitude of the sampling position") +
  ylab("Catch score")

ggplot(data = trawl_train, aes(x = Longitude, y = Score1, col = Year)) +
  geom_point(size = 1) +
  facet_grid(. ~ Year) +
  geom_line(data = partial_mars_deg2, aes(x = Longitude, y = yhat)) +
  scale_color_tableau(palette = "Color Blind") +
  theme_light() +
  theme(legend.position = "none") +
  xlab("Longitude of the sampling position") +
  ylab("Catch score")

y_test <- trawl_test$Score1

y_null <- mean(trawl_test$Score1)
y_hat_linear <- predict(m_linear, newdata = trawl_test)
y_hat_gam <- predict(m_gam, newdata = trawl_test)
y_hat_mars_deg1 <- predict(m_mars_deg1, newdata = trawl_test)
y_hat_mars_deg2 <- predict(m_mars_deg2, newdata = trawl_test)

tab_results <- rbind(
  c(
    mean(abs(y_test - y_null)),
    mean(abs(y_test - y_hat_linear)),
    mean(abs(y_test - y_hat_gam)),
    mean(abs(y_test - y_hat_mars_deg1)),
    mean(abs(y_test - y_hat_mars_deg2))
  ),
  sqrt(c(
    mean(abs(y_test - y_null)^2),
    mean(abs(y_test - y_hat_linear)^2),
    mean(abs(y_test - y_hat_gam)^2),
    mean(abs(y_test - y_hat_mars_deg1)^2),
    mean(abs(y_test - y_hat_mars_deg2)^2)
  ))
)
colnames(tab_results) <- c("Null model", "Linear model", "GAM", "MARS (degree 1)", "MARS (degree 2)")
rownames(tab_results) <- c("MAE", "RMSE")
knitr::kable(tab_results, digits = 3)
