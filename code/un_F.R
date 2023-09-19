#| message: false
rm(list = ls())
library(ISLR)
library(ggplot2)
library(ggthemes)

data(Hitters)
Hitters <- na.omit(Hitters)
dplyr::glimpse(Hitters)



# Hitters <- mutate(Hitters,
#   TotalScore = (RBI + Assists + Walks - Errors) / AtBat,
#   logAHits = log1p(CHits / Years),
#   logAAtBat = log1p(CAtBat / Years),
#   logARuns = log1p(CRuns / Years),
#   logARBI = log1p(CRBI / Years),
#   logAWalks = log1p(CWalks / Years),
#   logAHmRun = log1p(CHmRun / Years),
#   RatioHits = Hits / CHits,
#   RatioAtBat = AtBat / CAtBat,
#   RatioRuns = Runs / CRuns,
#   RatioRBI = RBI / CRBI,
#   RatioWalks = Walks / CWalks,
#   RatioHmRun = HmRun / (CHmRun + 1),
#   logYears = log(Years),
#   logSalary = log(Salary)
# ) %>% select(-c(Salary, Years))



Hitters$logSalary <- log(Hitters$Salary)
Hitters <- subset(Hitters, select = -Salary)

# Data splitting
set.seed(123)
id_train <- sample(1:nrow(Hitters), size = floor(0.75 * nrow(Hitters)), replace = FALSE)
id_test <- setdiff(1:nrow(Hitters), id_train)
Hitters_train <- Hitters[id_train, ]
Hitters_test <- Hitters[id_test, ]


#| fig-width: 18
#| fig-height: 8.5
#| fig-align: center
library(ggcorrplot)
corr <- cor(subset(Hitters_train, select = -c(logSalary, Division, League, NewLeague))) # Remove logSalary
ggcorrplot(corr,
  hc.order = TRUE,
  outline.col = "white",
  ggtheme = ggplot2::theme_bw,
  colors = c("#fc7d0b", "white", "#1170aa")
)


#| fig-width: 12
#| fig-height: 6
#| fig-align: center
library(leaps)
fit.bests <- regsubsets(logSalary ~ ., data = Hitters_train, nvmax = ncol(Hitters_train))
summary.bests <- summary(fit.bests)

par(mfrow = c(1, 2))
plot(summary.bests$cp, xlab = "Number of covariates", ylab = "Mallow's Cp", type = "b", pch = 16)
plot(fit.bests, scale = "Cp")


#| output: false
library(broom)
m_linear <- lm(logSalary ~ Hits + Walks + Years + CRuns + CWalks + League + Division + Errors, data = Hitters_train)
knitr::kable(tidy(summary(m_linear)), digits = 3)


#| fig-width: 7.8
#| fig-height: 4
#| fig-align: center
#| message: false
ggplot(data = Hitters_train, aes(x = Years, y = logSalary)) +
  geom_point(size = 0.7) +
  geom_smooth(se = FALSE, span = 1, col = "#1170aa") +
  theme_minimal() +
  xlab("Number of years in the major leagues (Years)") +
  ylab("log(Salary)")



library(mgcv)
m_gam <- gam(logSalary ~ s(AtBat) + s(Hits) + s(HmRun) + s(Runs) + s(RBI) + s(Walks) + s(Years) + s(CAtBat) + s(CHits) + s(CHmRun) + s(CRuns) + s(CRBI) + s(CWalks) + League + Division + s(PutOuts) + s(Assists) + s(Errors) + NewLeague, data = Hitters_train, select = FALSE)



summary(m_gam)



m_gam_selected <- gam(logSalary ~ s(AtBat) + s(Hits) + s(HmRun) + s(Runs) + s(RBI) + s(Walks) + s(Years) + s(CAtBat) + s(CHits) + s(CHmRun) + s(CRuns) + s(CRBI) + s(CWalks) + League + Division + s(PutOuts) + s(Assists) + s(Errors) + NewLeague,
  data = Hitters_train, select = TRUE
)

summary(m_gam_selected)


#| fig-width: 7.8
#| fig-height: 4
#| fig-align: center
#| message: false
library(gratia)
data_plot <- smooth_estimates(m_gam_selected, smooth = "s(Years)")

ggplot(data = data_plot, aes(x = Years, y = est)) +
  geom_line(linewidth = 1, col = "#1170aa") +
  geom_point(data = add_partial_residuals(m_gam_selected, data = Hitters_train), aes(x = Years, y = `s(Years)`), size = 0.7) +
  theme_minimal() +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Number of years in the major leagues (Years)") +
  ylab("Partial effect")


#| fig-width: 7.8
#| fig-height: 4
#| fig-align: center
#| message: false
data_plot <- smooth_estimates(m_gam_selected, smooth = "s(CHits)")

ggplot(data = data_plot, aes(x = CHits, y = est)) +
  geom_line(linewidth = 1, col = "#1170aa") +
  geom_point(data = add_partial_residuals(m_gam_selected, data = Hitters_train), aes(x = CHits, y = `s(CHits)`), size = 0.7) +
  theme_minimal() +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Number of years in the major leagues (Years)") +
  ylab("Partial effect")



library(earth)
fit_mars_deg1 <- earth(logSalary ~ ., data = Hitters_train, degree = 1, pmethod = "cv", nfold = 10)
summary(fit_mars_deg1, style = "pmax")
plot(evimp(fit_mars_deg1))
plotmo(fit_mars_deg1)



fit_mars_deg2 <- earth(logSalary ~ ., data = Hitters_train, degree = 2, pmethod = "cv", nfold = 10)
summary(fit_mars_deg2, style = "pmax")
plot(evimp(fit_mars_deg2))
plotmo(fit_mars_deg2)



library(pdp)
earth_partial <- partial(fit_mars_deg2, pred.var = c("Years", "CHits"), grid.resolution = 40)
autoplot(earth_partial) + theme_light()



library(pdp)
gam_partial <- partial(m_gam_selected, pred.var = c("Years", "CHits"), grid.resolution = 40)
autoplot(gam_partial) + theme_light()



y_test <- exp(Hitters_test$logSalary)

y_hat_linear <- exp(predict(m_linear, newdata = Hitters_test))
y_hat_gam <- exp(predict(m_gam, newdata = Hitters_test))
y_hat_gam_selected <- exp(predict(m_gam_selected, newdata = Hitters_test))
y_mars_deg1 <- exp(predict(fit_mars_deg1, newdata = Hitters_test))
y_mars_deg2 <- exp(predict(fit_mars_deg2, newdata = Hitters_test))

tab_results <- rbind(
  c(
    mean(abs(y_test - y_hat_linear)),
    mean(abs(y_test - y_hat_gam)),
    mean(abs(y_test - y_hat_gam_selected)),
    mean(abs(y_test - y_mars_deg1)),
    mean(abs(y_test - y_mars_deg2))
  ),
  sqrt(c(
    mean(abs(y_test - y_hat_linear)^2),
    mean(abs(y_test - y_hat_gam)^2),
    mean(abs(y_test - y_hat_gam_selected)^2),
    mean(abs(y_test - y_mars_deg1)^2),
    mean(abs(y_test - y_mars_deg2)^2)
  ))
)
colnames(tab_results) <- c("Best subset", "GAM", "GAM (selected)", "MARS (degree 1)", "MARS (degree 2)")
rownames(tab_results) <- c("Test MAE", "Test RMSE")
knitr::kable(tab_results, digits = 3)
