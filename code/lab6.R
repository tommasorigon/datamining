# ----------------------------------------
# Title: LAB 6 (auto data, nonparametric regression)
# Author: Tommaso Rigon
# ----------------------------------------

rm(list = ls()) # Clean the environment

# The dataset can be downloaded here: https://tommasorigon.github.io/datamining/data/auto.txt
auto <- read.table("../data/auto.txt", header = TRUE)
auto <- subset(auto, select = c(city.distance, engine.size))

# Summary
str(auto)

y <- auto$city.distance # As the name suggests: city distance
x <- auto$engine.size # And engine size (L)

plot(x, y, xlab = "Engine size (L)", ylab = "City distance (km/L)", pch = 16, cex = 0.7)

# Set of points at which the curve is evaluated
newx <- data.frame(x = seq(min(x), max(x), length = 1000))

# p = 1 (linear regression)
lines(newx$x, predict(lm(y ~ x), newdata = newx), lty = 1, col = "black")

# p = 2 (parabolic regression)
lines(newx$x, predict(lm(y ~ x + I(x^2)), newdata = newx), lty = 2, col = "darkorange")

# p = 3 (cubic regression)
# Here I am using the more convenient syntax "poly"
lines(newx$x, predict(lm(y ~ poly(x, degree = 3)), newdata = newx), lty = 3, col = "darkblue")

# If I use an extreme value for the degree, it does not work well anymore
lines(newx$x, predict(lm(y ~ poly(x, degree = 10)), newdata = newx), lty = 6)

# Nadaraya-Watson estimator ---------------------------------------------

# Notice the bandwidth parametrization and the kernel used
?ksmooth

plot(x, y, xlab = "Engine size (L)", ylab = "City distance (km/L)", pch = 16, cex = 0.7)

h_param <- 1 # Let us try a few parameters here
band <- 4 * qnorm(0.75) * h_param # Bandwidth as parametrized in ksmooth
m_nw1 <- ksmooth(x, y, kernel = "normal", bandwidth = band, x.points = newx$x)
lines(m_nw1)

h_param <- 0.2 # Let us try a few parameters here
band <- 4 * qnorm(0.75) * h_param # Bandwidth as parametrized in ksmooth
m_nw2 <- ksmooth(x, y, kernel = "normal", bandwidth = band, x.points = newx$x)
lines(m_nw2, col = "darkorange")

h_param <- 0.1 # Let us try a few parameters here
band <- 4 * qnorm(0.75) * h_param # Bandwidth as parametrized in ksmooth
m_nw3 <- ksmooth(x, y, kernel = "normal", bandwidth = band, x.points = newx$x)
lines(m_nw3, col = "darkblue")

# Local polynomial regression (KernSmooth)---------------------------------------

# install.packages("KernSmooth")
library(KernSmooth)

?locpoly

plot(x, y, xlab = "Engine size (L)", ylab = "City distance (km/L)", pch = 16, cex = 0.7)

# Local linear regression (with a given bandwidth)
m_local_linear <- locpoly(x, y, degree = 1, bandwidth = 0.5, kernel = "normal", gridsize = 500)
lines(m_local_linear)

# Local quadratic regression
m_local_quadratic <- locpoly(x, y, degree = 2, bandwidth = 0.5, kernel = "normal", gridsize = 500)
lines(m_local_quadratic, lty = 2, col = "darkorange")

# Local cubic regression
m_local_cubic <- locpoly(x, y, degree = 3, bandwidth = 0.5, kernel = "normal", gridsize = 500)
lines(m_local_cubic, lty = 2, col = "darkblue")

# Local polynomial regression (sm package) ------------------------------------------

# install.packages("sm")
library(sm)

?sm.regression

sm.regression(x, y, h = 2, pch = 16, cex = 0.6)
sm.regression(x, y, h = 0.5, pch = 16, cex = 0.6, lty = 2, col = "darkorange")
sm.regression(x, y, h = 0.1, pch = 16, cex = 0.6, lty = 3, col = "darkblue")

?h.select
h.select(x, y, method = "cv", hstart = 0.05, hend = 3, ngrid = 50) # Cross-validation
h.select(x, y, method = "aicc") # Corrected AIC

# Note: h.select() is preferred over the older hcv(). The two are equivalent,
# but hcv() may be removed in future releases of the sm package.
hcv(x, y, display = "lines", hstart = 0.05, hend = 3, ngrid = 50)

sm.regression(x, y, h = 0.42, pch = 16, cex = 0.6, lty = 2)

# Loess -----------------------------------------------------------------------------

?loess.smooth
?loess

plot(x, y, xlab = "Engine size (L)", ylab = "City distance (km/L)", pch = 16, cex = 0.7)

# Estimate a loess model
m_loess1 <- loess.smooth(x, y, span = 0.5, degree = 1, evaluation = 1000, family = "symmetric")
lines(m_loess1)

# Over-smoothing
m_loess2 <- loess.smooth(x, y, span = 0.8, degree = 1, evaluation = 1000, family = "symmetric")
lines(m_loess2, col = "darkorange")

# Under-smoothing
m_loess3 <- loess.smooth(x, y, span = 0.33, degree = 1, evaluation = 1000, family = "symmetric")
lines(m_loess3, col = "darkblue")

# Regression splines ---------------------------------------------------

# install.packages("splines")
library(splines)

# Knots selection
xi <- seq(min(x), max(x), length = 4) # I select 4 knots in total, equally spaced
xi_int <- xi[2:(length(xi) - 1)] # There are actually K = 2 INTERNAL knots

# newx is redefined here to include the internal knot locations. This ensures the
# predicted curve is plotted correctly at the knots, where the spline may change slope.
newx <- data.frame(x = sort(c(seq(min(x), max(x), length = 1000), xi_int)))

# The intercept has been excluded, therefore there are K + 3 components (and not K + 4)
B <- bs(x, knots = xi_int, degree = 3, intercept = FALSE)

dim(B)
head(B)

plot(x, y, xlab = "Engine size (L)", ylab = "City distance (km/L)", pch = 16, cex = 0.7)

# Spline regression is just a linear model!
m_spl1 <- lm(y ~ bs(x, knots = xi_int, degree = 3, intercept = FALSE))
lines(newx$x, predict(m_spl1, newx), lty = 2, col = "darkorange")

# Vertical lines where the knots are placed
abline(v = xi[2], lty = 3)
abline(v = xi[3], lty = 3)

# Regression splines - Knots on the quantiles ----------------------

xi <- quantile(x, c(0, 0.333, 0.666, 1))
xi_int <- xi[2:(length(xi) - 1)] # There are K = 2 INTERNAL knots

m_spl2 <- lm(y ~ bs(x, knots = xi_int, degree = 3, intercept = FALSE))

plot(x, y, xlab = "Engine size (L)", ylab = "City distance (km/L)", pch = 16, cex = 0.7)
lines(newx$x, predict(m_spl2, newx), lty = 2, col = "darkblue")

# Plot vertical lines on the internal knots
abline(v = xi[2], lty = 3)
abline(v = xi[3], lty = 3)

# Regression splines - Degrees of freedom specification ---------------

# Basis function of a B-spline (degree = 3, cubic splines)
# The following relationship holds: df = length(internal knots) + degree
# When intercept = FALSE, df = K + degree (here K = number of internal knots).
# Knots are chosen using quantiles of the x distribution.

plot(x, y, xlab = "Engine size (L)", ylab = "City distance (km/L)", pch = 16, cex = 0.7)

# This is equivalent to the previous command (K = 2 internal knots, degree = 3, df = 5)
m_spl2 <- lm(y ~ bs(x, df = 5, degree = 3, intercept = FALSE))
plot(x, y, xlab = "Engine size (L)", ylab = "City distance (km/L)", pch = 16, cex = 0.7)
lines(newx$x, predict(m_spl2, newx), lty = 1, col = "black")

# Let us change a bit the degrees of freedom
m_spl3 <- lm(y ~ bs(x, df = 10, degree = 3, intercept = FALSE))
lines(newx$x, predict(m_spl3, newx), lty = 2, col = "darkorange")

# This is, quite evidently, overfitting the data
m_spl4 <- lm(y ~ bs(x, df = 15, degree = 3, intercept = FALSE))
lines(newx$x, predict(m_spl4, newx), lty = 3, col = "darkblue")


# Smoothing Splines ------------------------------------------------

plot(x, y, xlab = "Engine size (L)", ylab = "City distance (km/L)", pch = 16, cex = 0.7)

?smooth.spline
m_smooth <- smooth.spline(x, y)
m_smooth

# The default uses GCV to select lambda, which here produces a very smooth fit.
# Inspect m_smooth$lambda and m_smooth$df to see the selected values.
lines(m_smooth)

# Let us try some alternative values of lambda (smaller = less smooth)
plot(x, y, xlab = "Engine size (L)", ylab = "City distance (km/L)", pch = 16, cex = 0.7)

m_smooth1 <- smooth.spline(x, y, lambda = 0.0001)
lines(predict(m_smooth1, x = newx$x), lty = 1, col = "black")

m_smooth2 <- smooth.spline(x, y, lambda = 0.001)
lines(predict(m_smooth2, x = newx$x), lty = 2, col = "darkorange")

m_smooth3 <- smooth.spline(x, y, lambda = 0.01)
lines(predict(m_smooth3, x = newx$x), lty = 2, col = "darkblue")

# Let us use "spar" instead of lambda (spar is a standardized version of lambda)
plot(x, y, xlab = "Engine size (L)", ylab = "City distance (km/L)", pch = 16, cex = 0.7)

m_smooth <- smooth.spline(x, y, spar = 0.8)
lines(predict(m_smooth, x = newx$x))