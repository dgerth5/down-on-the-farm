# Load necessary libraries
library(splines)
library(mgcv)
library(ggplot2)

# Create sample data
set.seed(123)
x <- seq(from = 0, to = 10, length.out = 100)
y <- sin(x) + rnorm(100, sd = 0.5)

# Natural spline model from the splines package
df <- 4  # Degrees of freedom for splines
x_spline <- ns(x, df = df)
model_splines <- lm(y ~ x_spline)

# Predictions for natural spline model
x_pred <- seq(min(x), max(x), length.out = 100)
x_pred_spline <- ns(x_pred, df = df, Boundary.knots = range(x))
y_pred_splines <- predict(model_splines, newdata = list(x_spline = x_pred_spline))

# GAM model from the mgcv package
model_gam <- gam(y ~ s(x, k = df + 1))  # k is number of basis functions, roughly corresponds to df+1
y_pred_gam <- predict(model_gam, newdata = list(x = x_pred))

# Plotting both fits for comparison
df_plot <- data.frame(x = c(x_pred, x_pred), 
                      y = c(y_pred_splines, y_pred_gam), 
                      model = factor(rep(c("Natural Splines", "GAM"), each = 100)))

ggplot(df_plot, aes(x = x, y = y, color = model)) +
  geom_line() +
  geom_point(data = data.frame(x = x, y = y), aes(x = x, y = y), color = "black", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Comparison of Natural Splines and GAM", x = "x", y = "y", color = "Model")
