# Load the survival package
library(survival)

set.seed(123)  
data <- data.frame(
  hadtj = rbinom(200, size = 1, prob = 0.1),  # 10% probability of having surgery
  avg_velocity = rnorm(200, mean = 90, sd = 3),  # normal distribution around 90 mph
  pitch_count = rpois(200, lambda = 110)  # Poisson distribution around 110 pitches
)

# Assign days since season started, random for those who had surgery, 0 otherwise
data$days_since_season_started <- ifelse(data$hadtj == 1,
                                         sample(30:150, sum(data$hadtj), replace = TRUE),
                                         200)  # 0 for no surgery

# Create a survival object
surv_obj <- Surv(time = data$days_since_season_started, event = data$hadtj, type = "right")
head(surv_obj)


# Create model
cox_model <- coxph(surv_obj ~ avg_velocity*pitch_count, data = data)
summary(cox_model)

final_surve <- predict(cox_model, data, type = "survival")
head(final_surve)
predicted_survival <- survfit(cox_model, newdata = data[1:6,])

# Plot predicted survival curves
plot(predicted_survival, col = 1:6, 
     lty = 1, xlab = "Days", ylab = "Survival Probability", main = "Predicted Survival Curves for New Pitchers",
     ylim = c(0.8,1))
legend("bottomleft", legend = paste("Pitcher", 24:30), col = 24:30, lty = 1)
