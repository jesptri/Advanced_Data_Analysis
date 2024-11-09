library(readxl)

mcq_exam <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced data analysis/Test 1 Data.xlsx")

# print(mcq_exam)

y <- c(mcq_exam$y)
x1 <- c(mcq_exam$x1)
x2 <- c(mcq_exam$x2)
x5 <- c(mcq_exam$x5)

LnY <- sqrt(y)

# Fit the MLR model with LnY as the dependent variable
model <- lm(LnY ~ x1 + x2 + x5, data = mcq_exam)

predicted_values <- predict(model)

# Calculate residuals
residuals <- resid(model)

# Standardize the residuals
standardized_residuals <- rstandard(model)

# Create the scatterplot of predicted values vs standardized residuals
plot(predicted_values, standardized_residuals, 
     xlab = "Predicted Values", 
     ylab = "Standardized Residuals", 
     main = "Predicted Values vs Standardized Residuals",
     pch = 19, col = "blue")

# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red", lwd = 2)