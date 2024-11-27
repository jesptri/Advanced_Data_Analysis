library(readxl)
bike_data <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced_data_analysis/Assignment/city_bike_use.xlsx")
bike_data$random_number <- runif(nrow(bike_data))

library(dplyr)
sorted_bike_data <- bike_data %>% arrange(random_number)
sample_bike_data <- sorted_bike_data[1:2000, ]

Bikes <- c(sample_bike_data$Bikes)
Season <- factor(sample_bike_data$season, levels=c(0,1,2,3), labels=c("Winter", "Spring", "Summer", "Autumn"))
Month <- c(sample_bike_data$Month)
Hour <- c(sample_bike_data$Hour)
Rain <- factor(sample_bike_data$Rain, levels=c(0,1), labels=c("No", "Yes"))
t1 <- c(sample_bike_data$t1)
t2 <- c(sample_bike_data$t2)
hum <- c(sample_bike_data$hum)
wind_speed <- c(sample_bike_data$wind_speed)
is_holiday <- factor(sample_bike_data$is_holiday, levels=c(0,1), labels=c("No", "Yes"))
is_weekend <- factor(sample_bike_data$is_weekend, levels=c(0,1), labels=c("No", "Yes"))

print(sample_bike_data)


##### Q.1 #####


library(ggplot2)

### Seasons ###
ggplot(bike_data, aes(x = factor(season), y = Bikes)) +
  geom_boxplot(fill = "red") +
  labs(title = "Bikes Hired vs Season", x = "Season", y = "Number of Bikes Hired") +
  theme_minimal()

### Hour ###
ggplot(bike_data, aes(x = factor(Hour), y = Bikes)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Bikes Hired vs Hour", x = "Hour", y = "Number of Bikes Hired") +
  theme_minimal()

### Rain ###
ggplot(bike_data, aes(x = factor(Rain), y = Bikes)) +
  geom_boxplot(fill = "green") +
  labs(title = "Bikes Hired vs Rain", x = "Rain", y = "Number of Bikes Hired") +
  theme_minimal()

### t1 ###
ggplot(bike_data, aes(x = t1, y = Bikes)) +
  geom_point(alpha = 0.5, color = "red") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Bikes Hired vs Temperature (t1)", x = "Temperature (t1)", y = "Number of Bikes Hired") +
  theme_minimal()





##### Q.2 #####





# initial model #
model_1 <- lm(Bikes ~ Season + Month + Hour + Rain + t1 + t2 + hum + wind_speed + is_holiday + is_weekend, data=sample_bike_data)
summary(model_1)

### MULTICOLLINEARITY ###

# correlation matrix #
cor_matrix <- cor(bike_data[, sapply(bike_data, is.numeric)], use = "complete.obs")
print(cor_matrix)
# t1 and t2 are very correlated
# VIF #
library(car)
vif(model_1)
# I remove t2 as the VIF is very high

# new model without t2 #
model_2 <- lm(Bikes ~ Season + Month + Hour + Rain + t1 + hum + wind_speed + is_holiday + is_weekend, data=sample_bike_data)
summary(model_2)

anova(model_1, model_2) # partial F-test #

# new model without wind_speed and without Month #
model_3 <- lm(Bikes ~ Season + Hour + Rain + t1 + hum + is_holiday + is_weekend, data=sample_bike_data)
summary(model_3)
anova(model_2,model_3) 

# new model without Rain #
model_4 <- lm(Bikes ~ Season + Hour + t1 + hum + is_holiday + is_weekend, data=sample_bike_data)
summary(model_4)
anova(model_3,model_4) 

# plot of the residuals #
z_res <- rstandard(model_4)
hist(z_res)
# plot(model_4)

### TRANSFORMATIONS ###

# new model with log transformation #
model_5 <- lm(log(Bikes) ~ Season + Hour + t1 + hum + is_holiday + is_weekend, data=sample_bike_data)
summary(model_5)
# plot of the residuals after the log transformation #
z_res <- rstandard(model_5)
hist(z_res)
plot(model_5)

## Final model ##

model_6 <- lm(log(Bikes) ~ Hour + t1 + hum, data=sample_bike_data) # I removed Season from the model to see the difference
anova(model_5, model_6)
summary(model_6)

plot(model_6)

# Cook's D #

influential_points <- cooks.distance(model_6)
which(influential_points > 1) # vu que c'est un gros dataset, je peux remplacer 4/n par 1 pour le critère de Cook's D
