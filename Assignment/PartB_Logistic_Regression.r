library(readxl)
data <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced_data_analysis/Assignment/Staff_Turnover.xlsx")

print(data)

### 1 ###

model <- glm(Attrition ~ Age + BusinessTravel + Department + DistanceFromHome +
               Gender + HourlyRate + JobSatisfaction,
             data = data, family = binomial)

summary(model)

### 2 ###

odds_ratios <- exp(coef(model))
conf_intervals <- exp(confint(model))

print(odds_ratios)
print(conf_intervals)
