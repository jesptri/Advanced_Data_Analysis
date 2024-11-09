library(readxl)
# install.packages("car")
library(car)

lab_4 <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced data analysis/data/lab 4.xlsx")

# pairs(lab_4)

# scatterplot(lab_4$price)

price <- c(lab_4$price)
bedrooms <- c(lab_4$bedrooms)
bathrooms <- c(lab_4$bathrooms)
sqft_living <- c(lab_4$sqft_living)
sqft_lot <- c(lab_4$sqft_lot)
floors <- c(lab_4$floors)
condition <- c(lab_4$condition)
grade <- c(lab_4$grade)
yr_built <- c(lab_4$yr_built)
lat <- c(lab_4$lat)
long <- c(lab_4$long)

# pairs(lab_4)
reg <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + condition + grade + yr_built + lat + long)

# print(vif(reg))

z_res <- rstandard(reg)
fit <- reg$fitted.values

# plot(fit, z_res)
# hist(z_res)

lnprice <- log(lab_4$price)

reg_2 <- lm(lnprice ~ bathrooms + sqft_living + factor(condition) + grade + yr_built + lat)

# print(summary((reg_2)))
anova(reg_2)

z_res <- rstandard(reg_2)
fit <- reg_2$fitted.values

plot(fit, z_res)
# hist(z_res)

# cooks <- cooks.distance(reg_2)

# print(cooks)
