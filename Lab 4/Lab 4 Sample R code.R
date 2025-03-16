library(readxl)
library(car)

Lab_4 <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced_data_analysis/data/lab 4.xlsx")

#(i)  Use scatterplots and boxplots


#(ii)

Lab_4$condition.f<- factor(Lab_4$condition, levels = c("1","2","3"), 
                           labels =c("poor", "good", "excellent"))

reg1 <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + condition.f
           + grade + yr_built + lat + long, data = Lab_4)

summary(reg1)

# VIF > 5 indicates issues with multicollinearity

library(car)
vif(reg1)  # No VIF values above 5, so no major issues with multicollinearity

z_res <- rstandard(reg1)
fit <- fitted(reg1)
hist(z_res)
plot(fit, z_res)  #plot shows evidence of hetroscadasity of the residuals
                  #Log transformation of the dependent variable may correct this.

Lab_4$lnprice <- log(Lab_4$price)  #Create the natural log transformation of price and save to dataset

reg2 <- lm(lnprice~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + condition.f
           + grade + yr_built + lat + long, data = Lab_4)

summary(reg2)

z_res <- rstandard(reg2)
fit <- fitted(reg2)
hist(z_res)
plot(fit, z_res)

#Backward regression - remove the predictor variable with the largest non-significant p-values
#from the model and repeat until the model contains only statistically signficant variables. 
reg3 <- lm(lnprice~  bathrooms + sqft_living 
           + grade + yr_built + lat + condition.f, data = Lab_4)

anova(reg3)   #Type 1 sums of squares can be used to give Partial F-test as condition is the last variable 
              #in the list of variables fitted, anova() function is used to provide the Type 1 Sums of squares

Lab_4$cooks <- cooks.distance(reg3)   # computes Cook's D and saves to dataset. 
# No cook's D values above 1, so no influential points. 

summary(reg3)

exp(reg3$coefficients)  #The exponential of the coefficients can be interpreted as the 
# estimate factor by which price increases or decreases for a 1 unit increase in the 
# independent variable controlling for the other variables in the model. 

reg3 <- lm(lnprice~  bathrooms + sqft_living 
           + grade + yr_built + lat + condition.f, data = Lab_4)  
#final fitted regression model 
summary(reg3)
anova(reg3)
avPlots(reg3)  #Partial regression plots 

#Saving the residuals to the data set identifies houses that are over and under valued. 
Lab_4$z_res <- rstandard(reg3)
Lab_4$fit <- exp(fitted(reg3))
