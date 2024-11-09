library(readxl)

Lab3Emp <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced_data_analysis/data/Lab3Emp.xlsx")

Lab3Emp$sex.f <- factor(Lab3Emp$Sex, levels = c("1", "2"), 
                           labels = c("female", "male"))

Lab3Emp$Region.f <- factor(Lab3Emp$Region, levels = c("1", "2", "3","4"), 
                           labels = c("munster", "connaught", "leinster",
                                      "ulster"))

reg1 <- lm(Wage ~ Age + Doc + Quant + Educ + sex.f + Region.f, data = Lab3Emp)
summary(reg1)

library(car)
vif(reg1) # Quant and Doc have VIF > 5, indicating the regression model 
#has issues with multicollinearity

reg2 <- lm(Wage ~ Age + Quant + Educ + sex.f + Region.f, data = Lab3Emp)
summary(reg2)
vif(reg2)

z_res <- rstandard(reg2)
fit <- fitted(reg2)

hist(z_res)
plot(fit, z_res)

Lab3Emp$LnWage <-log(Lab3Emp$Wage)

reg3 <- lm(LnWage ~ Age + Quant + Educ + sex.f + Region.f, data = Lab3Emp)
summary(reg3)


z_res <- rstandard(reg3)
fit <- fitted(reg3)

hist(z_res)
plot(fit, z_res)

anova(reg3) #partial f-test F(3,152) = 1.606, p = 0.19, thus region is not statistically significant

#Using backward regression to remove Region and sex
reg4 <- lm(LnWage ~ Age + Quant + Educ, data = Lab3Emp)
summary(reg4)
anova(reg4)

# lnWage = 0.06286 + 0.009637 Age + 0.008228 Quant +0.0387 Educ

exp(reg4$coefficients)

# 2. 

new <- data.frame(Age = 45, Quant = 400, Educ = 17)
exp(predict(reg4, new, interval = "confidence"))  
# Predicted wages is 85.27 euro (95% CI: 72.62, 100.12)

#3. 


Lab3Emp$z_res <- rstandard(reg3)
Lab3Emp$fit <- exp(fitted(reg3))




