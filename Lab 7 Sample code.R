#Gender
Heart_study$Gender.f <- factor(Heart_study$GENDER, levels = c("1", "2"), 
                               labels = c("male", "female"))
Heart_study$CHD.f <- factor(Heart_study$CHD, levels = c("0", "1"), 
                               labels = c("no", "yes"))

#(a) Basic cross tab with chi-square test
t<-table(Heart_study$Gender.f, Heart_study$CHD.f)
t
chisq.test(t)

#(b)
reg <- glm(CHD.f ~ CURSMOKE, family = "binomial", data = Heart_study)
summary(reg)
round(cbind(exp(reg$coefficients),exp(confint(reg))),2)

#(c)
# set the reference category for gender to be female - this ensures odds ratio will be > 1,
# as the crosstab in (a) has found the risk of CHD is higher in males compared to females

Heart_study$Gender.f <- relevel(Heart_study$Gender.f, "female")

reg <- glm(CHD ~ Gender.f, family = "binomial", data = Heart_study)
summary(reg)
round(cbind(exp(reg$coefficients),exp(confint(reg))),2)  #odds ratios with associated 95% CI

#(d)

reg <- glm(CHD ~ CURSMOKE + AGE + Gender.f + TOTCHOL + SYSBP, family = "binomial", data = Heart_study)
summary(reg)
exp(cbind(OR = coef(reg), confint.default(reg)))

round(exp(cbind(OR = coef(reg), confint.default(reg))), digit = 2)

#Question 2: Bad Car

reg <- glm(IsBadBuy ~ VehicleAge + Dealer + VehOdo + VehBCost, family = "binomial", data = Badcar)
summary(reg)

Badcar$Dealer.f <- factor(Badcar$Dealer, levels = c("1", "0"), label = c("franchised", "Independent"))

reg <- glm(IsBadBuy ~ VehicleAge + Dealer.f + VehOdo + VehBCost, family = "binomial", data = Badcar)
summary(reg)

round(exp(cbind(OR=coef(reg),confint.default(reg))), digits = 2)

# This analysis suggests that a car bought from an independent dealer is more likely to be a 
#"bad car" compared to franchised dealer, OR = 3.3 (95% CI: 2.39, 4.57), controlling for car age, milage and cost.

anova(reg)

