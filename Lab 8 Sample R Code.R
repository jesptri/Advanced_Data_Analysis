reg <- glm(Default ~ Age + DebtRatio + YearlyIncome + LatePayment, family = "binomial", na.action = na.exclude, data = CreditDefault)

summary(reg)

# Analysis has found strong evidence that customers with a previous history of late payment are more likely to default
#compared to those with no previous late payments  OR = 6.77 (95% CI: 5.03, 9.11), controlling for age, income and debt ratio
# The analysis also suggest that older customers are less likely to default (OR = 0.97 95% CI: 0.96, 0.98)), and 
#those with a higher income are less likely to default (OR = 0.99, 95% CI: 0.987, 0.996)
# Debt ratio was non-significant

round(exp(cbind(OR=coef(reg),confint.default(reg))), digits = 4 )


CreditDefault$p <- predict.glm(reg, type = "response")


p<- CreditDefault$p

CreditDefault$pred.default <- ifelse(CreditDefault$p> .5, 1, 0 )

table(CreditDefault$Default, CreditDefault$pred.default)

# caret library provides sensitivity, specificity etc from confusion matrix
library(caret)
cm <- confusionMatrix(table(CreditDefault$pred.default, CreditDefault$Default))
print(cm)

#ROC curve

library(pROC)

roc <- roc(CreditDefault$Default ~ CreditDefault$p)

plot(roc)

auc(roc) #gives the area under the curve
ci.auc(roc) #95% confidence interval for AUC
str(roc)  #shows all that is stored in roc

#Creates a dataframe which stores all the 
#possible combinations of p, sensitivity and specificity.
m<-data.frame(p = roc$thresholds, sensitivity = roc$sensitivities, specificity = roc$specificities)
View(m)


#ROC plot using ggplot2

library(ggplot2)
library(plotROC)
data <- data.frame(p = CreditDefault$p,s = CreditDefault$Default)
basicplot <- ggplot(data, aes(m = p, d = s)) + geom_roc()

#using style_roc() to add to the plot       
styledplot <- basicplot + style_roc()
styledplot

