library(readxl)

cereal <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced data analysis/data/cereal.xlsx")

summary(cereal)  #carbo, sugars, potass all have -1 values instead of NA

table <- table(cereal$mfr)
barplot(table, xlab = "Manufacturer")

#replacing -1s with NAs
cereal$sugars[cereal$sugars == -1]<-NA
cereal$potass[cereal$potass == -1]<-NA
cereal$carbo[cereal$carbo == -1]<-NA


hist(cereal$fiber) #positively skewed use median and IQR to summarise
summary(cereal$fiber) #median = 2, IQR = (1, 3)
hist(cereal$carbo)  #symmetrically distributed - use mean and SD to summarise
mean(cereal$carbo)
mean(cereal$carbo,na.rm=TRUE)
sd(cereal$carbo, na.rm=TRUE)
hist(cereal$sugars) #symmetrically distributed - use mean and SD to summarise
mean(cereal$sugars,na.rm=TRUE)
sd(cereal$sugars, na.rm=TRUE)

pairs(cereal[,c(4,5,6,10,11,16)])
round(cor(cereal[,c(4,5,6,10,11,16)], method = "pearson", use = "pairwise.complete.obs"),2)

new <- cereal[cereal$type == "C",]
View(new)
dim(new) #74 rows and 16 columns

reg <- lm(rating ~ calories + fiber+ fat + protein + sugars + potass, data = new)
summary(reg)

#To check for any issues with multicollinearity can use jtools library or car library. 
library(jtools)
reg <- lm(rating ~ calories + fiber+ fat + protein + sugars + potass, data = new)
summ(reg, vifs = TRUE)
# Potassium and Fiber are highly correlated - only need one of these variables in the model. 

#Standardised residuals
z_res <- rstandard(reg)

hist(z_res) #histogram of standardised residuals

plot(fitted(reg),z_res)   #scatterplot of fitted values vs standardised residuals
# residual plots are consistent with the regression assumuptions of the errors being
# normally disributed with constant variance. 