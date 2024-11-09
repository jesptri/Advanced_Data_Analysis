#Lab 2:   Possible R code for Q1 parts (i) – (iv)

library(readxl)

lab_2 <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced_data_analysis/data/lab 2.xlsx")

# read the data into vectors 
x1<-lab_2$x1
x2<-lab_2$x2
x3<-lab_2$x3
#Or could use attach() or could access the data from the dataframe using $

y<-lab_2$y
# create matrix X
one<-matrix(1,length(y),1)
X<-cbind(one,x1,x2,x3)

# (i) compute the Matrix (x'x)^-1
TX <- t(X)
inv<-solve(TX%*%X)   #solve() can be used to invert a matrix

# (ii) compute the vector b of regression coefficients
b <- inv%*%TX%*%y
b
# (iii) First compute the fitted values y_hat = Xb and the residuals y - y_hat
y_hat <- X%*%b
res_y <- y - y_hat
err_sq <- t(res_y)%*%res_y
sigma_hat <- err_sq/(length(y)-4)
var_cov <-sigma_hat[1,1]*solve(TX%*%X)
var_cov


# Fit regression using lm()

reg <- lm(y ~ x1+x2+x3, data = lab_2)
summary(reg)

# Results: overall model is statistically significant 
# F(3,26) = 22.17, p-value <0.001
# x1 is non-significant p = 0.2218
# x2 and x3 are statistically significant predictors of y

# Examine multicollinearity 
library(car)
vif(reg)  #results so no issue as VIF values <5

#Examine the residuals
z_res <- rstandard(reg)  #z-values for the residuals
fitted <- reg$fitted.values # fitted values (ie predicted values)

hist(z_res) #histogram of z-values for residuals shows approx
            #normally distributed
plot(fitted, z_res) #scatterplot of the residuals vs fitted shows no
                    #residuals have constant variance
#From the residual plots the assumptions of regression are satisfied.

