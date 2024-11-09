library(readxl)

lab_2 <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced data analysis/data/lab 2.xlsx")

y <- c(lab_2$y)
x1 <- c(lab_2$x1)
x2 <- c(lab_2$x2)
x3 <- c(lab_2$x3)

X <- cbind(x1, x2, x3) # création du vecteur

X_t <- t(X) # transposée du vecteur

produit <- X_t%*%X # produit

inverse_du_produit <- solve(produit)

b <- inverse_du_produit%*%X_t%*%y

# sigma^2 = sum(y-y_hat)^2

y_hat <- X %*% b
error <- y-y_hat

sigma2 <- (t(error)%*%error)[1,1]
# print(sigma2)

# inverse_produit_Xt_X <- solve(X_t*X)

# Varb <- sigma2*inverse_produit_Xt_X

reg <- lm(y ~ x1 + x2 + x3, data=lab_2)

print(summary(reg))


