library(readxl)

cereal <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced data analysis/data/cereal.xlsx")

table(cereal$mfr)

cereal$mfr <- as.factor(cereal$mfr)

# barplot(table(cereal$mfr))

cereal$sugars[cereal$sugars <0] <- NA
cereal$potass[cereal$potass <0] <- NA
cereal$carbo[cereal$carbo <0] <- NA

hist(cereal$fiber)
print(summary(cereal$fiber))
hist(cereal$carbo)
print(summary(cereal$carbo))

pairs(cereal[,c(4,5,6,10,11,16)])

round(cor(cereal[,c(4,5,6,10,11,16)], method = "pearson"))

### a terminer ###