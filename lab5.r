library(readxl)

### A ###

books <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced data analysis/data/Books.xlsx")
# print(books)

enjoy <- c(books$enjoy)
buy <- c(books$buy)
read <- c(books$read)

reg1 <- lm(read ~ enjoy)
# print(summary(reg1))

reg2 <- lm(buy ~ enjoy)
# print(summary(reg2))

reg3 <- lm(read ~ enjoy + buy)
# print(summary(reg3))

### B ### 

jigsaw <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced data analysis/Jigsaw.xlsx")
# print(jigsaw)

time <- c(jigsaw$time)
alcohol <- c(jigsaw$alcohol)
Gender <- c(jigsaw$Gender)

reg4 <- lm(time ~ alcohol + Gender + alcohol*Gender)
# print((summary(reg4)))

### C ###

NoiseFilter <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced data analysis/NoiseFilter.xlsx")
# print(NoiseFilter)

noise <- c(NoiseFilter$noise)
carsize <- c(NoiseFilter$carsize)
type <- c(NoiseFilter$type)

reg5 <- aov(noise ~ factor(carsize) + factor(type))
print(summary(reg5))

