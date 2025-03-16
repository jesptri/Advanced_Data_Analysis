library(readxl)
library(car)

NoiseFilter <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced_data_analysis/data/NoiseFilter.xlsx")
Books <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced_data_analysis/data/Books.xlsx")
Jigsaw <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced_data_analysis/data/Jigsaw.xlsx")


# A. Mediation 
summary(lm(read ~ enjoy,data = Books))  #Estimates total effect of X (enjoy) on Y (reading)
summary(lm(buy ~ enjoy, data = Books))  #Estimates effect of X (enjoy) on mediating variable (buy)
summary(lm(read ~ enjoy + buy, data = Books)) #Estimates the direct effect of X (enjoy) on Y (reading) 

# B. Moderation

Jigsaw$gender.f <- factor(Jigsaw$Gender)

# Main Effects model 
summary(lm(time ~ alcohol + gender.f, data = Jigsaw))  

# Moderated regression model (ie with an interaction term included)
summary(lm(time ~ alcohol + gender.f + alcohol*gender.f, data = Jigsaw))

# C. Two-way ANOVA
NoiseFilter$carsize.f <- factor(NoiseFilter$carsize, labels = c("small", "medium", "large"))
NoiseFilter$noise <- NoiseFilter$noise
NoiseFilter$type.f <- factor(NoiseFilter$type, labels = c("standard", "Octel"))

reg <- lm(noise ~ carsize.f + type.f + carsize.f:type.f, data = NoiseFilter)
summary(reg)
anova(reg)

# Estimated marginal means
library(emmeans)

m<-emmeans(reg, ~carsize.f:type.f)
emmip(m, type.f ~ carsize.f )  # Interaction plot

# Standardised residauls and fitted values

r<- rstandard(reg)  # z-values for residuals
p<- predict.lm(reg)  # fitted values

plot(p, r)

