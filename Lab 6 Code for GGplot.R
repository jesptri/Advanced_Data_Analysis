library(readxl)

Forbes <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced_data_analysis/data/Forbes.xlsx")

# A. Forbes 

Forbes$Sector.f <- factor(Forbes$Sector, levels = c("2", "3", "4"), labels = c("Energy", "Finance", "High Tech"))

Forbes$lnAssets <- log(Forbes$Assets)
Forbes$lnSales <- log(Forbes$Sales)

reg <- lm(lnSales ~ lnAssets + Sector.f, data = Forbes)

# print(summary(reg))

print(anova(reg))

Forbes$p <- fitted(reg)

library(emmeans)
emmeans(reg, ~Sector.f)

# (iv) GGplots

library(ggplot2)

#ggplot with separate regression lines fitted to the Sectors

ggplot(Forbes, aes(x = lnAssets, y = lnSales, color = Sector.f)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_line(aes(y = p))  

# ggplot with predicted values from the ANCOVA model included
ggplot(Forbes, aes(x = lnAssets, y = lnSales, color = Sector.f)) + 
  geom_point() +
  geom_line(aes(y = p)) 