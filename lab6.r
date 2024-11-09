library(readxl)

CT <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced data analysis/data/clinical_trial.xlsx")

print(CT)

Baseline <- CT$Baseline
Medication <- CT$Medication
Cognitive_Therapy <- CT$Cognitive_Therapy
Post_6_Weeks <- CT$Post_6_Weeks

reg <- lm(Post_6_Weeks ~ Baseline + Medication + Cognitive_Therapy)

anova(reg)
print(summary(reg))

library(emmeans)
emmeans(reg, ~Medication:Cognitive_Therapy)

