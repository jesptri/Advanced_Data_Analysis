library(readxl)

# Premiership_2019_20_stats <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced data analysis/Premiership_2019_20_stats.xlsx")
Premiership_2019_20_stats <- read_excel("c:/Users/jules/Desktop/Limerick/Advanced data analysis/Premiership 2019_20 stats.xlsx")


#Ensures that position is treated as categorical variable in the analysis. 
Premiership_2019_20_stats$position <- as.factor(Premiership_2019_20_stats$position)

#Simple frequency table
table(Premiership_2019_20_stats$position)

#Boxplot of height by position
boxplot(height ~ position, data = Premiership_2019_20_stats,
        main = "Boxplot of Height",
        xlab = "Position",
        ylab = "Height (cm)"
       )

#Creates a dataset that only contains information from Goalkeepers and Defenders
sub<- subset(Premiership_2019_20_stats,position %in% 
               c("Defender", "Goalkeeper"))
# View(sub)

# T-test (by default equal variances are not assumed)
t.test(height ~ position, data = sub)
