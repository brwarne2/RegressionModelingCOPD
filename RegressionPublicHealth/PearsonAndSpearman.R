library(dplyr)
library(tidyverse)

setwd("~/Desktop/RegressionPublicHealth")
COPD <- read.csv("COPD_student_dataset.csv")

# Assessing the association between walking distance and 
# lung function in COPD patients

# Take a looksie
hist(COPD$MWT1Best, main="Histogram of MWT1Best", xlab=
       "MWT1Best", breaks=12)
# Investigate high MWT1Best values
subset(COPD, MWT1Best > 650)
# Look at high and low MWTBest values
subset(COPD, MWT1Best > 600 | MWT1Best < 150)
# Look at AGE scores
hist(COPD$AGE, main="Histogram of AGE", xlab="AGE")
# View descriptive statistics for MWT1Best, removing NAs
list("Summary" = summary(COPD$MWT1Best), "Mean" = mean(COPD$MWT1Best, na.rm=TRUE), 
     "Standard Deviation" = sd(COPD$MWT1Best, na.rm=TRUE), "Range" = range(COPD$MWT1Best, 
      na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$MWT1Best, na.rm=TRUE)) 
# View descriptive statistics for AGE, removing NAs
list("Summary" = summary(COPD$AGE), "Mean" = mean(COPD$AGE, na.rm=TRUE), 
     "Standard Deviation" = sd(COPD$AGE, na.rm=TRUE), "Range" = range(COPD$AGE, 
      na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$AGE, na.rm=TRUE)) 
# View correlation between AGE and MWT1Best
plot(COPD$AGE, COPD$MWT1Best, xlab = "AGE", ylab = "MWT1Best")
# The above plot suggests Pearson's correlation coefficient is best to assess these two variables
# Calculate Pearson, removing missing values
cor.test(COPD$AGE, COPD$MWT1Best, use="complete.obs", method="pearson")
# The p-value is less than 0.001, showing strong evidence against a null hypothesis of the coefficient being zero
# Now calculate Spearman's rank correlation for funzies
cor.test(COPD$AGE, COPD$MWT1Best, use="complete.obs", method="spearman")
# Spearman's suggest similar results, with correlation around 0.45 (moderate correlation between walking distance and lung function)


