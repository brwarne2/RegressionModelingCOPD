
setwd("~/Desktop/RegressionPublicHealth")
COPD <- read.csv("COPD_student_dataset.csv")

# Binary variables
# Gender (Binary) and walking distance (Continuous) in COPD patients:
MWT1Best_gender <- lm(MWT1Best~gender, data = COPD)
summary(MWT1Best_gender)
# MWT1Best = 379.7 + 30.5*Gender  ...  what does this mean?
# The above regression coefficient for 'gender' represents the expected change in our outcome, 'walking distance', for a one unit increase in our predictor
# Female = 0 Male = 1
# Mean distance for males is 410.1 (379.7+30.5), Females is 379.7
# If gender values were flipped, the equation would be: 410 - 30.5*Gender

# Categorical variables
# One of the categories will still need to be selected as the reference category, and the other categories get compared to the reference
# COPD Severity (Mild, Moderate, Severe, Very Severe) and walking distance:
MWT1Best_COPD <- lm(MWT1Best ~ COPDSEVERITY,data = COPD)
summary(MWT1Best_COPD)
# The regression coefficients here show how much 'walking distance' changes for 'moderate', 'severe' and 'very severe' - all compared to 'mild'
# The coefficients are all negative, suggesting that 'walking distance' decreases as 'disease severity' increases, and the magnitude increases with increasing severity



