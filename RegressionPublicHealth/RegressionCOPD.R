library(dplyr)
library(tidyverse)

setwd("~/Desktop/RegressionPublicHealth")
COPD <- read.csv("COPD_student_dataset.csv")

# Basic format of lm(): lm(outcome ~ predictor, data=dataframe)

# Run a linear regression to check whether lung function is a predictor of walking distance in COPD patients
MWT1Best_FEV1 <- lm(MWT1Best~FEV1, data = COPD)
summary(MWT1Best_FEV1)
confint(MWT1Best_FEV1)
plot(MWT1Best_FEV1)
par(mfrow=c(2,2)) # shows all four plots at once
# The first plot is a constant variance plot, which checks for the homogeneity of the variance and the linear relation. If you see no pattern in this graph, then your assumptions are met. 
# The second plot is a Q-Q plot, which checks that the residuals follow a normal distribution. The points should fall on a line if the normality assumption is met. 
# The third plot allows to detect heterogeneity of the variance. 
# The fourth plot allows for the detection of points that have a large impact on the regression coefficients. 

# The wobbly line in the Residuals vs. Fitted plot suggests heteroscedasticity (unequal variance)

# Run a regression to check whether age is a predictor of walking distance in COPD patients
MWT1Best_AGE <- lm(MWT1Best~AGE, data = COPD)
summary(MWT1Best_AGE)
# For every one unit (year) increase in age, walking distance decreases 3.104 units
confint(MWT1Best_AGE)
# The range in possible effects is: -5.735 to -.0472
plot(MWT1Best_AGE)
par(mfrow=c(2,2)) # shows all four plots at once
# The first plot is a constant variance plot, which checks for the homogeneity of the variance and the linear relation. If you see no pattern in this graph, then your assumptions are met. 
# The second plot is a Q-Q plot, which checks that the residuals follow a normal distribution. The points should fall on a line if the normality assumption is met. 
# The third plot allows to detect heterogeneity of the variance. 
# The fourth plot allows for the detection of points that have a large impact on the regression coefficients. 

# The wobbly line in the Residuals vs. Fitted plot suggests heteroscedasticity (unequal variance)

# Multiple Regression time!
# Let's check whether lung function and age are predictors of walking distance in COPD patients
MWT1Best_FEV1_AGE <- lm(MWT1Best~FEV1+AGE, data=COPD)
summary(MWT1Best_FEV1_AGE)
confint(MWT1Best_FEV1_AGE)
# Great! The regression fits 25.5% of variability and p-value is very low

# Now let's replace FEV1 with FVC (the total volume of air that a patient can forcibly exhale in one breath)
MWT1Best_FVC_AGE <- lm(MWT1Best~FVC+AGE, data=COPD)
summary(MWT1Best_FVC_AGE)
confint(MWT1Best_FVC_AGE)
# But first, check the relationship between walking distance and FVC
MWT1Best_FVC <- lm(MWT1Best~FVC, data = COPD)
summary(MWT1Best_FVC)
# For every one unit increase in FVC, walking distance decreases 48.63 units
confint(MWT1Best_FVC)
# The range in possible effects is:
plot(MWT1Best_FVC)
par(mfrow=c(2,2))
# Check for collinearity by plotting
plot(COPD$AGE, COPD$FVC, xlab="AGE", ylab="FVC")
# and by calculating Spearman
cor.test(COPD$AGE, COPD$FVC, use="complete.obs", method="spearman")
# -.18 is a very weak association, collinearity is likely not an issue

# Reminder: Fitting a model between FVC, AGE, and FEV1 would not be a good idea because FEV1 and FVC are strongly correlated
# As you can see, there is a strong correlation:
par(mfrow=c(1,1))
plot(COPD$FEV1, COPD$FVC, xlab="FEV1", ylab="FVC")

# Let's run the following regression: MWT1Best= a + b*copd 
class(COPD$MWT1Best)
class(COPD$copd) #copd is categorical, therefor must be of type 'factor'
COPD$copd <- factor(COPD$copd)
class(COPD$copd)
# visualise the structure of the data in that variable
str(COPD$copd)
# Now we're ready to run the regression 
lr1 <- lm(MWT1Best~copd, data=COPD)
summary(lr1) # copd1 is automatically used as the reference category
# Change the reference category to 'severe'
COPD$copd <- relevel(COPD$copd,ref=3)
lr1 <- lm(MWT1Best~copd, data=COPD)
summary(lr1)

# Creating new variables (comorbidities) from old ones 
# Create an empty vector 'comorbid' of same length as other variables
comorbid <- length(COPD$Diabetes)
# Fill this vector with binary values where 1 indicates the presence of at least one comorbidity and 0 represents the complete absence of comorbitidies
comorbid[COPD$Diabetes ==1 | COPD$muscular == 1 | COPD$hypertension ==1 | COPD$AtrialFib == 1 | COPD$IHD == 1] <- 1
# Assign a value of zero to NA values
comorbid[is.na(comorbid)] <- 0
comorbid <- factor(comorbid)
# Add this variable to the existing COPD dataset
COPD$comorbid <- comorbid

# multiple regression: FEV1, AGE, gender, COPDSEVERITY, comorbid on MWT1best
mlr1 <- lm(MWT1Best~FEV1+AGE+factor(gender)+factor(COPDSEVERITY)+factor(comorbid), data=COPD)
summary(mlr1)
confint(mlr1)
  # Communicating findings
    # adjusted r2 shows that model explains 29% of variance
    # AGE: the model estimates that walking distance decreases by 3.2 metres for every year increase in age, adjusting for FEV1, gender, COPD severity, and comorbidities
    # Comorbidity: the model estimates that the average walking distance is 45.3 metres less in patients with at least one comorbidity compared to no comorbidities, adjusting for FEV1, AGE, gender, & COPDSEVERITY
    # COPDSEVERITY: not very precise (-255 to -66)

# Checking for colinearity
library(mctest)
imcdiag(mlr1,x = model.matrix(mlr1)[,-1], y = mlr1$model[1], method = 'VIF')
# FEV1 and COPSEVERITY appear to be associated with one another
