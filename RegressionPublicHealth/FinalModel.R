setwd("~/Desktop/RegressionPublicHealth")
COPD <- read.csv("COPD_student_dataset.csv")
library(Hmisc)
library(gmodels)
# Model Building
# Research Question: What are the patient characteristics that predict worsening depression/anxiety (HAD) in COPD patients?

# This requires the comorbid variable:
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

# Examine the relationship between candidate predictor variables (pairwise correlations and scatterplot matrices for continuous variables; cross tabulations for categorical variables)
  # Correlation Matrix
  my_data <- COPD[,c("AGE","MWT1Best","CAT")]
  # Create new vector including the variables to be analysed
  cor_matrix <- cor(my_data)
  # View the matrix
  cor_matrix
  round(cor_matrix,2)
  # Visually assess correlation
  pairs(~AGE+FEV1+CAT+gender, data = COPD)
  # There does not appear any correlated values

# Let's fit the model
mlr1 <- lm(HAD~AGE+CAT+gender*comorbid, data=COPD)
summary(mlr1)
confint(mlr1)
# Findings:
  # This model explains 15.5% of variance
  # For every one unit increase in FEV1, HAD decreases by 3.18 units
  # For every one unit increase in age, HAD decreases by .55 units
  # For every one unit increase in walking distance, HAD decreases by .044 units; walking distance increases by 22.72 metres for each decrease in HAD
  # For every one unit increase in CAT, HAD decreases by .023
# Checking for colinearity
library(mctest)
imcdiag(mlr1,x = model.matrix(mlr1)[,-1], y = mlr1$model[1], method = 'VIF')
# No collinearity is found