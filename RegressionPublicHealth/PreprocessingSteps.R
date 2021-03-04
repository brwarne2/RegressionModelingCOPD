library(Hmisc)
library(gmodels)

setwd("~/Desktop/RegressionPublicHealth")
COPD <- read.csv("COPD_student_dataset.csv")

# These steps MUST be performed before conducting any analyses, much less a multivariate regression model

# See how many rows and columns you have in your data set
dim(COPD)
# Examine variables in a few different views (Hmisc package)
describe(COPD)
# See the top of the data
head(COPD)
# Produce crosstabs for categorical variables (gmodels package)
CrossTable(COPD$copd)
# Summarise continuous variables
summary(COPD$MWT1Best)
# Check variable types
class(COPD$AGE)
# Basic statistics
summary(COPD$AGE)
# Examine the relationship between candidate predictor variables (pairwise correlations and scatterplot matrices for continuous variables; cross tabulations for categorical variables)
# Correlation Matrix
  my_data <- COPD[,c("AGE","PackHistory", "FEV1", "FEV1PRED", "FVC", "CAT", "HAD", "SGRQ")]
  # Create new vector including the variables to be analysed
    cor_matrix <- cor(my_data)
  # View the matrix
    cor_matrix
    round(cor_matrix,2)
  # Visually assess correlation
    pairs(~AGE+PackHistory+FEV1+FEV1PRED+FVC+CAT+HAD+SGRQ, data = COPD)
# Fit a simple linear regression model
    # model_name <- lm(outcome ~ predictor, data = dataframe)
    # summary(model_name)
    # confint(model_name)
# Check distributions
hist(COPD$AGE) # We notice a slight skew in the age data: the tail at the lower end is longer than the tail at the upper end

# Do these steps with CAT
class(COPD$CAT)
summary(COPD$CAT) # We notice an outlier: 188
hist(COPD$CAT) # Confirmed this is indeed and outlier

# Inspect COPDSEVERITY
class(COPD$COPDSEVERITY)
table(COPD$COPDSEVERITY, exclude=NULL)

# Inspect Gender
class(COPD$gender)
# Make sure R treats this as categorical (factor):
COPD$gender <- as.factor(COPD$gender)
class(COPD$gender)


