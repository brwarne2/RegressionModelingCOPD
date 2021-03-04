setwd("~/Desktop/RegressionPublicHealth")

g <- read.csv(file = "diabetesData_.csv", header=TRUE, sep =',')
dim(g)
colnames(g)
chol <- g["chol"]
gender <- as.factor(g[,"gender"])
dm <- as.factor(g[,"dm"])
t <- table(gender) # store the tabulation for future manipulation
addmargins(t) # sum up the gender totals and give an overall total
round(prop.table(t),digits=3) # get proportions rounded to 3dp
dm2 <- factor(dm, exclude=NULL) # make new factor from old one
table(dm2) # display the counts including the missing (NAs)
summary(chol)
height <- g[,'height']
weight <- g[,'weight']
summary(height)
summary(weight)
height.si <- height*0.0254
weight.si <- weight*0.453592
bmi <- weight.si/height.si^2
summary(bmi)
bmi_categorised <- ifelse(bmi < 18.5, "underweight", 
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                 ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                        ifelse(bmi > 30, "obese", NA)))) 

# check that the bmi_categorised variable has worked  
table(bmi_categorised, exclude = NULL) 
# frequencies of diabetes by BMI category 
dm_by_bmi_category <- table(bmi_categorised, dm2, exclude = NULL) 
# check 
dm_by_bmi_category 
# with row percentages
round(100 * prop.table(dm_by_bmi_category, margin = 1), digits = 1) 
# make age groups
age <- g$age
age_groups <- ifelse(age > 75, ">75", 
                          ifelse(age >= 65 & age <= 74, "65-74", 
                                 ifelse(age >= 45 & age <= 64, "45-64", 
                                        ifelse(age < 45, "<45", NA)))) 
table(age_groups, exclude = NULL) 
# frequencies of gender by age group
gender_age_groups <- table(age_groups, gender, exclude = NULL) 
gender_age_groups
# with row percentages
round(100 * prop.table(gender_age_groups, margin = 1), digits = 1) 


# Simple Logistic Regression
m <- glm(dm ~ age, family=binomial (link=logit))
summary(m)
# create a cross tabulation of age and diabetes status  
dm_by_age <- table(age, dm) 

# output the frequencies of diabetes status by age 
freq_table <- prop.table(dm_by_age, margin = 1) 

# calculate the odds of having diabetes 
odds <- freq_table[, "yes"]/freq_table[, "no"] 

# calculate the log odds 
logodds <- log(odds) 

# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(freq_table), logodds) 

# Now for gender
n <- glm(dm ~ gender, family=binomial (link=logit))
summary(n)
contrasts(gender)

levels(gender)
gender <- relevel(gender, ref = "male") # changing male to be the reference value
levels(gender)
# the new model compares the log odds of diabetes of females compared to males
n <- glm(dm ~ gender, family=binomial (link=logit))
summary(n)
# look at model's coefficients
n$coefficients
# change from log odds ratios to odds ratios
exp(m$coefficients)

# make location group for Buckingham
location <- as.factor(g$location)
locations <- ifelse(location == "Buckingham", "Buckingham", 
                     ifelse(location == "Louisa", "Louisa", NA))
table(locations, exclude = NULL) 
# frequencies of gender by age group
dm_location_groups <- table(locations, dm, exclude = NULL) 
dm_location_groups
# with row percentages
round(100 * prop.table(dm_location_groups, margin = 1), digits = 1) 
levels(location)
location <- relevel(location, ref = "Buckingham") # changing male to be the reference value
levels(location)
# the new model compares the log odds of diabetes of Louisa compared to Buckingham
l <- glm(dm ~ location, family=binomial (link=logit))
summary(l)
# look at model's coefficients
l$coefficients
# change from log odds ratios to odds ratios
exp(l$coefficients)
