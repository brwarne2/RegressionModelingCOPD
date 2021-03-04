setwd("~/Desktop/RegressionPublicHealth")

g <- read.csv(file = "diabetesData_.csv", header=TRUE, sep =',')
age <- g$age
d <- density(age)
plot(d,main="") # kernal density plot

# Cholesteral 
chol.no.na <- chol[is.na(chol)==0]
Cd <- density(chol.no.na)
plot(d,main = "") 
# HDL
HDL <- g$hdl
HDL.no.na <- HDL[is.na(HDL)==0]
Hd <- density(HDL.no.na)
plot(d,main = "")

# Asessing crude relations between predictors and outcome
  # define the gender variable 
  gender <- as.factor(g[,"gender"]) 
  # cross tabulation 
  dm_by_gender <- table(gender, dm) # not including NA values because there aren't that many 
  # proportion of diabetes status by gender 
  dm_by_gender_prop <- prop.table(dm_by_gender, margin = 1) 
  # calculate the odds of having diabetes by gender 
  odds_gender <- dm_by_gender_prop[, "yes"]/dm_by_gender_prop[, "no"] 
  # calculate the log odds 
  logodds_gender <- log(odds_gender) 
  # plot the log odds of having diabetes by gender 
  dotchart(logodds_gender)
  # draw lines instead of dots
  plot(as.factor(names(logodds_gender)), logodds_gender)
  # plotting the relation between age and the outcome
    # define the age variable (continuous) 
    age <- age <- g[,"age"] 
    # create a cross tabulation of age and diabetes status  
    dm_by_age <- table(age, dm) # not including NA values because there aren't that many 
    # output the frequencies of diabetes status by age 
    dm_by_age_prop <- prop.table(dm_by_age, margin = 1) 
    # calculate the odds of having diabetes 
    odds_age <- dm_by_age_prop[, "yes"]/dm_by_age_prop[, "no"] 
    # calculate the log odds 
    logodds_age <- log(odds_age) 
    # plot the ages found in the sample against the log odds of having diabetes 
    plot(rownames(dm_by_age_prop), logodds_age) 
    # age grouping converting continuous variable to a categorical (ordinal) one  
    age_grouped <- ifelse(age < 45, "under 45", 
                          ifelse(age >= 45 & age < 65, "45 - 64",  
                                 ifelse(age >= 65 & age < 75, "65 - 74",  
                                        ifelse(age >= 75, "75 or over", NA)))) 
    
    age_grouped <- factor(age_grouped, levels = c("under 45", "45 - 64", "65 - 74", "75 or over")) 
    # create a cross tabulation of age and diabetes status  
    dm_by_age_grouped <- table(age_grouped, dm) 
    # output the frequencies of diabetes status by age 
    age_grouped_prop <- prop.table(dm_by_age_grouped, margin = 1) 
    # calculate the odds of having diabetes 
    odds_age_grouped <- age_grouped_prop[, "yes"]/age_grouped_prop[, "no"] 
    # calculate the log odds 
    logodds_age_grouped <- log(odds_age_grouped) 
    # plot the age groups found in the sample against the log odds of having diabetes 
    dotchart(logodds_age_grouped) 
  # plotting the relation between cholesterol and the outcome
    # define chol as a continuous variable 
    chol <- g[,"chol"]
    # create a cross tabulation of cholesterol and diabetes status  
    dm_by_chol <- table(chol, dm) # not including NA values because there aren't that many 
    # output the frequencies of diabetes status by cholesterol 
    dm_by_chol_prop <- prop.table(dm_by_chol, margin = 1) 
    # calculate the odds of having diabetes 
    odds_chol <- dm_by_chol_prop[, "yes"]/dm_by_chol_prop[, "no"] 
    # calculate the log odds 
    logodds_chol <- log(odds_chol) 
    # plot the cholesterol found in the sample against the log odds of having diabetes 
    plot(rownames(dm_by_chol_prop), logodds_chol, xlim=c(150, 300)) 
    # categorising chol into an ordinal variable 
    # https://www.medicalnewstoday.com/articles/315900.php 
    chol_categorised <- ifelse(chol < 200, "healthy",  
                               ifelse(chol < 240, "borderline high", 
                                      ifelse(chol >= 240, "high", NA))) 
    # make sure that it is treated as a factor/categorical variable and ordering the levels within the factor for the table 
    chol_categorised <- factor(chol_categorised, levels = c("healthy", "borderline high", "high")) 
    # create a cross tabulation of cholesterol and diabetes status  
    dm_by_chol_categorised <- table(chol_categorised, dm) # not including NA values because there aren't that many 
    # output the frequencies of diabetes status by cholesterol 
    dm_by_chol_categorised_prop <- prop.table(dm_by_chol_categorised, margin = 1) 
    # calculate the odds of having diabetes 
    odds_chol_categorised <- dm_by_chol_categorised_prop[, "yes"]/dm_by_chol_categorised_prop[, "no"] 
    # calculate the log odds 
    logodds_chol_categorised <- log(odds_chol_categorised) 
    # plot the cholesterol categories found in the sample against the log odds of having diabetes 
    dotchart(logodds_chol_categorised)  
  # showing the relation between BMI and diabetes
    #bmi 
    height <- g[,"height"] 
    weight <- g[,"weight"] 
    height.si <- height*0.0254 
    weight.si <- weight*0.453592 
    bmi <- weight.si/height.si^2 
    # categorising BMI 
    bmi_categorised <- ifelse(bmi < 18.5, "underweight", 
                              ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                     ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                            ifelse(bmi > 30, "obese", NA)))) 
    # make sure that it is treated as a factor/categorical variable and ordering the levels within the factor for the table 
    bmi_categorised <- factor(bmi_categorised, levels = c("underweight", "normal", "overweight","obese")) 
    # create a cross tabulation of BMI and diabetes status  
    dm_by_bmi_categorised <- table(bmi_categorised, dm) # not including NA values because there aren't that many 
    # output the frequencies of diabetes status by BMI 
    dm_by_bmi_categorised_prop <- prop.table(dm_by_bmi_categorised, margin = 1) 
    # calculate the odds of having diabetes 
    odds_bmi_categorised <- dm_by_bmi_categorised_prop[, "yes"]/dm_by_bmi_categorised_prop[, "no"] 
    # calculate the log odds 
    logodds_bmi_categorised <- log(odds_bmi_categorised) 
    # plot the BMI categories found in the sample against the log odds of having diabetes 
    dotchart(logodds_bmi_categorised) 
    
# fitting a multiple logistic regression model
    m1 <- glm(dm ~ age + gender + bmi, family=binomial (link=logit)) 
    summary(m1) # age: log odds of  0.055 and small p-value means age is significantly and positively associated with the risk of getting diabetes
    # age: if we exponentiate 0.055, we get 1.057. This means that a one-year increase in age is associated with six percent higher odds of being diagnosed with diabetes
    exp(confint(m1)) # age: odds ratio is 1.06 with 95% CI 1.04 to 1.08.
    # gender: large p-value and wide CI suggest no strong evidence of gender difference in risk

# fitting another model: age, cholesterol and insurance type
    insurance <- factor(g$insurance) # must factor categoricals
    m2 <- glm(dm ~ age + chol + insurance, family=binomial (link=logit))
    summary(m2)    
    exp(confint(m2))    
    
# McFadden's r-squared
    # design your logistic regression 
    full_model <- glm(dm ~ age + chol + insurance, family=binomial (link=logit)) 
    # check your model 
    summary(full_model) 
    # run a null model 
    null_model <- glm(dm ~ 1, family=binomial (link=logit)) 
    # check 
    summary(null_model) 
    # calculate McFadden's R-square 
    R2 <- 1-logLik(full_model)/logLik(null_model) 
    # print it 
    R2 # .136 is not bad but also not great
    
# c-statistic
    install.packages("DescTools") 
    require(DescTools)
    # design your logistic regression 
    full_model <- glm(dm ~ age + chol + insurance, family=binomial (link=logit)) 
    # check your model 
    summary(full_model) 
    # generate the c-statistic 
    Cstat(full_model) 
    
# Hosmer-Lemeshow statistic and test
    install.packages("ResourceSelection")
    require(ResourceSelection) 
    # design your logistic regression 
    full_model <- glm(dm ~ age + chol + insurance, family = binomial(link = logit)) 
    full_model$y
    # run Hosmer-Lemeshow test 
    HL <- hoslem.test(x = full_model$y, y = fitted(full_model), g = 10) 
    HL 
    ##  Hosmer and Lemeshow goodness of fit (GOF) test 
      ## data:  full_model$y, fitted(full_model) 
      ## X-squared = 11.25, df = 8, p-value = 0.1879 
      # plot the observed vs expected number of cases for each of the 10 groups 
      plot(HL$observed[,"y1"], HL$expected[,"yhat1"]) 
      # plot the observed vs expected number of noncases for each of the 10 groups 
      plot(HL$observed[,"y0"], HL$expected[,"yhat0"])
      # plot observed vs. expected prevalence for each of the 10 groups 
      plot(x = HL$observed[,"y1"]/(HL$observed[,"y1"]+HL$observed[,"y0"]), 
           y = HL$expected[,"yhat1"]/(HL$expected[,"yhat1"]+HL$expected[,"yhat0"])) 
      install.packages("generalhoslem") 
      require(generalhoslem) 
      # run Hosmer-Lemeshow test 
      logitgof(obs = full_model$y, exp = fitted(full_model), g = 10) 
      # p-value of 0.1879 suggests good calibration
      