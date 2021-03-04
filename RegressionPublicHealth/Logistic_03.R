setwd("~/Desktop/RegressionPublicHealth")
g <- read.csv(file = "diabetesData_.csv", header=TRUE, sep =',')
# Fitting a model using Backwards Elimination
  # making the variables
  dm <- as.factor(g[,'dm'])
  insurance <- as.factor(g[,'insurance']) # 0=none, 1=gov, 2=private
  fh <- as.factor(g[,'fh']) # 1=FH, 0 = no FH
  smoking <- as.factor(g[,"smoking"]) # 1,2,3 
  chol <- g[,'chol'] 
  hdl <- g[,'hdl'] 
  ratio <- g[,'ratio'] 
  location <- as.factor(g[,'location']) 
  age <- g[,'age'] 
  gender <- as.factor(g[,'gender']) 
  frame <- as.factor(g[,'frame']) 
  systolic <- g[,'bp.1s'] 
  diastolic <- g[,'bp.1d'] 
  #bmi 
  height <- g[,"height"] 
  weight <- g[,"weight"] 
  height.si <- height*0.0254 
  weight.si <- weight*0.453592 
  bmi <- weight.si/height.si^2 
# creating the model
  model <- glm(dm ~ age + bmi + chol + hdl + systolic + diastolic, family = binomial(link = logit)) 
  summary(model)  
  anova(model, test = "Chisq") # it is clear that neither BP variables is significantly associated with the odds
# revised model
  model <- glm(dm ~ age + bmi + chol + hdl, family = binomial(link = logit)) 
  summary(model)
# investigating why systolic and diastolic are not significant
  cor.test(systolic, hdl) # not significant 
  cor.test(systolic, bmi) # significant 
  cor.test(systolic, chol) # very significant
  cor.test(systolic, age) # extremely significant 
# revise model without age, adding back systolic
  model <- glm(dm ~ bmi + chol + hdl + systolic, family = binomial(link = logit)) 
  summary(model)  # systolic is significant now
# new model: age, BMI, cholesterol, HDL, systolic BP, and diastolic BP
  # adding in gender, location, frame, insurance, and smoking
  model <- glm(dm ~  bmi + chol + hdl + systolic + diastolic + gender + location + frame +insurance+smoking, family = binomial(link = logit)) 
  summary(model)  
  anova(model, test = "Chisq") 
  
  