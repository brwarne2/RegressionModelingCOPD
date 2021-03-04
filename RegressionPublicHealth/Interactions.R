setwd("~/Desktop/RegressionPublicHealth")
COPD <- read.csv("COPD_student_dataset.csv")

# Model: MWT1best = a + b1 * Diabetic + 
    # b2 * AtrialFib + b3 * Diabetic * AtrialFib

COPD$Diabetes <- c(0,1)[as.integer(COPD$Diabetes)]
COPD$AtrialFib<- c(0,1)[as.integer(COPD$AtrialFib)]
DAF <- COPD$Diabetes * COPD$AtrialFib
# regression
r1 <- lm(MWT1Best~factor(Diabetes)+factor(AtrialFib)+factor(DAF),data=COPD)
summary(r1)
confint(r1)
# shortcut
r2 <- lm(MWT1Best~factor(Diabetes)+factor(AtrialFib)+factor(Diabetes*AtrialFib), data=COPD)
summary(r2)
confint(r2)

library(prediction)
list("Diabetes" = prediction(r2, at = list(Diabetes = c(0,1))),
     "AtrialFib" = prediction(r2, at = list(AtrialFib = c(0,1))),
     "Diabetes*AtrialFib" = prediction(r2, at = list(Diabetes = c(0,1), AtrialFib = c(0,1))))
# Shows walking distance changes for each value


