library(dplyr) 
library(ggplot2) 
library(GGally) 
library(caTools) 
library(ROCR) 
library(MASS)

# Read data- Framingham file 
CHD <- read.csv("framingham.csv") 
str(CHD) 
head(CHD)

# No need to worry about factors for Logistic Regression 
# Split into train and test 
set.seed(142) 
split = sample.split(CHD$TenYearCHD, SplitRatio = 0.7) 
CHD.train <- filter(CHD, split==TRUE) 
CHD.test <- filter(CHD, split==FALSE) 

# How many people have had CHD ?
table(CHD.train$TenYearCHD) 
table(CHD.test$TenYearCHD) 

# Baseline Model: predict that no one will have CHD in 10 years

#training baseline accuracy 
baseline_accuracy_tr = 2171/(2171+390) 
#test baseline accuracy 
baseline_accuracy_ts = 930/(930+167)

# Fit logistic regression model
model <- glm(TenYearCHD ~ male + age + education + currentSmoker + 
               cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + 
               diabetes + totChol + sysBP + diaBP + BMI + heartRate + 
               glucose, data=CHD.train,family = "binomial") 

summary(model)

#test set predictions
test.pred = predict(model, newdata=CHD.test, type="response") 
summary(test.pred) 

#confusion matrix, threshold = 0.16 
table(CHD.test$TenYearCHD, test.pred > 0.16) 

# predict a single value for following clinic patient: 
# Female, age 51, college education, currently a smoker with an average of 
# 20 cigarettes per day. Not on blood pressure medication, has not had stroke, 
# but has hypertension. Not diagnosed with diabetes; total Cholesterol at 
# 220. Systolic/diastolic blood pressure at 140/100, BMI at 31, heart rate 
# at 59, glucose level at 78. 


CHD.obs <- data.frame(male=0, age=51, education = 'College', currentSmoker = 1, 
                      cigsPerDay = 20, BPMeds = 0, prevalentStroke = 0, prevalentHyp =1, 
                      diabetes =0, totChol=220, sysBP=140, diaBP=100, BMI=31, 
                      heartRate=59, glucose=78) 
predict(model, newdata=CHD.obs, type="response") 

# ROC curve 
rocr.log.pred <- prediction(test.pred , CHD.test$TenYearCHD) 
logPerformance <- performance(rocr.log.pred, "tpr", "fpr") 
plot(logPerformance, colorize = TRUE) 
abline(0, 1)

as.numeric(performance(rocr.log.pred, "auc")@y.values)










