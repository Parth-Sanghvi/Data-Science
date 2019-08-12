library(dplyr) 
library(ggplot2) 
library(caTools) # splits 
library(GGally) 
library(ROCR) 
library(MASS) 
library(rpart) # CART 
library(rpart.plot) # CART plotting 
library(caret)


Letters = read.csv('Letters242.csv') 
str(Letters) 

## a) Exploratory Data Analysis 

# Bar Chart: 
ggplot(data = Letters) +  geom_bar(mapping = aes(x= letter)) 

# we can see that there is almost the same number of letters , so no major unbalancing 
# we are interested in seeing each variable's distribution, so we look at the diagonal: 
ggscatmat(Letters, columns = 2:17, alpha = 0.8) 

#density <- density(Letters$letter) 
#plot(density) 

#Box Plots 
ggplot(data = Letters, mapping = aes(x = letter, y = xbox)) +  geom_boxplot() 
ggplot(data = Letters, mapping = aes(x = letter, y = ybox)) +  geom_boxplot()

## b) 
Letters$isB = as.factor(Letters$letter == "B") 
str(Letters) 
set.seed(456)

# standard split data 

train.ids = sample(nrow(Letters), 0.65*nrow(Letters)) 
Letters.train = Letters[train.ids,] 
Letters.test = Letters[-train.ids,] 

# b) i) 
#baseline: 

table(Letters.train$isB) 

# We have 1532 instances of isNotB and 493 of isB in training set 
# thus predict: isNotB all the time 

#On test set, we have 818 isNotB and 273 isB 
table(Letters.test$isB) 
#Baseline accuracy on test  = 0.749: 
818/(818+273) 



## b) ii) 
#Logistic Regression model 
log_model = glm(isB ~ xbox + ybox + width + height + onpix +
                  xbar + ybar + x2bar + y2bar + xybar + x2ybar +
                  xy2bar + xedge + xedgeycor + yedge + yedgexcor, data = Letters.train, family = "binomial") 
summary(log_model) 

#predict on TestSet 
log_predTest = predict(log_model, newdata = Letters.test, type = "response") 

#get Accuracy = 0.943 
table(Letters.test$isB, log_predTest > 0.5) 
log_accuracy = (792+237)/(792+237+26+36) 

## b) iii) 
# ROC curve and AUC = 0.978 
rocr.log.pred <- prediction(log_predTest, Letters.test$isB) 
logPerformance <- performance(rocr.log.pred, "tpr", "fpr") 
plot(logPerformance, colorize = TRUE) 
abline(0, 1) 
as.numeric(performance(rocr.log.pred, "auc")@y.values)


## b) iv) CART model, we obtain cp = 0.01 
train.cart = train(isB ~ xbox + ybox + width + height + onpix +
                     xbar + ybar + x2bar + y2bar + xybar + x2ybar +
                     xy2bar + xedge + xedgeycor + yedge + yedgexcor,
                   data = Letters.train, method = "rpart",tuneGrid = data.frame(cp=seq(0, 0.1, 0.005)),
                   trControl = trainControl(method="cv", number=10),
                   metric = "Accuracy") 

train.cart$results 
train.cart 

# plot the results 
ggplot(train.cart$results, aes(x=cp, y=Accuracy)) + geom_point(size=3) +  xlab("Complexity Parameter (cp)") + geom_line() 

# Extract the best model and make predictions 
train.cart$bestTune cart_final = train.cart$finalModel 
prp(cart_final, digits=3) 

#no need for model matrix because no factor variables in data, all continuous 

pred = predict(cart_final, newdata=Letters.test, type="class") 
table(Letters.test$isB, pred) 

# accuracy = 0.933 (790+228)/(790+228+45+28) 

## b) v) Random Forest using default values 

mod.rf <- randomForest(isB ~ xbox + ybox + width + height + onpix +
                         xbar + ybar + x2bar + y2bar + xybar + x2ybar +
                         xy2bar + xedge + xedgeycor + yedge + yedgexcor,
                       data = Letters.train) 
importance(mod.rf) 
pred.rf <- predict(mod.rf, newdata = Letters.test) 

#Get Accuracy = 0.977 table(Letters.test$isB, pred.rf) 
(811+255)/(811+255+7+18) 

## c) i) Baseline
# get most represented category: A with 525 

table(Letters.train$letter) 

#check the same on test and assess baseline performance 
table(Letters.test$letter) 
(264)/(264+273+283+271) 

## c) ii) 
train.cart_2 = train(letter ~ xbox + ybox + width + height + onpix +
                       xbar + ybar + x2bar + y2bar + xybar + x2ybar +
                       xy2bar + xedge + xedgeycor + yedge + yedgexcor ,
                     data = Letters.train, method = "rpart",
                     tuneGrid = data.frame(cp=seq(0, 0.1, 0.005)),
                     trControl = trainControl(method="cv", number=10),
                     metric = "Accuracy") 

train.cart_2 = train(letter ~ xbox + ybox + width + height + onpix +
                       xbar + ybar + x2bar + y2bar + xybar + x2ybar +
                       xy2bar + xedge + xedgeycor + yedge + yedgexcor ,
                     data = Letters.train,
                     method = "rpart",
                     tuneGrid = data.frame(cp=seq(0, 0.03, 0.001)),
                     trControl = trainControl(method="cv", number=10),
                     metric = "Accuracy") 

train.cart_2$results 
train.cart_2 


# plot the results 
ggplot(train.cart_2$results, aes(x=cp, y=Accuracy)) + geom_point(size=3) +  xlab("Complexity Parameter (cp)") + geom_line() 

# Extract the best model and make predictions 

train.cart_2$bestTune cart_final_2 = train.cart_2$finalModel 
prp(cart_final_2, digits=3) 

#no need for model matrix because no factor variables in data, all continuous 

pred_2 = predict(cart_final_2, newdata=Letters.test, type="class") 
table(Letters.test$letter, pred_2) 

# accuracy = (258+240+268+237)/(nrow(Letters.test)) 

## c) iii) Random Forests
mod_2.rf <- randomForest(letter ~ xbox + ybox + width + height + onpix +
                           xbar + ybar + x2bar + y2bar + xybar + x2ybar +
                           xy2bar + xedge + xedgeycor + yedge + yedgexcor,
                         data = Letters.train) 

importance(mod_2.rf) 

pred_2.rf <- predict(mod_2.rf, newdata = Letters.test) 
summary(mod_2.rf) 

#Get Accuracy = 0.977 
table(Letters.test$letter, pred_2.rf) 
(263+262+280+260)/(1091) 

## c) iv) Random Forests: with CV 

train.rf <- train(letter ~ xbox + ybox + width + height + onpix +
                    xbar + ybar + x2bar + y2bar + xybar + x2ybar +
                    xy2bar + xedge + xedgeycor + yedge + yedgexcor,
                  data = Letters.train,
                  method = "rf",
                  tuneGrid = data.frame(mtry=1:16),
                  trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                  metric = "Accuracy") 

train.rf$results 
train.rf 

ggplot(train.rf$results, aes(x = mtry, y = Accuracy)) + 
  geom_point(size = 3) +   ylab("Accuracy") + theme_bw() + 
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18)) 

best.rf <- train.rf$finalModel 
pred.best.rf <- predict(best.rf, newdata = Letters.test) 

table(Letters.test$letter, pred.best.rf) 

#accuracy = 0.978 (263+262+280+262)/(1091)

## c) v) Boosting 

mod.boosting <- gbm(letter ~ xbox + ybox + width + height + onpix +
                      xbar + ybar + x2bar + y2bar + xybar + x2ybar +
                      xy2bar + xedge + xedgeycor + yedge + yedgexcor,
                    data = Letters.train, distribution = 'multinomial', n.trees = 3300,interaction.depth = 10) 

pred.boosting <- predict(mod.boosting, newdata = Letters.test, n.trees=3300, type = 'response') 
p.pred.boosting <- apply(pred.boosting, 1, which.max)
table(Letters.test$letter, p.pred.boosting) 

#accuracy : 0.982 (262+264+280+266)/(1091)