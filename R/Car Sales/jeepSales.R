library(dplyr) 
library(ggplot2) 
library(GGally) 
library(caTools) 
library(ROCR)
library(MASS)
library(car)
library(broom)

wrangler <- read.csv("Wrangler242-Spring2019.csv") 

str(wrangler)
head(wrangler)

# PART 1

# Iteration 1
ggscatmat(wrangler, columns = 4:length(wrangler), alpha = 0.8) 
wrangler.train <- filter(wrangler, Year <= 2015) 
wrangler.test <- filter(wrangler, Year > 2015) 
model1 <- lm(WranglerSales ~ Unemployment + WranglerQueries + CPI.Energy + CPI.All, data = wrangler.train) 
summary(model1) 
vif(model1)

# Iteration 2 Remove CPI.All 

model2 <- lm(WranglerSales ~ Unemployment + WranglerQueries + CPI.Energy, data = wrangler.train) 
summary(model2) 
vif(model2)

# Iteration 3 Remove Unemployment

model3 <- lm(WranglerSales ~ WranglerQueries + CPI.Energy, data = wrangler.train) 
summary(model3) 
ggcoef(  model2,  vline_color = "yellow",  vline_linetype =  "solid",  errorbar_color = "blue",
         errorbar_height = .75,  exclude_intercept = TRUE  )

# PART 2

model4 <- lm(WranglerSales ~ MonthFactor + Unemployment + WranglerQueries + CPI.Energy + CPI.All, data = wrangler.train) 
summary(model4) 
ggcoef(  model4,  vline_color = "yellow",  vline_linetype =  "solid",  errorbar_color = "blue",  
         errorbar_height = .75,  exclude_intercept = TRUE  )

# PART 3

# Removing Unemployment from model 4 with Categorical Var

model5 <-lm(WranglerSales ~ MonthFactor + WranglerQueries + CPI.Energy, data = wrangler.train) 
vif(model5) 
summary(model5)

# Compute OSR^2 for model 5

WranglerTestPredictions <- predict(model5, newdata=wrangler.test) 
SSE = sum((wrangler.test$WranglerSales - WranglerTestPredictions)^2) 
SST = sum((wrangler.test$WranglerSales - mean(wrangler.test $WranglerSales))^2) 
OSR2 = 1 - SSE/SST 
 