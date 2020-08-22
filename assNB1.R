library(mlbench)

###train data 
train_salary <- read.csv(choose.files())
str(train_salary)
class(train_salary)

###test data
test_salary <- read.csv(choose.files())
str(test_salary)
class(test_salary)

library(e1071)
##naive bayes model
model <- naiveBayes(train_salary$Salary~.,data = train_salary)
model

pred <- predict(model,test_salary)
mean(pred == test_salary$Salary)##81.93%
confusionMatrix(pred,test_salary$Salary)
