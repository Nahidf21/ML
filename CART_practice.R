library(caret) ### caret library has the confusion matrix
library(forecast) ### forecast library has the prediction errors
library(rpart) 
library(rpart.plot) ### rpart and rpart.plot libraries are for the tree analysis
library(randomForest) ### for random forest



######## Universal Bank practice

bank_data<- read.csv("UniversalBank.csv")


### Partition data first
train_index<- sample(rownames(bank_data), dim(bank_data)[1]*0.6)
valid_index<-setdiff(rownames(bank_data),train_index)
train_data<- bank_data[train_index, ]
valid_data<- bank_data[valid_index, ]

### Train the model using the training data
mytree<- rpart(Personal.Loan ~ Age + Income + Experience + CCAvg + Family + Mortgage + Securities.Account + CD.Account + Online + CreditCard, data = train_data, method = "class")

prp(mytree)  ## plot the tree

### Predict using the validation data
predicted_values <- predict(mytree,newdata=valid_data,type = "class")

### Confusion matrix
confusionMatrix(relevel(as.factor(predicted_values),"1"),
                relevel(as.factor(valid_data$Personal.Loan),"1"))


### Random forest

## since we are doing a classification random forest now,
## we need to convert the outcome variable to factor (i.e., categorical) type
train_data$Personal.Loan<-as.factor(train_data$Personal.Loan) 
valid_data$Personal.Loan<-as.factor(valid_data$Personal.Loan)

myforest <- randomForest(Personal.Loan ~ Age + Income + Experience + CCAvg + Family + Mortgage + Securities.Account + CD.Account + Online + CreditCard, data = train_data)

predicted_values_forest <- predict(myforest,newdata=valid_data)

confusionMatrix(relevel(as.factor(predicted_values_forest),"1"),
                relevel(as.factor(valid_data$Personal.Loan),"1"))




######## Toyota practice

toyota_data<- read.csv("ToyotaCorolla.csv")

### Partition data first
train_index<- sample(rownames(toyota_data), dim(toyota_data)[1]*0.6)
valid_index<-setdiff(rownames(toyota_data),train_index)
train_data<- toyota_data[train_index, ]
valid_data<- toyota_data[valid_index, ]

### Train the model using the training data
mytree<- rpart(Price ~ Age_08_04 + KM + Automatic + Weight + Doors + Gears + Guarantee_Period, data = train_data, method = "anova")

prp(mytree)  ## plot the tree

### Predict using the validation data
predicted_values <- predict(mytree, newdata=valid_data)

### Prediction performance 
accuracy(predicted_values, valid_data$Price)


### Random forest

myforest <- randomForest(Price~ Age_08_04 + KM + Automatic + Weight + Doors + Gears + Guarantee_Period, data = train_data)

predicted_values_forest <- predict(myforest,newdata=valid_data)

accuracy(predicted_values_forest, valid_data$Price)