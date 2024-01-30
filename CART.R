library(caret) ### caret library has the confusion matrix
library(forecast) ### forecast library has the prediction errors
library(rpart) 
library(rpart.plot) ### rpart and rpart.plot libraries are for the tree analysis
library(randomForest) ### for random forest


##### Cereal example for classification tree

cereal_data<- read.csv("Cereals_classification.csv")

### Partition data first
train_index<-sample(rownames(cereal_data), dim(cereal_data)[1]*0.6)
valid_index<-setdiff(rownames(cereal_data),train_index)
train_data<-cereal_data[train_index, ]
valid_data<-cereal_data[valid_index, ]

### Train the model using the training data
mytree<- rpart(high_rating ~ calories + protein + fat + fiber + sugars + vitamins, data = train_data, method = "class")

prp(mytree)  ## plot the tree

### Predict using the validation data
predicted_values <- predict(mytree, newdata=valid_data, type = "class")

### Confusion matrix
confusionMatrix(relevel(as.factor(predicted_values), "1"), 
                relevel(as.factor(valid_data$high_rating), "1"))



### Random forest

## since we are doing a classification random forest now,
## we need to convert the outcome variable to factor (i.e., categorical) type
train_data$high_rating<-as.factor(train_data$high_rating) 
valid_data$high_rating<-as.factor(valid_data$high_rating) 

### Train the model using the training data
myforest <- randomForest(high_rating ~ calories + protein + fat + fiber + sugars + vitamins, data = train_data) 

### Predict using the validation data
predicted_values_forest <- predict(myforest, newdata=valid_data)

### Confusion matrix
confusionMatrix(relevel(as.factor(predicted_values_forest), "1"), 
                relevel(as.factor(valid_data$high_rating), "1"))




##### Cereal example for regression tree

cereal_data<- read.csv("Cereals_prediction.csv")

### Partition data first
train_index<-sample(rownames(cereal_data), dim(cereal_data)[1]*0.6)
valid_index<-setdiff(rownames(cereal_data),train_index)
train_data<-cereal_data[train_index, ]
valid_data<-cereal_data[valid_index, ]

### Train the model using the training data
mytree<- rpart(rating ~ calories + protein + fat + fiber + sugars + vitamins, data = train_data, method = "anova")

prp(mytree)  ## plot the tree

### Predict using the validation data
predicted_values <- predict(mytree, newdata=valid_data)

### Prediction performance
accuracy(predicted_values, valid_data$rating)


### Random forest

### Train the model using the training data
myforest <- randomForest(rating ~ calories + protein + fat + fiber + sugars + vitamins, data = train_data) 

### Predict using the validation data
predicted_values_forest <- predict(myforest, newdata=valid_data)

### Prediction performance 
accuracy(predicted_values_forest, valid_data$rating)