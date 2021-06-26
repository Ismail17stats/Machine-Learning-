# Libraries Needed
library(caret)
library(glmnet)
library(mlbench)
library(psych)

# Data
data("BostonHousing")
data <- BostonHousing


# We want to create a prediction model for "medv" as a function of other variables 

### When viewing colinarity between variables,  Exclude the factor variable "chas" and the response variable "medv"

pairs.panels(data[c(-4,-14)], cex = 2)



### Partitioning
set.seed(1234) 

index = sample(1:nrow(data), 0.7*nrow(data)) 

train = data[index,] # Create the training data 
test = data[-index,] # Create the test data

dim(train)
dim(test)

#### simple linear regression 
set.seed(1234)
model1 = lm(medv ~ ., data = train)
summary(model1)

### simple linear regression with cross validation 
## Custom Control Parameters
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = T)

model2 <- train(medv ~., train, method = 'lm', trControl = custom)
model2

### Ridge regression 
set.seed(1234)
model3 <- train(medv ~., 
                train,
                method = 'glmnet',
                tuneGrid = expand.grid(alpha = 0 , lambda = seq(0.0001, 1, length = 5)),
                trControl = custom)
model3


### Lasso regression 
set.seed(1234)

model4 <- train(medv ~., 
                train,
                method = 'glmnet',
                tuneGrid = expand.grid(alpha = 1 , lambda = seq(0.0001, 0.2, length = 5)),
                trControl = custom)

model4


### Elastic Net regression 

set.seed(1234)
model5 <- train(medv ~., 
                train,
                method = 'glmnet',
                tuneGrid = expand.grid(alpha = seq(0, 1, length = 10) , lambda = seq(0.0001, 1, length = 5)),
                trControl = custom)

model5


### Compare models

model_list <- list(Linear_model = model2, Ridge = model3 , Lasso = model4 , ElastNet = model5)
res <- resamples(model_list)
summary(res)


### Use best model for predictions 

model5$bestTune

best <- model5$finalModel

coef(best, s = model5$bestTune$lambda)

pred1 <- predict(model5,train)

sqrt(mean((train$medv-pred1)^2)) #### RMSE 

#for test data 
pred2 <- predict(model5,test)

sqrt(mean((test$medv-pred2)^2)) #### RMSE 
