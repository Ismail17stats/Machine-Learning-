#StatQuest !!!!

library(glmnet)
set.seed(12345)

### Setting up our data 

n <- 1000 # made up data set will have 1000 samples
p <- 5000 # parameters to estimate

real_p <- 15 # only 15 parameters will help us predict the outcome 

x <- matrix(rnorm(n*p), nrow = n,ncol = p)

# Now we will create a vector y that we will try to predict with the data in x

y <- apply(x[,1:real_p],1,sum)+rnorm(n) # call to apply will return a vector of values that depends on the first 15 columns in x 

# To summarize this first part:
### x is a matrix of data that we will use Ridge, Lasso and Elastic Net to predict the values in y

### Partitioning

index = sample(1:n, 0.7*n) 

x.train = x[index,] # Create the training data 
x.test = x[-index,] # Create the test data

y.train = y[index]
y.test = y[-index]

### Ridge regression 
# The first thing we need to do is fit a model to the training data 
# cv: using cross validation to find the optimal values for lambda 
# we want to use x.train to predict y.train
alpha0.fit <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha = 0
                        , family ="gaussian")
# now we will use the predict() to apply alpha0.fit to the testing data
# s: is the size of the penalty which is set to one of the optimal values for lambda stored in alpha0.fit
alpha0.predicted <- predict(alpha0.fit, s = alpha0.fit$lambda.1se, newx = x.test)

# NOTE: we are setting s to lambda.1se which is the value of lambda that resulted in 
# the simplest model(the model with fewest non zero parameters) and was withtin 1 standard error of the lambda
# that had the smallest sum.

# NOTE2: Alternatively we could set s to be lambda.min, which would be the lambda that resulted in the smallest sum
# However, in this example we will use lambda.1se because, in a statistical sense, it is indistinguishable from lambda.min, but it results in a model with 
# fewer parameters

# Okay let's move on 

# Now we calculate the mean squared error of the difference between the true values stored in y.test
# and the predicted values stored in alpha0.predicted

mean((y.test - alpha0.predicted)^2)# MSE

### Lasso regression
# just like before we call cv.glmnet() to fit a linear regression (10 fold cv by default) to determine the optimal value of lambda
alpha1.fit <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha = 1
                        , family ="gaussian")
# call the predict function just like before using alpha1.fit
alpha1.predicted <- predict(alpha1.fit, s = alpha1.fit$lambda.1se, newx = x.test)

# MSE

mean((y.test - alpha1.predicted)^2)

# We got a much smaller value than in Ridge, so Lasso regression is much better with this data than Ridge regression

### Elastic Net regression (which combines both Ridge and Lasso penalties)

alpha0.5.fit <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha = 0.5
                        , family ="gaussian")

alpha0.5.predicted <- predict(alpha0.5.fit, s = alpha0.5.fit$lambda.1se, newx = x.test)

# MSE

mean((y.test - alpha0.5.predicted)^2)
# slightly larger than the value we got from Lasso regression, so in this case Lasso wins 
# HOWEVER 
# we need to try a bunch of different values from alpha 
# I am going to create a list that will store a bunch of Elastic Net regression fits
list.of.fits <- list()
# create a for-loop to try different values for alpha and fit those values 
for(i in 0:10){
  fit.name <- paste0("alpha", i/10)
  
  list.of.fits[[fit.name]]<-
    cv.glmnet(x.train, y.train, type.measure = "mse", alpha = i/10, family = "gaussian")
  
}

# we will also create an empty data frame to store the MSE and other things 

results <- data.frame()

# Then another for-loop to predict values using the testing dataset in order to find the MSE

for(i in 0:10){
  fit.name <- paste0("alpha", i/10)
  
  predicted <- 
    predict(list.of.fits[[fit.name]], s = list.of.fits[[fit.name]]$lambda.1se, newx = x.test)
  
  mse <- mean((y.test - predicted)^2)
  
  temp <- data.frame(alpha = i/10, mse = mse, fit.name = fit.name)
  results <- rbind(results, temp)
}

results

## seems like Lasso still wins 