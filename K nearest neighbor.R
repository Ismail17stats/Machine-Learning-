getwd()
data <- read.table(file.choose(), sep = ',')
data <- data[,-1] ### exculde ID 

### Function to Normalize data 

data_norm <- function(x){
  ((x - min(x))/ (max(x)-min(x))) ### creation a range between 0 and 1
}

new_data <- lapply(data[,-1],data_norm) # get rid of the factor fariable B & M 
new_data <- as.data.frame(new_data) # get this as data frame 

### create training and testing sets 

### Partitioning
set.seed(1234) 

index = sample(1:nrow(new_data), 0.7*nrow(new_data)) 

train = new_data[index,] # Create the training data 
test = new_data[-index,] # Create the test data

# First method using class library 
library(class)
nrow(train)
pred1 <- knn(train,test, data[1:nrow(train),1], k = 20)

tab1 <- table(pred1, data[1:nrow(test),1])
library(caret)
confusionMatrix(tab1)


# Second method using knn library 

library(kknn)

#reattach factor to dataset 

dat1 <- cbind(data[,1], new_data)
names(dat1)[1] <- "Type" # just renaming the column 

#set up train and test 
n=dim(dat1)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.75))
train=dat1[id,]

id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
test=dat1[id2,]







modelK20 <- kknn(as.factor(Type)~.,train = train, test = train,k=20, kernel= "rectangular")

#Train
fit_K20<- fitted(modelK20)


misclass_rate_20train<-(sum(fit_K20!= train$Type)*100/nrow(train))
table(actual_Y=train[,1],calculated_Y=fit_K20)
cat("Missclassification rate",misclass_rate_20train)

### we got better result using the knn library and after fitting the model 

