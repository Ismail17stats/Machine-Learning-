getwd()

data <- read.csv("binary.csv", header = TRUE)

#Logistic regression model 

library(nnet)

model1 <- multinom(admit~., data = data)

#Confusion Matrix and Missclassification error 

p <- predict(model1, data)
tab <- table(p, data$admit)
#Confusion Matrix
tab
#ME

1-sum(diag(tab))/sum(tab)

# Is this a good classification ? 

table(data$admit)
# from the table we can see that 127 were admitted and 273 were not

# Model Performance Evaluation 

install.packages("ROCR")
library(ROCR)

pred <- predict(model1, data, type = 'prob')
head(pred)
head(data)
# first probability is 0.18 which is below the 0.5 cut off from the confusion table, and as we can see the first student was not admitted so it is a correct prediction 

# it is not the case however for the second student who was addmitted and given a prob of 0.31, so this is a classification error. 

pred <- prediction(pred, data$admit)
eval <- performance(pred, "acc")
plot(eval)

# Identify the best cutt off and frequency 

max <- which.max(slot(eval,"y.values")[[1]])
acc <- slot(eval,"y.values")[[1]][max]
cut <- slot(eval,"x.values")[[1]][max]
print(c(Accuracy = acc, Cutoff = cut))

# ROC Curve

roc <- performance(pred,"tpr", "fpr") # tpr = true positive rate 
plot(roc)
abline(a=0,b=1)

#AUC 
auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc
