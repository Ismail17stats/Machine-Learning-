library(olsrr)
getwd()
##Forward regression using p-values
load("regdata.RData")
model<-lm(achieve~mastery+interest+anxiety+perfgoal+genderid,
data=regdata)

FWDfit.p<-ols_step_forward_p(model,penter=.05) ## fitting the model 

#This gives us the short summary of the model at each step 
FWDfit.p

# Now we will use the AIC function 

FWDfit.aic <- ols_step_forward_aic(model)

#This gives us the short summary of the model at each step 
FWDfit.aic

# Selected the same variable as before 
# Check while showing the intermediate steps 

FWDfit.aic <- ols_step_forward_aic(model, details = TRUE)

## Now let's use backward regression using p-values

BWDfit.p <- ols_step_backward_p(model, penter =0.05, details = TRUE)
BWDfit.p

# Using p method, it did not removed the variable "genderid" which is considered to be not significant 
# AIC

BWDfit.aic <- ols_step_backward_aic(model, details = TRUE)

# We can see that AIC method examine all possible subsets and choose the best method 

## Stepwise Regression using p-values 

Bothfit.p <- ols_step_both_p(model, pent = .05, prem = .05) # conbination of both backward and forward regression
Bothfit.p

## Stepwise regression using aic

Bothfit.aic <- ols_step_both_aic(model)
Bothfit.aic

## All possible subset regression 

modcompare <- ols_step_all_possible(model)
modcompare ## a little messy 

as.data.frame(modcompare)

plot(modcompare)

### Selecting the best model 

modcompare <- ols_step_best_subset(model)
modcompare

plot(modcompare)
