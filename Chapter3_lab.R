##  Lab 3.6.3 Multiple Linear Regression.  

library(MASS)
library(ISLR)

attach(Boston)

lm.fit=lm(medv~lstat+age)
summary(lm.fit)

lm.fit_2=lm(medv~.,data=Boston)
summary(lm.fit_2)

library(car)
vif(lm.fit_2)

##  Lab 3.6.4 Interaction Terms.  

summary(lm(medv~lstat*age,data=Boston))

##  Lab 3.6.5 Non-linear Transformations of the Predictors.  

lm.fit2=lm(medv~lstat+I(lstat^2))

summary(lm.fit2)

lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)

x11()
par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)

log.fit_rm=lm(medv~log(rm),data=Boston)
summary(log.fit_rm)

##  Lab3.6.6 Qualitative Predictors

fix(Carseats)
names(Carseats)
attach(Carseats)

lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)

contrasts(ShelveLoc)
?contrasts

## Lab 3.6.7 Writing Functions.  

LoadLibraries=function(){
    library(ISLR)
    library(MASS)
    print("The libraries have been loaded.")
}

LoadLibraries()
