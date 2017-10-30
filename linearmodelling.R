library(MASS)
names(Boston)
library(ISLR)
plot(medv~lstat,Boston)
Boston
fit1=lm(medv~lstat,data=Boston)
fit1
summary(fit1)
abline(fit1,col="red")
names(fit1)
confint(fit1)
predict(fit1,data.frame(lstat=c(5,10,15)),interval = "confidence")
predict(fit1,data.frame(lstat=c(5,10,15)))#doesnot shows confidence intervals

##multiple linear regression-----
fit2=lm(medv~lstat+age,data=Boston)
fit2
summary(fit2)

fit3=lm(medv~.,data=Boston)
summary(fit3)

fit4=update(fit3,~.-age-indus)#removing the variables that are not significant
fit4
summary(fit4)


#nonlinear terms and interaction----
fit5=lm(medv~lstat*age,Boston)
fit5
summary(fit5)

#polynomial function-----
fit6=lm(medv~lstat+I(lstat^2),Boston)
fit6
summary(fit6)
plot(medv~lstat,Boston)
points(Boston$lstat,fitted(fit6),col="red")
attach(Boston)
fit7 = lm(medv~poly(lstat,4))
points(lstat,fitted(fit7),col="blue",pch=20)
plot(1:20,1:20,pch=1:20,cex=2) 

#Qualitative Predictors
fix(Carseats)
names(Carseats)
summary(Carseats)
fit1 = lm(Sales~.+Income:Advertising+Age:Price,Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc)

#Writing R functions
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}

attach(Carseats)
regplot(Price,Sales)

regplot=function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}

regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)
