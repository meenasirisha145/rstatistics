mtcars
mtcars.lm=lm(mpg~1,data=mtcars)
mtcars.lm
summary(mtcars.lm)
add1(mtcars.lm,mtcars,test = 'F')
mtcars.lm=lm(mpg~wt,data=mtcars)
mtcars.lm
summary(mtcars.lm)
add1(mtcars.lm,mtcars,test = 'F')
mtcars.lm=lm(mpg~wt+cyl,data=mtcars)
mtcars.lm
summary(mtcars.lm)
add1(mtcars.lm,mtcars,test = 'F')
#after this no variables are significant
fitfull=lm(mpg~wt+cyl+hp+am+disp,data = mtcars)
fitfull
summary(fitfull)
drop1(fitfull,test='F')
drop1(update(fitfull,~.-disp),test='F')

drop1(fitfull,test='F')
drop1(update(fitfull,~.-disp-am),test='F')
drop1(fitfull,test='F')
drop1(update(fitfull,~.-disp-am-hp),test='F')

fit1=lm(mpg~wt,data = mtcars)
fit1
summary(fit1)
fit2=lm(mpg~wt+cyl,data=mtcars)
fit2
summary(fit2)

anova(fit1,fit2)
#here p value is less than 0.05 so we reject the hypothesis that fit1 is better than fit2
#so fit2 is the better model


coefficients(fit2)
confint(fit2,level = 0.90)
fitted(fit2)
cbind(mtcars$mpg,fitted(fit2))
residuals(fit2)
anova(fit2)
vcov(fit2)
influence(fit2)
plot(fit2)


library(MASS)
fit=lm(mpg~wt+cyl+hp+am,data=mtcars)
step=stepAIC(fit,direction = "both")
step$anova


#LEAPS Command
data=mtcars
data
library(leaps)#gives the n best models
leaps=regsubsets(mpg~wt+cyl+hp+am,data=data,nbest=10)
summary(leaps)
plot(leaps,scale="r2")


library(car)
subsets(leaps,statistic = "rsq")

library(relaimpo)
calc.relimp(fit,type = c("lmg","last","first","pratt"),rela=TRUE)
boot=boot.relimp(fit,b=1000,type=c("lmg","last","first","pratt"),rank=TRUE,diff=TRUE,rela=TRUE)

booteval.relimp(boot)
plot(booteval.relimp(boot,sort=TRUE))#to check the relative importance of each variable in the model


#LOGISTIC REGRESSION
dataset=mtcars
t=table(mtcars$am)
amt=addmargins(t)
amt
ppt=prop.table(t)
ppt
#MODEL1
base=glm(am~1,data=mtcars,family = binomial())
summary(base)
odds=exp(-0.3795)
odds
#MODEL2
fit1=glm(am~mpg+disp+hp+wt,data=dataset,family = binomial())
summary(fit1)
plot(fit1)
layout(matrix(c(1,2,3,4),2,2))

#MODEL3
fit2=glm(am~mpg+hp+wt,data=dataset,family = binomial())
summary(fit2)

#model4
fit3=glm(am~hp+wt,data=dataset,family=binomial())
summary(fit3)
#model5
fit4=glm(am~hp+wt+cyl,data=dataset,family=binomial())
summary(fit4)

probpred=predict(fit3,type="response",newdata = dataset[,c("hp","wt")])
probpred
probpred=ifelse(probpred>0.5,1,0)
table(probpred)
x=table(dataset$am,probpred)
x
accuracy=(18+12)/length(dataset$am)
accuracy

#checking for the sample
probpred=predict(fit3,type="response",newdata = data.frame(wt=2,hp=150))
probpred
v=(18.86+(0.0362*150)-(8.083*2))
(p=exp(v)/(1+exp(v)))

caret::confusionMatrix(x)
car::vif(fit3)
sqrt(car::vif(fit3))>2
car::vif(fit2)
sqrt(car::vif(fit2))>2
