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
