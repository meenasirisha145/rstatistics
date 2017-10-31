data(women)
women
nrow(women)
attach(women)
cor.test(weight,height,data=women)
class(women)
fit=lm(weight~height,data=women)
fit
summary(fit)#residuals =summary(yi-yh)
plot(height,weight,main="height Vs weight")
abline(fit,col="red")
text(height,weight,labels=paste0('(',height,',',weight,')'),col="blue")
fitted(fit)#predicted values


(fit2=lm(weight~height+I(height^2),data=women))
summary(fit2)
plot(height,weight)
lines(height,fitted(fit2),col="green")

fit3=lm(weight~height+I(height^2)+I(height^3),data=women)
fit3
summary(fit3)
plot(height,weight)
lines(height,fitted(fit3),col="blue")


library(car)
car::scatterplot(weight~height,data=women,spread=FALSE,lty.smooth=2,pch=19,main="women age 30-39",xlab="height(inches)",ylab="weight(lbs.)")
?car::scatterplot
