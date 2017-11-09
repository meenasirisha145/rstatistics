data(women)
women
names(women)
names(women)=c("h","w")
attach(women)
fit=lm(w~h,data=women)
fit
W=predict(fit)
W
women$W=W
women
W=round(fitted(fit),2)
W
women$W=W
women
R=round(residuals(fit),2)
R
#R=round(w-W,2)
#women$R=round(w-W,2)
#women
cbind(w,h,W,R)
women$R=R
women
plot(fit)
summary(fit)
plot(y=R,x=W)


