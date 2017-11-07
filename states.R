
state.x77
str(state.x77)
class(state.x77)
states=as.data.frame(state.x77)
states
attach(states)
fit=lm(Murder ~ . , data=states)
summary(fit)
str(states)
fit1=lm(Murder~Population + Income + Illiteracy+Frost,data=states)
fit1
anova(fit1,fit)
names(states)
states$`Life Exp`
fit2=lm(Murder~Population+Illiteracy,data=states)
summary(fit2)
anova(fit2,fit1)
summary(anova(fit1,fit))
summary(anova(fit2,fit1))
AIC(fit1,fit2)
AIC(fit,fit1)
