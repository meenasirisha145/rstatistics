df=read.csv(file.choose())
str(df)
View(df)
names(df)
#df_final=df[,-c(1,31,32)]
str(df)
dfg3=df[,-c(1)]
attach(dfg3)
#evaluate a full model
fitg3=lm(G3~.,data=dfg3)
summary(fitg3)
#predict G3
#model exists
null=lm(G3~1, data=dfg3)
full=lm(G3~., data=dfg3)
#most appropriate model according to AIC
step(null, scope=list(lower=null, upper=full), direction="forward")

fitg3=lm(formula = G3 ~  famrel +G2  + G1 + age + activities + 
            Walc + romantic, data = dfg3)

summary(fitg3)

#fitg31=lm(formula = G3 ~  famrel +G2  + G1 + age + activities + 
 #          Walc  ,data = dfg3)

#summary(fitg31)
#anova(fitg31,fitg3)
#AIC(fitg3,fitg31)
#conclusion depends on famrel,G2,G1,age,activities,Walc
#now evaluate G1
#dfg1=df[,c("g3")]
#Evaluate G1 grades

dfg1=dfg3[,-c(31,32)]
names(dfg1)
null=lm(G1~1, data=dfg1)
full=lm(G1~., data=dfg1)

step(null, scope=list(lower=null, upper=full), direction="forward")

fitg1=lm(formula = G1 ~  schoolsup + Fjob + studytime + famsup +goout+sex 
     failures + Walc + Mjob + health+higher+freetime, data = dfg1)

summary(fitg1)

dfg2=dfg3[,-c(32)]
dfg2
null=lm(G2~1, data=dfg2)
full=lm(G2~., data=dfg2)
step(null, scope=list(lower=null, upper=full), direction="forward")
fitg2=lm(formula = G2 ~ G1 + age + paid + romantic + traveltime + internet + 
           schoolsup + Fjob + famrel + Pstatus + reason, data = dfg2)

summary(fitg2)







library(dplyr)
library(caTools)

set.seed(123)

split = sample.split(dfg3, SplitRatio = 0.80)

training_set = subset(dfg3, split == TRUE)
test_set = subset(dfg3, split == FALSE)
str(training_set)
str(test_set)
fullmodel=lm(formula = G3 ~  famrel +G2  + G1 + age + activities + 
               Walc + romantic, data = training_set)
library(ROCR)
g3pred = predict(fullmodel, type = 'response', 
                 newdata = test_set)
g3pred[g3pred<=0]=0
error=test_set$G3-g3pred
rmse=sqrt(mean(error*error))
rmse
plot(fullmodel,which=1)

car::outlierTest(fullmodel)

df[c(265,335,141,311,260,297),c("G3","G1","G2")]
dfg3=dfg3[-c(265,335,141,311,260,297),]

fullmodel1=lm(formula = G3 ~  famrel +G2  + G1 + age + activities + 
               Walc + romantic, data = training_set)
plot(fullmodel1,which=1)
car::outlierTest(fullmodel1)
car::durbinWatsonTest(fullmodel1)

library(relaimpo)
imp=calc.relimp(fullmodel1,type="lm",rela=TRUE)
