sup1=c(18.5,24,17.2,19.9,18)
sup2=c(26.3,25.3,24,21.2,24.5)
sup3=c(20.6,24.2,20.8,24.7,22.9)
sup4=c(25.4,19.9,22.6,17.5,20.4)
df=data.frame(sup1,sup2,sup3,sup4)
df
colMeans(df)
colSums(df)
addmargins(df,c(1,2),FUN = list(list(mean,sd,length),list(length)))
stack(df)           
unstack(stack(df))        
dfstack=stack(df)
str(dfstack)
aggregate(dfstack$values,by=list(dfstack$ind),FUN = mean)
aggregate(dfstack$values,by=list(dfstack$ind),FUN = sd)
fit2w=aov(values~ind,data=dfstack)
fit2w
summary(fit2w)
qf(p=0.95,df1=3,df2=16)
boxplot(dfstack$values)
boxplot(df)
boxplot(values~ind,data=dfstack)
#we can see the outliers from the boxplot
gplots::plotmeans(values~ind,data=dfstack)
abline(h=19.5)
abline(h=colMeans(df))
#from the graph we can say that the means are not same
TukeyHSD(fit2w)
plot(TukeyHSD(fit2w))
#from the graph we can say that there is a large difference between sup2 and sup1
library(multcomp)
tuk=glht(fit2w,linfct=mcp(ind='Tukey'))
tuk
plot(cld(tuk,level=0.05))
plot(cld(tuk,level=0.01))


car::qqPlot(fit2w,simulate=T,labels=F)
#it is to check the noramlity of the dependent variable

bartlett.test(values~ind,data=dfstack)#to check the variance equality
#since the p value is > 0.05,the variances are equal

#outlier Test
car::outlierTest(fit2w)
