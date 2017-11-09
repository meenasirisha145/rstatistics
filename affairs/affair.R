library(AER)
data(Affairs)
Affairs
names(Affairs)
data=Affairs
summary(data)
str(data)
dim(data)
attach(data)
table(affairs)
prop.table(table(affairs))
prop.table(table(gender))
prop.table(table(children))
prop.table(table(children))*100
data$ynaffair[data$affairs>0]=1
data$ynaffair[data$affairs==0]=0
data
data$ynaffair=factor(data$ynaffair,levels = c(0,1),labels = c('No','Yes'))
table(data$ynaffair)


