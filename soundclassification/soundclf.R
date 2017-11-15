library(caTools)
library(nnet)


getwd()

setwd( "F:/rWork/rProjects/rstatistics/soundclassification")
data=read.csv("train.csv")
test=read.csv("test.csv")
attach(data)
colSums(is.na(data))
multi=multinom(Class~ID,data=data)
summary(multi)

pred=predict(multi,test,"class")
test$predclass=pred
test


multi=multinom(ID~Class,data=data)
summary(multi)

pred=predict(multi,test,"class")
test$predclass=pred
test
