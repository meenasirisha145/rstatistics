data("iris")
iris
dataset=iris
View(dataset)
colnames(dataset)
library(caTools)
set.seed(1234)
split=sample.split(dataset$Species,SplitRatio =0.8)
trainingdata=subset(dataset,split=TRUE)
testdata=subset(dataset,split=FALSE)
dim(trainingdata)
str(trainingdata)
summary(trainingdata)
levels(trainingdata$Species)
#BOXPLOT 
par(mfrow=c(1,4))
for(i in 1:4){
  boxplot(trainingdata[,i],main=names(trainingdata)[i])
}
classifier=glm(iris$Species~.,family=binomial,data = trainingdata)
classifier
summary(classifier)
attach(iris)
classifier=glm(Species~Sepal.Length+Sepal.Width+Petal.Length,family=binomial,data=trainingdata)
summary(classifier)
classifier=glm(Species~Sepal.Length+Sepal.Width,family=binomial,data=trainingdata)
summary(classifier)
classifier=glm(Species~Sepal.Length,family=binomial,data=trainingdata)
summary(classifier)
plot(iris$Species,iris$Sepal.Length,data=iris)
scatter.smooth(iris$Species,iris$Sepal.Length)
