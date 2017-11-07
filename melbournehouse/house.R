library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('caTools') # Spliting data
library('rms') # For step wise regression

getwd()
setwd("F:/rWork/rProjects/rstatistics/melbournehouse")
dataset=read.csv("Melbourne_house.csv",stringsAsFactors = F)
dataset
str(dataset)
colSums(is.na(dataset))
#replacing NA values of bedroom2 column
table(dataset$Bedroom2)
dataset$Bedroom2[is.na(dataset$Bedroom2)]=median(dataset$Bedroom2)
table(dataset$Bedroom2)

#replacing NA values of Bathroom
table(dataset$Bathroom)
dataset$Bathroom[is.na(dataset$Bathroom)]=median(dataset$Bathroom)
table(dataset$Bathroom)

pairs(dataset)

#replacing NA values of Car
