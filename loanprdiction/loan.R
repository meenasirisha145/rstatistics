library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('caTools') # Spliting data
library('rms') # For step wise regression
getwd()
setwd("F:/rWork/rProjects/rstatistics/loanprdiction")
data=read.csv("train_data.csv",stringsAsFactors = F)
data
length(data$Gender)
head(data)
str(data)
colSums(is.na(data))

#replacing na values
table(data$LoanAmount)
data$LoanAmount[is.na(data$LoanAmount)]=144
table(data$LoanAmount)

table(data$Loan_Amount_Term)
data$Loan_Amount_Term[is.na(data$Loan_Amount_Term)]=360
table(data$Loan_Amount_Term)

table(data$Credit_History)
data$Credit_History[is.na(data$Credit_History)]=1
table(data$Credit_History)

data$Gender[data$Gender==' ' ]='M'
apply(data,2, function(x) length(unique(x)))


