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

data$Gender[data$Gender== ""]='M'
apply(data,2, function(x) length(unique(x)))
unique(data$Gender)

unique(data$Self_Employed)
data$Self_Employed[data$Self_Employed==""]="Yes"
unique(data$Self_Employed)



apply(data,2, function(x) length(unique(x)))
attach(data)
cols=c("Loan_Status","Married","Gender","Credit_History","Self_Employed")
for (i in cols){
  data[,i]=as.factor(data[,i])
}
set.seed(123)

split = sample.split(data$Loan_Status, SplitRatio = 0.80)

training = subset(data, split == TRUE)
test = subset(data,split == FALSE)

# Structure of the data
str(training)
attach(training)
#model1=fastbw(lrm(Loan_Status~Application_ID+ Gender+ Married +Dependents  + Education +Self_Employed +ApplicantIncome
 #                + CoapplicantIncome+LoanAmount  ),rule="p")
#model1


model=glm(Loan_Status~Application_ID+ Gender+ Married +Dependents  + Education +Self_Employed +ApplicantIncome
          + CoapplicantIncome+LoanAmount  +Loan_Amount_Term +Credit_History +Property_Area  
           ,data=training,family=binomial)
summary(model)

#removing property area
model=glm(Loan_Status~Application_ID+ Gender+ Married +Dependents  + Education +Self_Employed +ApplicantIncome
          + CoapplicantIncome+LoanAmount +Credit_History +Loan_Amount_Term   
          ,data=training,family=binomial)
summary(model)
#removing loanamountterm
model=glm(Loan_Status~Application_ID+ Gender+ Married +Dependents  + Education +Self_Employed +ApplicantIncome
          + CoapplicantIncome+LoanAmount +Credit_History,data=training,family=binomial)
summary(model)
#removing credithistory
model=glm(Loan_Status~Application_ID+ Gender+ Married +Dependents  + Education +Self_Employed +ApplicantIncome
          + CoapplicantIncome+LoanAmount ,data=training,family=binomial)
summary(model)
#removing self employed
model=glm(Loan_Status~Application_ID+ Gender+ Married +Dependents  + Education +ApplicantIncome
          + CoapplicantIncome+LoanAmount ,data=training,family=binomial)
summary(model)

#removing married
model=glm(Loan_Status~Application_ID+ Gender+Dependents  + Education +ApplicantIncome
          + CoapplicantIncome+LoanAmount ,data=training,family=binomial)
summary(model)

#removing dependents
model=glm(Loan_Status~Application_ID+ Gender+ Education +ApplicantIncome
          + CoapplicantIncome+LoanAmount ,data=training,family=binomial)
summary(model)

#removing education
model=glm(Loan_Status~Application_ID+ Gender+ ApplicantIncome
          + CoapplicantIncome+LoanAmount ,data=training,family=binomial)
summary(model)

#removing gender
model=glm(Loan_Status~Application_ID+  ApplicantIncome
          + CoapplicantIncome+LoanAmount ,data=training,family=binomial)
summary(model)
