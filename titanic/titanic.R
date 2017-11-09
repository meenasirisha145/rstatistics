library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('caTools') # Spliting data
library('rms') # For step wise regression
getwd()
setwd("F:/rWork/rProjects/rstatistics/titanic")

# Read Data----
dataset <- read.csv("train.csv",stringsAsFactors = F,na = "")
# First few entries of the data-----
head(dataset)
# check data - str is structure of the data-----
str(dataset)
# is there any Missing obesrvation-----
colSums(is.na(dataset))
# #Replace Embarked by most frequent observation------
table(dataset$Embarked)
dataset$Embarked[is.na(dataset$Embarked)]="S"
table(dataset$Embarked)
##As Age and Cabin have too many missing values, we will check it during the analysis Phase-----

#Check the length and see how many varibles can move to factor for our analysis-----

apply(dataset,2, function(x) length(unique(x)))

##Will convert the below varibles into factors for analysis-----
 
cols=c("Survived","Pclass","Sex","Embarked")
for (i in cols){
  dataset[,i]=as.factor(dataset[,i])
}
str(dataset)
# Exploratory analysis and Feature engineering
## Exploratory Analysis on Pclass: Are Rich people more likely to survive?

#Hypothesis is that, survival rate for **Rich** folks is much higher than **poor** people, Does any diffrence in the Titanic?-----  

#Visualize Pclass which is the best proxy for Rich and Poor.-----


ggplot(dataset,aes(x=Pclass,fill=factor(Survived))) +
  geom_bar()+
  ggtitle("Pclass v/s Survival Rate")+
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")  
#No diffrence in the Titanic too, First class Survival rate is far better than the third class.

## Exploratory Analysis on Gender: Are Female more likely to survive?----
#Hypothesis, **gender** and survival rate are related----

ggplot(dataset, aes(x = Sex, fill = Survived)) +
  geom_bar() +
  ggtitle("Gender vs survival") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")
#Female survival rate is much higher than Male.


#Visualize the 3-way relationship of sex, pclass, and survival----

ggplot(dataset, aes(x = Sex, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("3D view of sex, pclass, and survival") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")


## Exploratory Analysis on Name----

head(dataset$Name)


#Lets extract the title and check if we have predictive power in that - 
 
# Grab title from passenger names------
names <- dataset$Name
title <-  gsub("^.*, (.*?)\\..*$", "\\1", names)
dataset$Title <- title

# Show title counts by sex
table(dataset$Sex, dataset$Title)

#Miss, Mrs, Master and Mr are taking more numbers.
#Creating a new varible with small data can cause overfitting.
#Better to group Other titles into bigger basket by checking gender and survival rate to avoid any overfitting.
# Army folks, doctors and nobel people to be combined in one group - "Officer" level


officer <- c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer', 'the Countess')
officer
dataset$Title[dataset$Title %in% officer]  <- 'Officer'

# Also reassign mlle, ms, and mme accordingly------
dataset$Title[dataset$Title == 'Mlle']        <- 'Miss' 
dataset$Title[dataset$Title == 'Ms']          <- 'Miss'
dataset$Title[dataset$Title == 'Lady']         <- 'Miss' 
dataset$Title[dataset$Title == 'Dona']         <- 'Miss' 
dataset$Title[dataset$Title == 'Mme']         <- 'Mrs' 

# Show title counts by sex again-----
table(dataset$Sex, dataset$Title)

# Lets check Title vs survival rate------
ggplot(dataset,aes(x = Title,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("Title V/S Survival rate")+
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived") 

#So in the titanic, if you are a Mr then there is less chance of survival, Miss and Mrs have better survival rate than Master and Officer-----


# Visualize the 3-way relationship of Title, Pclass, and Survival-----

ggplot(dataset, aes(x = Title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("3-way relationship of Title, Pclass, and Survival") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")
#Since Title is mostly dependent on the Age (except few cases), I will use title as a proxy for age.

## Exploratory Analysis on Family : Do families sink or swim together?
#First we're going to make a family size variable based on number of siblings/spouse(s) (maybe someone has more than one spouse?) and number of children/parents.


# Create a family size variable including the passenger themselves-----
dataset$Fsize <- dataset$SibSp + dataset$Parch + 1
dataset
# Use ggplot2 to visualize the relationship between family size & survival-----
ggplot(dataset, aes(x = Fsize, fill = Survived)) +
geom_bar() +
ggtitle("Family Size V/S Survival Rate") +
xlab("FamilySize") +
ylab("Total Count") +
labs(fill = "Survived")


#We can see that there's a survival penalty to singletons and those with family sizes above 4.
#We can collapse this variable into three levels which will be helpful since there are comparatively fewer large families.
#Let's create a discretized family size variable.

# Discretize family size----
dataset$FsizeD[dataset$Fsize == 1] <- 'singleton'
dataset$FsizeD[dataset$Fsize < 5 & dataset$Fsize > 1] <- 'small'
dataset$FsizeD[dataset$Fsize > 4] <- 'large'

ggplot(dataset, aes(x = FsizeD, fill = Survived)) +
geom_bar() +
ggtitle("Family Size V/S Survival Rate") +
xlab("FamilySize") +
ylab("Total Count") +
labs(fill = "Survived")


#The plot shows that there is a survival penalty among singletons and large families-----

## Exploratory Analysis on Embarked-----

###is there any association between Survial rate and where we get into the Ship------  
ggplot(dataset,aes(x = Embarked,fill=factor(Survived))) +
geom_bar() +
ggtitle("Embarked vs Survival") +
xlab("Embarked") +
ylab("Total Count") +
labs(fill = "Survived") 


# Prediction-----

## Split into training & test sets-----

#Divide data into train and test set for internal validation-----

# Set a random seed-----
set.seed(123)

split = sample.split(dataset$Survived, SplitRatio = 0.80)

training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Structure of the data
str(training_set)


## Building the model------
#We then build our model using logistic regression on the training set.


# Build the model (note: note all possible variables are used)
LogReg_model <- glm(factor(Survived) ~ Pclass + Sex + SibSp + Parch + Fare + Embarked +Title + FsizeD, data = training_set, family = binomial)

summary(LogReg_model)



# Backward Elimination to choose the predictors - Iternation 2 - Sex removed-----
LogReg_model <- glm(factor(Survived) ~ Pclass + SibSp + Parch + Fare + Embarked + Title + FsizeD,
                    data = training_set, family = binomial)

summary(LogReg_model)



# Iteration 3 - Embarked Removed-----
LogReg_model <- glm(factor(Survived) ~ Pclass + SibSp + Parch + Fare +Title + FsizeD,
                    data = training_set, family = binomial)

summary(LogReg_model)



# Iteration 4 - SibSp removed
LogReg_model <- glm(factor(Survived) ~ Pclass + Parch + Fare + Title + FsizeD,
                    data = training_set, family = binomial)

summary(LogReg_model)



# Iteration 5 - Parch removed
LogReg_model <- glm(factor(Survived) ~ Pclass + Fare + Sex + FsizeD,
                    data = training_set, family = binomial)

summary(LogReg_model)

# Iteration 6 - Fare removed
LogReg_model <- glm(factor(Survived) ~ Pclass + Title + FsizeD,
                    data = training_set, family = binomial)

summary(LogReg_model)

#Final Set of Variable: **Pclass**,**Title**,**FsizeD**
  
#Find set of variables using backward variable selection:
  
attach(training_set)
# Fast backward Selection 
fullmodel <- fastbw(lrm(factor(Survived) ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FsizeD)                     ,rule="p")
fullmodel


# Predicting the Test set results
prob_pred = predict(LogReg_model, type = 'response', newdata = test_set[c("Pclass","Title","FsizeD")])
Survived_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set$Survived, Survived_pred)
cm

# Accuracy

Accuracy = (96+51)/length(test_set$Survived)
Accuracy





#by including the sex column in the model
LogReg_model <- glm(factor(Survived) ~ Pclass + Sex + FsizeD,
                    data = training_set, family = binomial)

summary(LogReg_model)
prob_pred = predict(LogReg_model, type = 'response', newdata = test_set[c("Pclass","Sex","FsizeD")])
Survived_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set$Survived, Survived_pred)
cm

# Accuracy

Accuracy = (96+45)/length(test_set$Survived)
Accuracy

#ROC curve
library(ROCR)
pr=prediction(prob_pred,test_set$Survived)
pr
