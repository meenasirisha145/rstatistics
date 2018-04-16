####-------------Predicting adviews of a video-----------------####
#-----Load the train and test Data
ad_org_train=read.csv("vidooly_train.csv",stringsAsFactors = FALSE)
ad_org_test=read.csv("vidooly_test.csv",stringsAsFactors = FALSE)

#-------Looking at structure and summary of the Data
str(ad_org_train)
str(ad_org_test)
summary(ad_org_train)
#-------------Checking for NA values
colSums(is.na(ad_org_train))
fix(ad_org_train)

#----------Replacing the ambigious data with NA's
for (i in 1:nrow(ad_org_train)){
  for(j in 2:6){
    if(ad_org_train[i,j]=='F'){
      ad_org_train[i,j]=NA;
    }
  }
}

#-----------Converting the columns into integer type
for (k in 3:6){
  ad_org_train[,k]=as.integer(ad_org_train[,k])
}

#-----------Treatment for NA  values
colSums(is.na(ad_org_train))
ad_org_train$views[is.na(ad_org_train$views)]=median(ad_org_train$views,na.rm=T)
ad_org_train$likes[is.na(ad_org_train$likes)]=median(ad_org_train$likes,na.rm=T)
ad_org_train$dislikes[is.na(ad_org_train$dislikes)]=median(ad_org_train$dislikes,na.rm=T)
ad_org_train$comment[is.na(ad_org_train$comment)]=median(ad_org_train$comment,na.rm=T)
colSums(is.na(ad_org_train))

#--------Extracting the time components Hour,Minutes and Seconds from the variable duration

for (i in 1:nrow(ad_org_train))
{
  ad_org_train$hour[i]=0;
  ad_org_train$min[i]=0;
  ad_org_train$sec[i]=0;
  
  if((grepl('H',ad_org_train$duration[i])==TRUE) & (grepl('M',ad_org_train$duration[i])==TRUE) & (grepl('S',ad_org_train$duration[i])==TRUE))
  {
    ad_org_train$hour[i]=substr(ad_org_train$duration[i],regexpr('T',ad_org_train$duration[i])+1,regexpr('H',ad_org_train$duration[i])-1);
    ad_org_train$min[i]=substr(ad_org_train$duration[i],regexpr('H',ad_org_train$duration[i])+1,regexpr('M',ad_org_train$duration[i])-1);
    ad_org_train$sec[i]=substr(ad_org_train$duration[i],regexpr('M',ad_org_train$duration[i])+1,regexpr('S',ad_org_train$duration[i])-1);
  }
  
  else if((grepl('H',ad_org_train$duration[i])==FALSE) & (grepl('M',ad_org_train$duration[i])==TRUE) & (grepl('S',ad_org_train$duration[i])==TRUE))
  {
    ad_org_train$min[i]=substr(ad_org_train$duration[i],regexpr('T',ad_org_train$duration[i])+1,regexpr('M',ad_org_train$duration[i])-1);
    ad_org_train$sec[i]=substr(ad_org_train$duration[i],regexpr('M',ad_org_train$duration[i])+1,regexpr('S',ad_org_train$duration[i])-1);
  }
  else if((grepl('H',ad_org_train$duration[i])==FALSE) & (grepl('M',ad_org_train$duration[i])==FALSE) & (grepl('S',ad_org_train$duration[i])==TRUE))
  {
    ad_org_train$sec[i]=substr(ad_org_train$duration[i],regexpr('T',ad_org_train$duration[i])+1,regexpr('S',ad_org_train$duration[i])-1);
  }
  else if((grepl('H',ad_org_train$duration[i])==TRUE) & (grepl('M',ad_org_train$duration[i])==TRUE) & (grepl('S',ad_org_train$duration[i])==FALSE))
  {
    ad_org_train$hour[i]=substr(ad_org_train$duration[i],regexpr('T',ad_org_train$duration[i])+1,regexpr('H',ad_org_train$duration[i])-1);
    ad_org_train$min[i]=substr(ad_org_train$duration[i],regexpr('H',ad_org_train$duration[i])+1,regexpr('M',ad_org_train$duration[i])-1);
    
  }
  else if((grepl('H',ad_org_train$duration[i])==TRUE) & (grepl('M',ad_org_train$duration[i])==FALSE) & (grepl('S',ad_org_train$duration[i])==FALSE))
  {
    ad_org_train$hour[i]=substr(ad_org_train$duration[i],regexpr('T',ad_org_train$duration[i])+1,regexpr('H',ad_org_train$duration[i])-1);
    
  }
  else if((grepl('H',ad_org_train$duration[i])==FALSE) & (grepl('M',ad_org_train$duration[i])==TRUE) & (grepl('S',ad_org_train$duration[i])==FALSE))
  {
    ad_org_train$min[i]=substr(ad_org_train$duration[i],regexpr('T',ad_org_train$duration[i])+1,regexpr('M',ad_org_train$duration[i])-1);
   
  }
  else if((grepl('H',ad_org_train$duration[i])==TRUE) & (grepl('M',ad_org_train$duration[i])==FALSE) & (grepl('S',ad_org_train$duration[i])==TRUE))
  {
    ad_org_train$hour[i]=substr(ad_org_train$duration[i],regexpr('T',ad_org_train$duration[i])+1,regexpr('H',ad_org_train$duration[i])-1);
    ad_org_train$sec[i]=substr(ad_org_train$duration[i],regexpr('H',ad_org_train$duration[i])+1,regexpr('S',ad_org_train$duration[i])-1);
  }
}
head(ad_org_train)

for (j in 10:12){
  ad_org_train[,j]=as.numeric(ad_org_train[,j])
}


str(ad_org_train)
ad_org_train$durationnew=(ad_org_train$hour*3600)+(ad_org_train$min*60)+(ad_org_train$sec)
head(ad_org_train)

#-----------Finding the correlation between Independent Variables
cor(ad_org_train[,c(2,3,4,5,6,13)])

train=ad_org_train
str(train)

#---------Detecting the Outliers
boxplot.stats(train$views)$out
boxplot.stats(train$likes)$out
boxplot.stats(train$dislikes)$out
boxplot.stats(train$comment)$out

#--------------Replacing the Outliers

for (j in c(3,4,5,6,13)){
    r1=quantile(train[,j],0.25)-(1.5*IQR(train[,j]))
    r1
    r2=quantile(train[,j],0.75)+(1.5*IQR(train[,j]))
    r2
  for (i in 1:nrow(train)){
  if ((train[i,j]<r1) | (train[i,j]>r2)){
  train[i,j]=min(train[,j])
  } 
  }
}

boxplot(train$views)
boxplot(train$likes)
boxplot(train$dislikes)
boxplot(train$comment)
boxplot(train$durationnew)

#----------------Splitting the train data into validation and training datasets
library(car)
library(caTools)
spl=sample.split(train$views,0.7)
trainad=subset(train,spl==TRUE)
validad=subset(train,spl==FALSE)

str(trainad)
trainad$category=as.factor(trainad$category)


#-----------Build model using Random Forest
library(randomForest)
randfit=randomForest(adview~views+likes+dislikes+comment+durationnew+category,data=trainad)
randfit
summary(randfit)

randfit1=randomForest(adview~views+likes+dislikes+comment+durationnew,data=trainad)
randfit1
summary(randfit1)

pred=predict(randfit1,newdata=validad)

#------------Look at the test dataset
str(ad_org_test)
summary(ad_org_test)
#-------------Checking for NA values
colSums(is.na(ad_org_test))
test=ad_org_test

#----------Replacing the ambigious data with NA's
for (i in 1:nrow(test)){
  for(j in 2:5){
    if(test[i,j]=='F'){
       test[i,j]=NA;
    }
  }
}

#-----------Converting the columns into integer type
for (k in 2:5){
  test[,k]=as.integer(test[,k])
}

#-----------Treatment for NA  values
colSums(is.na(test))
test$views[is.na(test$views)]=median(test$views,na.rm=T)
test$likes[is.na(test$likes)]=median(test$likes,na.rm=T)
test$dislikes[is.na(test$dislikes)]=median(test$dislikes,na.rm=T)
test$comment[is.na(test$comment)]=median(test$comment,na.rm=T)
colSums(is.na(test))

#--------Extracting the time components Hour,Minutes and Seconds from the variable duration

for (i in 1:nrow(test))
{
  test$hour[i]=0;
  test$min[i]=0;
  test$sec[i]=0;
  
  if((grepl('H',test$duration[i])==TRUE) & (grepl('M',test$duration[i])==TRUE) & (grepl('S',test$duration[i])==TRUE))
  {
    test$hour[i]=substr(test$duration[i],regexpr('T',test$duration[i])+1,regexpr('H',test$duration[i])-1);
    test$min[i]=substr(test$duration[i],regexpr('H',test$duration[i])+1,regexpr('M',test$duration[i])-1);
    test$sec[i]=substr(test$duration[i],regexpr('M',test$duration[i])+1,regexpr('S',test$duration[i])-1);
  }
  
  else if((grepl('H',test$duration[i])==FALSE) & (grepl('M',test$duration[i])==TRUE) & (grepl('S',test$duration[i])==TRUE))
  {
    test$min[i]=substr(test$duration[i],regexpr('T',test$duration[i])+1,regexpr('M',test$duration[i])-1);
    test$sec[i]=substr(test$duration[i],regexpr('M',test$duration[i])+1,regexpr('S',test$duration[i])-1);
  }
  else if((grepl('H',test$duration[i])==FALSE) & (grepl('M',test$duration[i])==FALSE) & (grepl('S',test$duration[i])==TRUE))
  {
    test$sec[i]=substr(test$duration[i],regexpr('T',test$duration[i])+1,regexpr('S',test$duration[i])-1);
  }
  else if((grepl('H',test$duration[i])==TRUE) & (grepl('M',test$duration[i])==TRUE) & (grepl('S',test$duration[i])==FALSE))
  {
    test$hour[i]=substr(test$duration[i],regexpr('T',test$duration[i])+1,regexpr('H',test$duration[i])-1);
    test$min[i]=substr(test$duration[i],regexpr('H',test$duration[i])+1,regexpr('M',test$duration[i])-1);
    
  }
  else if((grepl('H',test$duration[i])==TRUE) & (grepl('M',test$duration[i])==FALSE) & (grepl('S',test$duration[i])==FALSE))
  {
    test$hour[i]=substr(test$duration[i],regexpr('T',test$duration[i])+1,regexpr('H',test$duration[i])-1);
    
  }
  else if((grepl('H',test$duration[i])==FALSE) & (grepl('M',test$duration[i])==TRUE) & (grepl('S',test$duration[i])==FALSE))
  {
    test$min[i]=substr(test$duration[i],regexpr('T',test$duration[i])+1,regexpr('M',test$duration[i])-1);
    
  }
  else if((grepl('H',test$duration[i])==TRUE) & (grepl('M',test$duration[i])==FALSE) & (grepl('S',test$duration[i])==TRUE))
  {
    test$hour[i]=substr(test$duration[i],regexpr('T',test$duration[i])+1,regexpr('H',test$duration[i])-1);
    test$sec[i]=substr(test$duration[i],regexpr('H',test$duration[i])+1,regexpr('S',test$duration[i])-1);
  }
}
head(test)


for (j in 9:11){
  test[,j]=as.integer(test[,j])
}


str(test)
test$durationnew=(test$hour*3600)+(test$min*60)+(test$sec)
head(test)

#-----------Finding the correlation between Independent Variables
cor(test[,c(2,3,4,5,12)])

#--------------Replacing the Outliers

for (j in c(2,3,4,5,12)){
  r1=quantile(test[,j],0.25)-(1.5*IQR(test[,j]))
  r1
  r2=quantile(test[,j],0.75)+(1.5*IQR(test[,j]))
  r2
  for (i in 1:nrow(test)){
    if ((test[i,j]<r1) | (test[i,j]>r2)){
      test[i,j]=min(test[,j])
    } 
  }
}
head(test)
prediction=predict(randfit1,newdata=test)
prediction
test$adviews=round(prediction)
head(test)
final=test[,c("vidid","adviews")]
head(final)
class(final)
write.csv(final,"finalsub.csv",row.names = FALSE)
