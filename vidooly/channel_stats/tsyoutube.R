#------Forecasting Views,Subscribers and Video Count-------------------#
#-----------Loading required packages

library(forecast)
library(tseries)
library(ggplot2)
library(MASS)
library(caTools)



#-----------------------------Setting the working directory-------------------------------------------#

data<-read.csv("oct_march1.csv",header = TRUE)
TSdata=data#To create a backup of original data

#---------------------------------Exploring the data-------------------------------------------------------#

head(TSdata)
str(TSdata)
summary(TSdata)
colSums(is.na(TSdata))

#-------Converting into Date fromat
TSdata$date=as.Date(TSdata$date,format="%m/%d/%Y")

str(TSdata)

#---------Extracting the date and Views columns
pviews=TSdata[,c("views","date")]
head(pviews)
#---------------------Transformation of the date data into time series------------------------------------#
library(xts)
library(zoo)
ts=xts(pviews[,1],order.by = as.Date(pviews[,2],"%Y-%m-%d"))
ts
start(ts)
end(ts)
frequency(ts)

#---------------------------plotting the views--------------------
plot(ts,ylab="Views", xlab="days",main="Views between OCT-16 AND MAR-17",col="green")


#---Checking the stationarity of transformed data using the Augmented Dickey-Fuller Test(ADF)
#If the p-value <0.05, hence, we reject the Ho: Series is Non-Stationary

adf.test(ts,alternative="s")

par(mfrow=c(1,1))

plot(diff(ts,differences = 1),ylab="Diff(Views)",xlab="daily",main="Diff(Views) between 2016oct-2017mar",col="grey")

#-------Differentiating the data
tsdata=diff(ts,differences=1)
class(tsdata)
tsdata
colnames(tsdata)
colSums(is.na(tsdata))
tsdata[,1][which(is.na(tsdata[,1]))]=median(tsdata[,1],na.rm = T)

#-------Finding the order of p and q
par(mfrow=c(1,2))
acf(tsdata,main="ACF plot")#ACF PLOT -- Moving Average or q
pacf(tsdata,main="PACF plot")#PACF PLOT -- Auto Regressive or p

class(ts)

#Running the ARIMA model
ARIMAFit=arima(ts,c(0,1,1))
summary(ARIMAFit)

#Predicting the future views
pred=predict(ARIMAFit,n.ahead=91)
pred
viewspred=pred$pred+2*pred$se
viewspred

solution=read.csv("solchannel.csv",header=TRUE)
head(solution)
solution$views=round(viewspred)
head(solution)
nrow(solution)

#--------------------Prediction of subscribers--------------------------


TSdata

subs=TSdata[,c("subscriber","date")]
head(subs)
#---------------------Transformation of the date data into time series------------------------------------#
library(xts)
library(zoo)
subts=xts(subs[,1],order.by = as.Date(subs[,2],"%Y-%m-%d"))
subts
start(subts)
end(subts)
frequency(subts)

#---------------------------plotting the views--------------------
plot(subts,ylab="Subscribers", xlab="days",main="Subscribers between OCT-16 AND MAR-17",col="green")


#-----Checking the stationarity of transformed data using the Augmented Dickey-Fuller Test(ADF)

adf.test(subts,alternative="s")

#----------Differentiating the data
subtsdata=diff(subts)
colSums(is.na(subtsdata))
head(subtsdata)
subtsdata[,1][which(is.na(subtsdata[,1]))]=median(subtsdata[,1],na.rm = T)

adf.test((subtsdata),alternative = "s")

par(mfrow=c(1,1))

plot(subtsdata,ylab="Diff(subscribers)",xlab="daily",main="Diff(subscribers) between 2016oct-2017mar",col="grey")

#-------Finding the order of p and q
par(mfrow=c(1,2))
acf(subtsdata,main="ACF plot")#ACF PLOT -- Moving Average or q
pacf(subtsdata,main="PACF plot")#PACF PLOT -- Auto Regressive or p

#Running the ARIMA model
ARIMAFitsub=arima(subts,c(0,1,1))
summary(ARIMAFitsub)

#Predicting the future Subscribers
pred1=predict(ARIMAFitsub,n.ahead=91)
pred1
subpred=pred1$pred+2*pred1$se
subpred
solution$Subscribers=round(subpred)
head(solution)



#-------------------------------Predicting the Videocount---------------------


TSdata
head(TSdata)
vc=TSdata[,c("videoscount","date")]
head(vc)
#---------------------Transformation of the date data into time series------------------------------------#
library(xts)
library(zoo)
vcts=xts(vc[,1],order.by = as.Date(vc[,2],"%Y-%m-%d"))
vcts
start(vcts)
end(vcts)
frequency(vcts)

#---------------------------plotting the views--------------------
plot(vcts,ylab="VideoCounts", xlab="days",main="Videocounts between OCT-16 AND MAR-17",col="green")


#----Checking the stationarity of transformed data using the Augmented Dickey-Fuller Test(ADF)

adf.test(vcts,alternative="s")

#--------Differentiating the data

vctsdata=diff(vcts)
colSums(is.na(vctsdata))
head(vctsdata)
vctsdata[,1][which(is.na(vctsdata[,1]))]=median(vctsdata[,1],na.rm = T)

adf.test((vctsdata),alternative = "s")

par(mfrow=c(1,1))

plot(vctsdata,ylab="Diff(Videocounts)",xlab="daily",main="Diff(videocounts) between 2016oct-2017mar",col="grey")

#-------Finding the order of p and q
par(mfrow=c(1,2))
acf(vctsdata,main="ACF plot")#ACF PLOT -- Moving Average or q
pacf(vctsdata,main="PACF plot")#PACF PLOT -- Auto Regressive or p

#Running the ARIMA model
ARIMAFitvc=arima(vcts,c(0,1,1))
summary(ARIMAFitvc)

#Predicting the future videocount
pred2=predict(ARIMAFitvc,n.ahead=91)
pred2
vcpred=pred2$pred+2*pred2$se
vcpred
solution$videocount=round(vcpred)
head(solution)
solution=solution[,c("chid","views","Subscribers","videocount","Date")]

#-----Storing the result into a csv file
write.csv(solution,"channelsol.csv",row.names = FALSE)

