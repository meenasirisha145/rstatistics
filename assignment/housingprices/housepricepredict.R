
data=read.csv("home_data.csv",stringsAsFactors=F)
data

str(data)
colnames(data)
head(data)
max(data$price)
colSums(is.na(data))
library(dplyr)
avg_price=aggregate(data$price,list(data$zipcode),FUN=mean)
filter(avg_price,x==max(x))

nrow(filter(data,sqft_living>2000 & sqft_living<=4000))/nrow(data)

#########

library(ggplot2)
qplot(data$sqft_living,data$price,xlab ="Square feet", ylab="price($)",colour=data$price)
library(caTools)
set.seed(123)

split = sample.split(data$price, SplitRatio = 0.80)

training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)
length(training_set[,1])
length(test_set[,1])

#creating a model with some of the variables
my_features =c('bedrooms','bathrooms','sqft_living','sqft_lot','floors','zipcode')
summary(data[,my_features])
#LINEAR REGRESSION
model1=lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+zipcode,data=training_set)
summary(model1)
residuals(model1)

rmse=sqrt(sum(residuals(model1)^2) / length(training_set$id))
rmse

#predicting the values for model
pred = predict(model1,test_set[,my_features])
rmsepred=sqrt(sum((pred-test_set$price)^2) / length(test_set$id))
rmsepred

#checking the prediction values
house1 = data[which(data$id=='9297300055'),]
house1
house1$price
predict(model1,house1)


house2 = data[which(data$id=='2524049179'),]
house2
house2$price
predict(model1,house2)
plot(model1)

#builing the model with more advanced features
advanced_features=c('bedrooms','bathrooms','sqft_living','sqft_lot','floors','zipcode','condition','grade',
                    'waterfront','view','sqft_above','sqft_basement','yr_built','yr_renovated','lat','long',
                    'sqft_living15','sqft_lot15')
model2=lm(formula=price ~ bedrooms + bathrooms + sqft_living + 
                    sqft_lot + floors + zipcode + condition + grade + 
                    waterfront + view + sqft_above + sqft_basement +
                    yr_built + yr_renovated + lat + long + sqft_living15 + 
                    sqft_lot15, data=training_set)
summary(model2)

rmse=sqrt(sum(residuals(model2)^2) / length(training_set$id))
rmse

#predicting the values for model
pred = predict(model2,test_set[,advanced_features])
rmsepred=sqrt(sum((pred-test_set$price)^2) / length(test_set$id))
rmsepred

#checking the prediction values
house1 = data[which(data$id=='9297300055'),]
house1
house1$price
predict(model2,house1)



house2 = data[which(data$id=='2524049179'),]
house2
house2$price
predict(model2,house2)
plot(model2)

#predictions with both models
house = data[which(data$id=='5309101200'),]
house
house$price
predict(model1,house)
predict(model2,house)





model=lm(formula=price ~ bedrooms + bathrooms + sqft_living + 
            sqft_lot +  zipcode + condition + grade + 
            waterfront + view + sqft_above + 
            yr_built + yr_renovated + lat + long + sqft_living15 + 
            sqft_lot15, data=training_set)



summary(model)


#backward variable selection
step(model2,direction="backward")
library(rms)
model=fastbw(ols(formula=price ~ bedrooms + bathrooms + sqft_living + 
                   sqft_lot + floors + zipcode + condition + grade + 
                   waterfront + view + sqft_above + sqft_basement +
                   yr_built + yr_renovated + lat + long + sqft_living15 + 
                   sqft_lot15,data=training_set),rule="p")
model

model3=fastbw(model2,rule = "p")
