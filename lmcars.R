data(cars)
str(cars)
View(cars)
cars.lm=lm(speed~dist,data=cars)
cars.lm
summary(cars.lm)
names(cars.lm)
coef(cars.lm)
confint(cars.lm)
confint(cars.lm,level = 0.9,parm = c("speed",'dist'))
fitted(cars.lm)
resid(cars.lm)
plot(dist~speed,data=cars)
abline(cars.lm,col="blue",pch=20,lty=2)
carsfit=lm(dist~poly(speed,2),data=cars)
points(dist~fitted(carsfit),col="red",pch=20,data=cars)


