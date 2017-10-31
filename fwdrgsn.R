#forward regression model
library(MASS)
data("mtcars")
mtcars
cor(mtcars)
mt.lm=lm(mpg~1,data=mtcars)
add1(mt.lm,scope=mtcars)
mt.lm=lm(mpg~wt,data=mtcars)
add1(mt.lm,scope = mtcars)
mt.lm=lm(mpg~wt+cyl,data=mtcars)
summary(mt.lm)
