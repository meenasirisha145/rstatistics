x=c(1,3,5,7,9,2,4,6,8,10)
x1=scale(x,center = F,scale=F)
x1
as.vector(x1)
xc=scale(x,center = T,scale=F)
xc
as.vector(xc)

xc1=scale(x,center = 4,scale=F)
as.vector(xc1)
cbind(x,-4,xc1)
xs=scale(x,center = F,scale=T)
as.vector(round(xs,2))

rmse=sqrt(sum(x^2)/(length(x)-1))
rmse
round((xs2=(x/rmse)),2)

xsc=scale(x,center = T,scale=T)
round(as.vector(xsc),2)

round((zscore=(x-mean(x))/sd(x)),2)


x=matrix(1:10,ncol=2)      
x
(centered.x=scale(x,center = T,scale=F))
(centered.x=scale(x,center = c(2,7),scale=F))
(centered.x=scale(x,center = c(2,7),scale=T))
(centered.x=scale(x,center = T,scale=T))
cov(centered.x)
