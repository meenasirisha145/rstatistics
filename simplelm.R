#simple linear modelling
#trying to predict the relation between area of store and sales
x = c(1.7,1.6,2.8,5.6,1.3,2.2,1.3,1.1,3.2,1.5,5.2,4.6,5.8,3 )
y = c(3.7,3.9,6.7,9.5,3.4,5.6,3.7,2.7,5.5,2.9,10.7,7.6,11.8,4.1 )
df=data.frame(x,y)
df
length(df)
summary(df)
length(x)
length(y)
sum(x)
sum(y)
mean(x)
mean(y)
cov(x,y)
cor(x,y)#sales are strongly correlated with area of the store
cor.test(x,y)

#alternative hypothesis: true correlation is not equal to 0 means 
#null hypothesis :true correlation is equal to 0 that is there is some relation between x and y
#degrees of freedom:n-1-k,k is no.of independent variables

plot(x,y, main="plot between x and y")
abline(lm(y~x),col="red",lty=4,pch=20)
text(x,y,labels=paste(x,y,sep=","),cex=0.7,col="red")
text(x,y,labels=paste0('(',x,',',y,')'))
text(4,10,labels="meena",col="blue")
fit=lm(y~x)
fit
summary(fit)

df$X=round(x-mean(x),2)
df
df$Y=round(y-mean(y),2)
df
df$XY=round(df$X*df$Y,2)
df
df$x2=round(df$X*df$X,2)
df
df$y2=round(df$Y*df$Y,2)
df
ssXY=sum(df$XY)
ssXY
ssx2=sum(df$x2)
ssx2
b1=ssXY/ssx2
b1
b0=mean(y)-(b1*mean(x))
b0
df$yh=round(fitted(fit),2)#predicted values of y:yh=b0+b1Xi
df
plot(df$yh,x)
df$sse=round((df$y-df$yh)^2,2)#sum of squares of error sse
df
summary(df$sse)
df$ssr=round((df$yh-mean(y))^2,2)
df
colSums(df)
R2=round(sum(df$ssr)/(sum(df$sse)+sum(df$ssr)),2)
R2
summary(fit)
n=length(x)
n
stderror=sqrt(sum(df$sse)/(n-2))
stderror


p=1#no.of independent variables
(MSR=round(sum(df$ssr),2/p))
(MSE=round(sum(df$sse),2/(n-p-1)))
fstat=MSR/MSE
fstat
