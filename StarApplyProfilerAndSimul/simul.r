#d --> return p(x)
#r --> return x in X randomly
#q --> return x that P(X<=x)>=p, ex: qnorm(p=10,mean=10,sd=1)
#p --> return P(X<=q), ex: pnorm(q=10,mean=10,sd=1)
dnorm(12,mean=10,sd=1,log=FALSE)
pnorm(10,mean=10,sd=1)
pnorm(10,mean=10,sd=1,lower.tail=FALSE)
rnorm(10,mean=10,sd=2)
set.seed(1)
qnorm(0.25,mean=10,sd=1)
qnorm(0.75,mean=10,sd=1)
pnorm(10.67449,mean=10,sd=1)
set.seed(20)
x <- rnorm(100)
e <- rnorm(100,0,2)
y <- 0.5+2*x+e
summary(y)
plot(x,y)
set.seed(20)
#x <- rnorm(100)
x <- rbinom(100,1,0.5)
e <- rnorm(100,0,2)
y <- 0.5+2*x+e
summary(y)
plot(x,y)
set.seed(1)
x <- rnorm(100)
log.mu <- 0.5 + 0.3*x
y <- rpois(100,exp(log.mu))
summary(y)
plot(x,y)
sample(1:10,4)
sample(letters,5)
sample(1:10,replace=TRUE)
set.seed(1)
X <- data.frame("varl"=sample(1:5),"var2"=sample(6:10),"var3"=sample(11:15))
X <- X[sample(1:5),]
X[(X$var1<=3 & X$var3>11),]
X$var2>8
X$var2[(c(1,3))]=NA
X[which(X$var2>=6),]
sort(X$varl)
sort(X$varl,decreasing=TRUE)
sort(X$var2)
sort(X$var2,na.last=TRUE)
order(X$var2)
a=c(4,4,3,3,3,2,2,1)
b=c(12,11,6,4,5,3,1,10)
ab=cbind(a,b)
order(a,b)
ab[order(a,b),]
library(plyr)
arrange(X,var1)
arrange(ab,var1)
ab=data.frame(ab)
arrange(ab,a)
arrange(ab,a,b)
desc(a)
desc(b)
arrange(ab,desc(a))
ab$c <- seq(8)
ab <- rbind(ab,seq(8))
