library(LearnBayes)
curve(dbinom(2,8,x))
#we now plot the prior
x<-seq(0,1,by=0.001)
x
k<-rnorm(100,0,10)
plot(x,dbeta(x,1,1),lty=3)
plot(k,col="red",bg="green",new=TRUE)
#plot the posterior distribution
prior1=c(1,1)
data1=c(2,6)
triplot(prior1,data1,where="topright")


#====================================================
x<-seq(0,1,by=0.001)
x
#k<-rnorm(100,0,10)
plot(x,dbeta(x,2,3))
#plot(k,col="red",bg="green",new=TRUE)
#plot the posterior distribution
prior2=c(2,3)
data2=c(2,6)
triplot(prior2,data2,where="topright")
#are under curve is 1
#the pick of posterior lies somewere bwn the picks of prior and the likelihood
#coz combines information from the prior and the likelihood
#====================================================

par(mfrow=c(2,2))
x<-seq(0,1,by=0.001)
x
plot(x,dbeta(x,1,1))
plot(x,dbeta(x,2,3))
triplot(prior1,data1,where="topright")
triplot(prior2,data2,where="topright")
#====================================================
par(mfrow=c(2,2))
x<-seq(0,1,by=0.001)
x
prior2=c(20,20)
data2=c(17,3)
plot(x,dbeta(x,20,20))
triplot(prior2,data2,where="bottomleft")
#=====================================================
theta<-seq(0,2,0.001)
y<-dgamma(theta,2,5)
plot(theta,y,lim=range(0:3))
y2<-dgamma(theta,10,15)
lines(theta,y2)
legend(0,2.5,"prior")
legend(0.6,2.8,"posterior")
#=======================================================
N=1000
theta<-rbeta(N,9.2,13,8)
x<-rbinom(N,20,theta)
y<-0
accept<-ifelse(x>14.5,y+1,y+0)
plot(density(accept))
prob<-sum(accept/N)
prob


