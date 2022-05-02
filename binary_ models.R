library(tidyverse)
library(psych)

mydata<-read.csv(file.choose(),head=T)#importing data from the excel
head(mydata)
str(mydata)# viewing the structure of your data
#converting to appropriate data
mydata$admit<-as_factor(mydata$admit)
mydata$rank<-as_factor(mydata$rank)
str(mydata)
set.seed(1234)
#data partitioning into training and testing
ind<-sample(2,nrow(mydata),replace = T,prob = c(0.8,0.2))
ind
train<-mydata[ind==1, ]#train data sample
test<-mydata[ind==2, ]# test data sample
#logistic model
model<-glm(admit~gre+gpa+rank,data=train,family = "binomial")
summary(model)
#predicting the logistic model using train data
p1<-predict(model,train,type="response")
head(p1)
head(train)
y<--1.586730+(0.004743*380)+(-0.219328*3.61)+(1*-1.125341)#fitting the coefficients into the model
y
exp(y)/(1+exp(y))#computing the probability
#confusion matrix of train data
pk<-ifelse(p1>0.5,1,0)
tab1<-table(predicted=pk,actual=train$admit)
tab1
1-sum(diag(tab1))/sum(tab1)
24/75
#predicting logistic model using test data
p2<-predict(model,test,type = "response")
head(p2)
head(test,3)
#confusion matrix
ph<-ifelse(p2>0.5,1,0)
ta<-table(predicted=ph,actual=test$admit)
ta
1-sum(diag(ta))/sum(ta)

#probit model

myprobit<-glm(admit~gre+gpa+rank,data = train,family = binomial(link="probit"))
summary(myprobit)
#predicting probit model using train data
ppro<-predict(myprobit,train,type = "response")
head(ppro)
head(train)
#confusion matrix
cpro<-ifelse(ppro>0.3,1,0)
tabpro<-table(predicted=cpro,actual=train$admit)
tabpro
1-sum(diag(tabpro))/sum(tabpro)
#predicting probit model using test data
ppro2<-predict(myprobit,test,type = "response")
head(ppro2)
head(test)
#confusion matrix
cpro2<-ifelse(ppro2>0.3,1,0)
tabpro2<-table(predicted=cpro2,actual=test$admit)
tabpro2
1-sum(diag(tabpro2))/sum(tabpro2)


#logit model

mylogit<-glm(admit~gre+gpa+rank,train,family=binomial(link="logit"))
summary(mylogit)
#predicting logit model using train data
plog<-predict(mylogit,train,type="response")
head(plog)
head(train)
clog<-ifelse(plog>0.3,1,0)
ctab<-table(predicted=clog,actual=train$admit)
ctab
1-sum(diag(ctab))/(sum(ctab))

#predicting logit model using test data
plog2<-predict(mylogit,test,type="response")
head(plog2)
head(test)
#confusion matrix
clog2<-ifelse(plog2>0.3,1,0)
ctab2<-table(predicted=clog2,actual=test$admit)
ctab2
1-sum(diag(ctab2))/(sum(ctab2))
