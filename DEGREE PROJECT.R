library(Rcmdr)
myproject <- readXL("C:/Users/hosea/OneDrive/Desktop/PROJECT DATA.xlsx", 
                    rownames=FALSE, header=TRUE, na="", sheet="Social_Network_Ads", 
                    stringsAsFactors=TRUE)
head(myproject)
summary(myproject)
myproject <- within(myproject, {
  Purchased <- as.factor(Purchased)
})
str(myproject)
with(myproject, Barplot(Purchased, xlab="Purchased", ylab="Frequency", 
                        label.bars=TRUE))
with(myproject, Barplot(Gender, xlab="SEX", ylab="Frequency", 
                        label.bars=TRUE))
with(myproject, Barplot(AGECAT, xlab="AGE", ylab="Frequency", 
                        label.bars=TRUE))
with(myproject, Barplot(SALARYCAT, xlab="SALARYCAT", ylab="Frequency", 
                        label.bars=TRUE))
#objective number one fomulating a logistic model
model <- glm(Purchased ~ Gender + AGECAT + SALARYCAT, 
             family=binomial(logit), data=myproject)
model
summary(model)
exp(coef(model))  # Exponentiated coefficients ("odds ratios")
attributes(model)
#normality of the residuals
shapiro.test(model$residuals)
library(tidyverse)

#testing for heterosedasticity using graph
par(mfrow=c(2,2))
plot(model)

library(lmtest)
###"""variance inflation factors to test multicollinearity
#as a rule of thump a Vif value that exceed 5 or 10 indicate apply(
#  probamatic amount of collinearity"""
library(car)
vif(model)

#"""Breusch_godfrey test of autocorrelation
#null hypothesis is first order autocorrelation is not present
# v/s
#alternative hypothesis is first order autocorrelation is present"""#
library(lmtest) 
bgtest(model,order=3)

#logistic model building
###logistic regression is a method for fitting a regression curve 
#when y is categorical variable the typical use of this model is
#predicting y given a set of predictorsx. the predictors can be
#continuous,categorical or a mix of both.

#we srart by spliting the data into two chunks: training and testing data
# the training data will be used to fit our model which we will be testing over the testing set

set.seed(1234)
data<-sample(2,nrow(myproject),replace=T,prob=c(0.8,0.2))
train<-myproject[data==1,]
test<-myproject[data==2,]

#now let build the model with train data
new_model<-glm(Purchased ~ Gender + AGECAT + SALARYCAT,
               family = binomial(link = "logit"),data=train)
summary(new_model)
#"""first we can see that gender is not  statistically significant 
#for the significant variables age has smallest p value which surgest a strong
#association of age of the customer with the probability of purchasing a product
#a unit increase of age increases the odds by 0.2052"""

library(stats)
#generating anova table

anova(new_model,test)

#obtaining the Mcfadden R squared index for assessing the model fit

library(pscl) 
pR2(new_model)
#chisquare test of association
#sex is not associated with purchase of goods
#v/s sex is associated with purchase of goods
cross<-table(myproject$Purchased,myproject$Gender)
cross
addmargins(cross)
round(100*prop.table(cross,2),digits = 0)
barplot(prop.table(cross,2)*100,
        xlab = "sex",ylab = "percentages",
        main = "percentage purchase by sex",beside = T,
        col = c("grey","black"),
        legend=row.names(cross))
chisq.test(cross)
#fisher.test(cross)
#we have observed the pearson chi square statistic is 0.49946 coresponding to pvalue of0.4797
#which is greater than o,o5 hence a greate evidence of failing to reject the null hypothesis
#and conclude that sex is not associated with purchase of goods

#chisquire tes of association
cross1<-table(myproject$Purchased,myproject$AGECAT)
cross1
addmargins(cross1)
round(100*prop.table(cross1,2),digits = 0)
barplot(prop.table(cross1,2)*100,
        xlab = "sex",ylab = "percentages",
        main = "percentage purchase by age",beside = T,
        col = c("grey","black"),
        legend=row.names(cross))
chisq.test(cross1)
#we have observed the pearson chi square statistic is 93.799 corresponding to p value of 0.00000000000000022
#which is less than 0.05 hence a grate evidence of reject the null hypothesis
#and conclude that age is associated with purchase of goods
cross2<-table(myproject$Purchased,myproject$SALARYCAT)
cross2
addmargins(cross2)
round(100*prop.table(cross2,2),digits = 0)
barplot(prop.table(cross2,2)*100,
        xlab = "sex",ylab = "percentages",
        main = "percentage purchase by salary",beside = T,
        col = c("grey","black"),
        legend=row.names(cross2))
chisq.test(cross2)
#fisher.test(cross2)
#we have observed the pearson chi square statistic is 125.18 corresponding to p value of 0.00000000000000022
#which is less than 0.05 hence a grate evidence of reject the null hypothesis
#and conclude that salary is associated with purchase of goods





#prediction
#"""discision boundary will be 0.5 if probality is >0.5then y is 1
#otherwse it is zero"""

p1<-predict(new_model,train,type = "response")
pred<-ifelse(p1>0.5,1,0)
a<-table(pred,train$Purchased)
confusionmatrix<-as.matrix(a)

accuracy<-(192+83)/(192+33+14+83)
accuracy
misclassification<-1-accuracy
misclassification
sensitivity=(83)/(33+83)
sensitivity
specificity=(192)/(192+14)
specificity
#ploting ROC :it is the curve generated by plotting the true positve rates
#against false positive rates
# AUC:area under ROC a good predictive model should have AUC close to 1
library(psych)
library(caret)
library(pROC)
r<-multiclass.roc(train$Purchased,p1,percent=TRUE)
roc<-r[["rocs"]]
r1<-roc[[1]]
plot.roc(r1,col = "red",lwd = 5,main="ROC with train data")
plot.roc(r1,print.auc = T,auc.polygon = T,grid = c(0.1,0.2),
         grid.col = c("green","red"),max.auc.polygon = T,
         auc.polygon.col = "lightblue",print.thres = T)
coords(r1,"best",ret="threshold",transpose=F)
auc(r1)
