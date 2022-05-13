library(prophet)
library(lubridate)
library(ggplot2)
library(forecast)
library(quantmod)
library(PerformanceAnalytics)
library(xts)
#extrating ethereum data.
#ethereum it is a cryptocurrency
ethereum_data<-read.csv(file.choose(),header=T)
str(ethereum_data)
head(ethereum_data)
#changing the date format
ethereum_data$Date<-dmy(ethereum_data$Date)
plot(ethereum_data$Date,ethereum_data$Close)
#plotting a quick plot.
qplot(Date,Close,data = ethereum_data,
      main="ethereum closing price 2015-2019")

#log trasformation
ds<-ethereum_data$Date
y<-log(ethereum_data$Close)
log_data<-data.frame(ds,y)
qplot(dt,y,data=log_data,
      main="ethereum closing price with log trasformation")
plot(log_data,type="b")
#differencing log of closeing price
d<-diff(y)
plot(d)
#auto arima model
model<-auto.arima(y)
model
acf(model$residuals)
pacf(model$residuals)
Box.test(model$residuals,lag = 19,type = "Ljung-Box")
hist(model$residuals,
     col = "blue",
     breaks = 60,
     main="normality of the error",
     freq=F)
lines(density(model$residuals))
f<-forecast(model,365)
autoplot(f)
# forecasting using prophet package
m<-prophet(log_data)
m
future_price<-make_future_dataframe(m,periods = 365)
tail(future_price)
forecast<-predict(m,future_price)
#plot forecast
plot(m,forecast)
#interactive forecast plot
dyplot.prophet(m,forecast)
#plotting the components
prophet_plot_components(m,forecast)

