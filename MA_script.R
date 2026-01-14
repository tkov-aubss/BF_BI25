#The packages we will use frequently need to be called before using functions
library(forecast)
library(stats)
library(datasets)
library(readxl)

# Importing data from the directory (notice that your directories will be different!)
#Problem 9-10 on page 93 of HW 
data<-read_excel("C:/Users//Week 45/prob9-10p93HW.xlsx")
View(data)

#Renaming the series and defining the numerical vector as a time-series object using ts() function
yt <- ts(data)

#Forecasting 10 periods ahead with MA of order 3
k<-5 #length of your MA window
#starting point is your actual data, replace yt with your variable of interest name
l<-length(yt) #number of available data points
h <-10 #number of periods ahead you want to forecast

########################################CODE STARTS
y <- yt 
#generating space to save the forecasts
for (i in 1:h){
  y <- c(y, 0)
}
#calculating the fitted values (MA for the periods already available, will need it to calculate the RMSE)
yfit <- rep(NA,l)
for (j in (k+1):(l) ){
  a<-j-k
  b<-j-1
  x <-y[a:b]
  yfit[j] <- mean(x)
}
#calculating RMSE
e<-yt-yfit
RMSE<-sqrt(sum(e^2,na.rm = TRUE)/length(yfit))
#calculating the forecast values 
##(extends the data variable with the forecasted values, so the y series is ready for plotting)
for (j in (l+1):(l+h) ){
  a<-j-k
  b<-j-1
  x <-y[a:b]
  y[j] <- mean(x)
}
#########################################CODE ENDS
View(y)


