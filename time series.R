help('ts')

#weekly data of covid-19 positive cases from 22-01-2020 to 15 april 2020
x<-c(580,7813,28266,59287,75700,
     87820,95314,126214,218843,471497,
     936851,1508725,2072113)
#library required for decimal_date() function
library(lubridate)
#output to be created as png file
png(file='timeseries.png')
#creating time series object
#from date 22 januari 2020
mts<- ts(x,start = decimal_date(ymd('2020-04-15')),
         frequency = 365.25/7)
mts
plot(mts)
plot(mts, xlab="weekly data",
     ylab='total positive cases',
     main='covid 19 pandemic',
     col.main='red')

install.packages('forecast')
library(forecast)
fit <- auto.arima(mts)
prediksilonjakan <- forecast(fit,5)
plot(prediksilonjakan)

plot(prediksilonjakan,xlab="weekly data",
     ylab='total positive covid cases',
     main='covid 19 pandemic',
     col.main='darkgreen')
