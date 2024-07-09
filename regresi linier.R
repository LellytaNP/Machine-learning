cars <- read.csv('E:\\datasetbelajar\\cars.csv')
summary(cars)

head(cars)
dataset <- cars[,2:13]
dataset$Brand = as.factor(dataset$Brand)
dataset$Fuel_Type = factor(dataset$Fuel_Type, levels=c('Petrol','Diesel'),labels=c('1','2'))
dataset$Transmission = factor(dataset$Transmission, levels = c('Manual','Automatic'), labels = c('1','2'))
dataset$Owner_Type = factor(dataset$Owner_Type, levels = c('First','Second','Third'), labels = c('1','2','3'))
View(dataset)
datacars <- dataset[,-c(1:2)]
head(datacars)
summary(datacars)
datasetcar <- datacars[,c(1,2,5,10)]
datasetcar

#linier regresion
library(caTools)
set.seed(24)
split=sample.split(datasetcar$Price,SplitRatio = 0.7)
split
View(datasetcar)
trainset = subset(datasetcar,split==TRUE)
testset = subset(datasetcar,split==FALSE)
lmprice=lm(Price~., data=trainset)

library(ggplot2)
sqrt(mean(lmprice$residuals^2))

