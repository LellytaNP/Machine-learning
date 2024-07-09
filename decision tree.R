sosmed <- read.csv('E:\\datasetbelajar\\Social_Network_Ads.csv')
sosmed$Age[is.na(sosmed$Age)]=mean(sosmed$Age,na.rm=TRUE)
sosmed$EstimatedSalary[is.na(sosmed$EstimatedSalary)]=mean(sosmed$EstimatedSalary,na.rm=TRUE)
sosmed$Purchased=as.factor(sosmed$Purchased)
sosmedclear=sosmed[,3:5]
summary(sosmedclear)
#SPLIT
library(caTools)
set.seed(123)
splitsosmed=sample.split(sosmedclear$Purchased,SplitRatio = 0.7)
trainsosmed=subset(sosmedclear,splitsosmed==TRUE)
testsosmed=subset(sosmedclear,splitsosmed==FALSE)
#TRAINTEST
library(rpart)
library(rpart.plot)
pohon <- rpart(Purchased~.,data = trainsosmed)
pohon
prp(pohon)
prediction<-predict(pohon,newdata=testsosmed,type='class')
result= cbind(testsosmed,prediction)
result