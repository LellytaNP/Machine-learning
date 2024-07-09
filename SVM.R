sosmed <- read.csv('E:\\datasetbelajar\\Social_Network_Ads.csv')
sosmed$Age[is.na(sosmed$Age)]=mean(sosmed$Age,na.rm=TRUE)
sosmed$EstimatedSalary[is.na(sosmed$EstimatedSalary)]=mean(sosmed$EstimatedSalary,na.rm=TRUE)
sosmed$Purchased=as.factor(sosmed$Purchased)
sosmedclear=sosmed[,3:5]
#NORMALISASI
library(BBmisc)
sosmednorm <- BBmisc::normalize(sosmedclear,method = 'range')
summary(sosmednorm)
#SPLIT
library(caTools)
set.seed(123)
splitsosmed=sample.split(sosmednorm$Purchased,SplitRatio = 0.7)
trainsosmed=subset(sosmednorm,splitsosmed==TRUE)
testsosmed=subset(sosmednorm,splitsosmed==FALSE)
#BIKIN MODEL
library(e1071)
sosmedsvm=svm(Purchased~.,data=trainsosmed)
sosmedsvm
plot(sosmedsvm,testsosmed)
#TEST MODEL
prediksi=predict(sosmedsvm,newdata=testsosmed)
prediksi
result=cbind(testsosmed,prediksi)
result