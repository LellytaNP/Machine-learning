#KNN
sosmed <- read.csv('E:\\datasetbelajar\\Social_Network_Ads.csv')
sosmed$Age[is.na(sosmed$Age)]=mean(sosmed$Age,na.rm=TRUE)
sosmed$EstimatedSalary[is.na(sosmed$EstimatedSalary)]=mean(sosmed$EstimatedSalary,na.rm=TRUE)
sosmed$Purchased=as.factor(sosmed$Purchased)
sosmedclear=sosmed[,3:5]
library(BBmisc)
sosmednorm <- BBmisc::normalize(sosmedclear,method = 'range')
plot(sosmednorm)
library(caTools)
set.seed(123)
splitsosmed=sample.split(sosmednorm$Purchased,SplitRatio = 0.7)
trainsosmed=subset(sosmednorm,splitsosmed==TRUE)
testsosmed=subset(sosmednorm,splitsosmed==FALSE)
library(class) #crete model + test model
y_pred=knn(train=trainsosmed[,-3],test = testsosmed[,-3],cl=trainsosmed$Purchased,k=5)
result=cbind(testsosmed,y_pred)
result

cm=table(result[,3:4])
cm
result$y_pred=as.character(result$y_pred)
result$y_pred=as.numeric(result$y_pred)
colAUC(result$y_pred,result$Purchased,plotROC = TRUE)