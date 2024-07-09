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
library(randomForest)
hutan = randomForest(x=trainsosmed[,-3],
                     y=trainsosmed$Purchased,
                     ntree = 80)
hutan
prediksi =predict(hutan,newdata=testsosmed[,-3])
prediksi
result=cbind(testsosmed,prediksi)
result

#UJIMODEL
#CARA1
library(caret)
result$prediksi=as.factor(result$prediksi)
conf.mat<-caret::confusionMatrix(result$prediksi,
                                  result$Purchased,
                                  positive = '1')
conf.mat
broom::tidy(conf.mat)
result$prediksi=as.numeric(as.character(result$prediksi))
library(pROC)
#CALCULATE ROC CURVE
roc_curve <- roc(result$Purchased,result$prediksi)
plot(roc_curve, main='Roc Curve', col= 'blue')

#CARA2
cm= table(result[,3:4])
cm
result$prediksi=as.numeric(as.character(result$prediksi))
colAUC(result$prediksi,result$Purchased,plotROC = TRUE)