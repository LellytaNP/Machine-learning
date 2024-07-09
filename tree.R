CustomerPurchase <- read.csv('E:\\datasetbelajar\\CustomerPurchase.csv')
summary(CustomerPurchase)

#ganti purchase jadi factor
#CustomerPurchase$Purchased = as.factor(CustomerPurchase$Purchased)
#CustomerPurchase$Review = as.factor(CustomerPurchase$Review)
#CustomerPurchase$Education = as.factor(CustomerPurchase$Education)

summary(CustomerPurchase$Purchased)
summary(CustomerPurchase$Review)
summary(CustomerPurchase$Education)



#memilih fitur
ambilfitur=CustomerPurchase[,c(4:6)]
summary(ambilfitur)
ambilfitur$Education=factor(ambilfitur$Education,levels = c('PG','School','UG'),labels = c(1,2,3))
ambilfitur$Review=factor(ambilfitur$Review,levels = c('Average','Good','Poor'),labels = c(2,3,1))
ambilfitur$Purchased=factor(ambilfitur$Purchased,levels = c('No','Yes'),labels = c(1,2))

#split data
library(caTools)
set.seed(24)
split=sample.split(ambilfitur$Purchased,SplitRatio = 0.7)
databind=cbind(ambilfitur,split)
databind
databind$split = as.factor(databind$split)
summary(databind)
trainset = subset(ambilfitur,split==TRUE)
tesset = subset(ambilfitur,split==FALSE)

#decision tree
library(rpart)
library(rpart.plot)
pohon=rpart(Purchased~.,data = trainset)
pohon

prp(pohon)
prediction = predict(pohon,newdata = tesset,type = 'class')
result=cbind(tesset,prediction)
result
#result$prediction=as.factor(result$prediction)

library(caret)
conf.mat=caret::confusionMatrix(result$prediction,
                                result$Purchased,
                                positive = '2')
conf.mat
result$prediction=as.numeric(as.character(result$prediction))
library(pROC)
roc_curve=roc(result$Purchased,result$prediction)
plot(roc_curve,main='ROC Curve',col='blue')

levels(result$prediction)