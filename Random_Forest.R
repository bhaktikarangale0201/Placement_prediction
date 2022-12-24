library(randomForest)
library(dplyr)
library(caTools)
library(caret)
df<-read.csv("D:/Sem-3/SEM_3_PROJECTS/DS/Placement_Data_Full_Class1.csv")
df<-select(df,gender,ssc_p,ssc_b,hsc_p,hsc_b,hsc_s,degree_p,degree_t,workex,etest_p,specialisation,mba_p,status)
df$ssc_b<-as.factor(df$ssc_b)
df$hsc_b<-as.factor(df$hsc_b)
df$hsc_s<-as.factor(df$hsc_s)
df$workex<-as.factor(df$workex)
df$gender<-as.factor(df$gender)
df$degree_t<-as.factor(df$degree_t)
df$specialisation<-as.factor(df$specialisation)
df$status<-as.factor(df$status)

set.seed(123)
sample<-sample.split(df$status,SplitRatio=.80)
train<-subset(df,sample==TRUE)
test<-subset(df,sample==FALSE)

rfm = randomForest(status~.,data=train,importance=TRUE,proximity=TRUE)
status_pred = predict(rfm,test)
test$status_pred = status_pred 
#View(test)

#confusionMatrix(status_pred,testing$status)
cfm=table(test$status,test$status_pred)
cat("The Confusion matrix : \n")
print(cfm)
#cfm1=confusionMatrix(data=status_pred,reference=test$status)
classifier_RF = randomForest(x = train[-5],
                             y = train$status,
                             ntree = 300)
plot(classifier_RF)

classification_ac = sum(diag(cfm)/sum(cfm))
classification_ac

P <- 10/(10+3)
R <- 10/(10+0)
cat("1) Accuracy : ",classification_ac,"\n")
cat("2) Precision :",P,"\n")
cat("3) Recall :",R,"\n")
cat("4) F1 score :",(2*P*R)/(P+R))

plot(rfm)


