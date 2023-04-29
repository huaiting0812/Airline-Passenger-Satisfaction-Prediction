#資料類別轉換
gc()
memory.size(F) 
options(scipen = 100)
memory.size()
memory.limit()
train_new <- read.csv("D:\\研究所\\多變量分析\\報告\\archive (3)\\train_new.csv")
train_new <- train_new[,-c(1,2,5)]
for (i in 1:22){
  train_new[,i] <- factor(train_new[,i])
}
train_new[,5] <- as.numeric(train_new[,5])
train_new[,20] <- as.numeric(train_new[,20])
train_new[,21] <- as.numeric(train_new[,21])
train_new <- data.frame(train_new)
str(train_new)
test <- read.csv("D:\\研究所\\多變量分析\\報告\\archive (3)\\test_new.csv")
test <- test[,-c(1,2,5)]
for (i in 1:22){
  test[,i] <- factor(test[,i])
}
test[,5] <- as.numeric(test[,5])
test[,20] <- as.numeric(test[,20])
test[,21] <- as.numeric(test[,21])
str(test)
#隨機森林
##bagging
library(adabag)
train_new_control <- rpart.control(minisplit=10,minbucket=3,cp=0.01)
train_new.bagging<-bagging(satisfaction~., data=train_new,nbagg=100,control=train_new_control)
train.bagging.pred <- predict.bagging(train_new.bagging,test)
train.bagging.pred1 <- predict.bagging(train_new.bagging,train_new)
train.bagging.pred$confusion
train.bagging.pred1$error
barplot(train_new.bagging$importance,names.arg = c("ADM","BH","CS",
                                                   "Class","Cleanliness","CT",
                                                   "DAT","DDM","EOB",
                                                   "FD"," FAD","GL",
                                                   "Gender","IE","IS","IWS",
                                                   "LRS","OBS","OB",
                                                   "SC","TOT"),cex.names = 0.5)

#cv
train_baggingcv <- bagging.cv(satisfaction~.,data = train_new,v=10,mfinal=100,control=train_new_control)
train_baggingcv$confusion
train_baggingcv$error
##隨機森林套件
install.packages("randomForest")
library(randomForest)
randomforest <- randomForest(satisfaction~.,importance=TRUE,data=train_new,na.action=na.roughfix)
print(randomforest)
plot(randomforest)#由圖可知在ntree=100時，模型內誤差趨於平穩
abline(v=100)
rm(randomforest)
gc()
randomforest_new <- randomForest(satisfaction~., data=train_new, ntree=100,proximity=TRUE,na.action=na.roughfix)
#boosting without bootstrap
train_new_control <- rpart.control(minisplit=10,minbucket=3,cp=0.01)
train_adaboost_F <- boosting(satisfaction~., data=train_new, boos=F, mfinal=100,control=train_new_control)
train_adaboost_pred_F <-predict.boosting(train_adaboost_F,test) 
train_adaboost_pred_F1 <-predict.boosting(train_adaboost_F,train_new)
train_adaboost_pred_F$confusion
train_adaboost_pred_F$error
train_adaboost_pred_F1$confusion
train_adaboost_pred_F1$error

train_adaboost_pred_CV <- boosting.cv(satisfaction~.,data = train_new,v=10,mfinal=100,control=train_new_control)

barplot(train_adaboost_F$importance,names.arg = c("ADM","BH","CS",
                                                "Class","Cleanliness","CT",
                                                "DAT","DDM","EOB",
                                                "FD"," FAD","GL",
                                                "Gender","IE","IS","IWS",
                                                "LRS","OBS","OB",
                                                "SC","TOT"),cex.names = 0.5)
train_new_control <- rpart.control(minisplit=10,minbucket=3,cp=0.01)
train_adaboost_T <- boosting(satisfaction~., data=train_new, boos=T, mfinal=100,control=train_new_control)
train_adaboost_pred <-predict.boosting(train_adaboost_T,test) 
train_adaboost_pred$confusion
train_adaboost_pred$error
barplot(train_adaboost_T$importance,names.arg = c("ADM","BH","CS",
                                                  "Class","Cleanliness","CT",
                                                  "DAT","DDM","EOB",
                                                  "FD"," FAD","GL",
                                                  "Gender","IE","IS","IWS",
                                                  "LRS","OBS","OB",
                                                  "SC","TOT"),cex.names = 0.5)
train_new_control <- rpart.control(minisplit=10,minbucket=3,cp=0.01)

train_adaboost_pred_CV_T <- boosting.cv(satisfaction~.,data = train_new,v=10,mfinal=100,control=train_new_control,boos = T)
train_adaboost_pred_CV_T$error







