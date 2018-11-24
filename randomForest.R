#randomforest

library(randomForest)
library(RWeka)
library(C50)
library(nnet)
library(rpart)
library(party)
library(caret)
library(FSelector)
library(NeuralNetTools)
library(CHAID) #install.packages("CHAID", repos = "http://R-Forge.R-project.org", type = "source")
library(ROCR)



y1<-filter(data1,CHCOCNCR==0)
y1<-sample_n(filter(data1,CHCOCNCR==0), round(nrow(y1)*0.5))
y2<-filter(data1,CHCOCNCR==1)
y2<-sample_n(filter(data1,CHCOCNCR==1), round(nrow(y2)*0.05))


data3<-rbind(y1,y2)
rm(y1,y2)

#trainData, testData
ind<-sample(2,nrow(data3),replace = TRUE,prob=c(0.7,0.3))
trainData<-data3[ind==1,]
testData<-data3[ind==2,]
rm(ind)

#C4.5
model<-J48(CHCOCNCR~.,trainData)
p<-predict(model,newdata = testData)
table(p,testData$CHCOCNCR)
summary(model)
model
information.gain(CHCOCNCR~., trainData)
model$finalModel

C45Fit <- train(CHCOCNCR ~., method="J48", data=trainData)
C45Fit <- train(CHCOCNCR ~., method="J48", data=trainData,
                tuneLength = 5,
                trControl = trainControl(
                  method="cv", indexOut=train))


#랜덤포레스트
model<-randomForest(CHCOCNCR~.,trainData, important = TRUE)
model
summary(model)
importance(model)

p<-predict(model,newdata = testData)
table(p,testData$CHCOCNCR)
confusionMatrix(as.factor(p), testData$CHCOCNCR)



#인공신경망
nn_model <- nnet(CHCOCNCR ~. , data=trainData, size=3)
summary(nn_model)
garson(nn_model)+  theme(axis.text.x=element_text(angle=90, hjust=1))
garson(nn_model)+  coord_cartesian(ylim=c(0.0125, 0.1000))
garson(nn_model)+ 
garson(nn_model)+  theme(legend.position = "top")

p<-predict(nn_model,newdata = testData,type = "class")
table(p, testData$CHCOCNCR)



#CHAID 모형
model<- chaid(CHCOCNCR ~. , data=select(trainData,-c(HTM4, WTKG3)))
model<- chaid(CHCOCNCR ~. , data=trainData)
p<-predict(model,newdata = testData)
confusionMatrix(p, testData$CHCOCNCR)

pr2<-prediction(as.numeric(p), as.numeric(testData$CHCOCNCR))




#CART
model <-rpart(CHCOCNCR ~. , data=trainData)
plotcp(model)
model <- prune(model, cp = 0.09) #가지치기
p<-predict(model,newdata = testData, type="class")
confusionMatrix(as.factor(p), testData$CHCOCNCR)



#party 모형
model<-ctree(CHCOCNCR~.,trainData)
p<-predict(model, newdata = testData, type="response")
confusionMatrix(as.factor(p), testData$CHCOCNCR)

plot()

pr<-prediction(as.numeric(p), as.numeric(testData$CHCOCNCR))


#rocr 곡선
plot(performance(pr, "tpr", "fpr"))
plot(performance(pr2, "tpr", "fpr"))

install.packages("pROC")
library(pROC)
roc.test(pr,pr2,plot=T)


#최종 선정 모형 
#party 모형
model<-ctree(CHCOCNCR~.,trainData)
p<-predict(model, newdata = testData, type="response")
confusionMatrix(as.factor(p), testData$CHCOCNCR)

plot(model)


