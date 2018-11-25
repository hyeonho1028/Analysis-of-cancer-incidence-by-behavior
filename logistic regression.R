library(caret)

#로지스틱회귀모형

#교차분석
set.seed(1234)
ind<-sample(2,nrow(data1),replace = TRUE,prob=c(0.7,0.3))
trainData<-data1[ind==1,]
testData<-data1[ind==2,]


#모델
model<-glm(CHCOCNCR~.,trainData,family="binomial")
summary(model)
p<-predict(model,newdata = testData,type = "response")
round(p)
table(round(p),testData$CHCOCNCR)

confusionMatrix(p, testData)

#교차검증
confusionMatrix(p1, testData_sam)
p1<-round(p)
factor(p1)
factor(testData_sam)

str(p1)
p1<-as.factor(p1)
testData_sam<-as.factor(testData_sam)
str(testData_sam)
p1
testData_sam<-testData$CHCOCNCR




