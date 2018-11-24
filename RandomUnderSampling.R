#Random Under-Sampling CHCOCNCR가 상당히 불균형하므로 under-sampling수행


set.seed(7777)
library(dplyr)

data2<-data1 #변수 그대로

data2<-select(data1, GENHLTH, HLTHPLN1, CHECKUP1, CHCOCNCR, HAVARTH3, 
              SMOKE100, USENOW3, RENTHOM1, PNEUVAC3, HTM4, WTKG3, DRNKANY5) #로지스틱회귀모형에서 선택된 변수

data2<-select(data1, CHCOCNCR, PNEUVAC3, GENHLTH, MARITAL, 
              HAVARTH3, CHCSCNCR, PERSDOC2, CHECKUP1) #C4.5모형에서 선택된 변수

data2<-select(data1, CHCOCNCR, WTKG3, INCOME2, HTM4, GENHLTH, 
              PNEUVAC3, MARITAL, EDUCA, CHECKUP1, PERSDOC2) #랜덤포레스트에서 선택된 변수

data2<-select(data1, CHCOCNCR, GENHLTH, CHCKIDNY, USENOW3, 
              HAVARTH3) #인공신경망에서 선택된 변수

data2<-select(data1, CHCOCNCR, WTKG3, HTM4, PNEUVAC3, GENHLTH, 
              CHECKUP1) #로지스틱회귀모형과 랜덤포레스트에서 선택된 변수

y1<-filter(data2,CHCOCNCR==0)
y1<-sample_n(filter(data2,CHCOCNCR==0), round(nrow(y1)*0.1))
y2<-filter(data2,CHCOCNCR==1)
y2<-sample_n(filter(data2,CHCOCNCR==1), round(nrow(y2)*0.01))

data3<-rbind(y1,y2)
rm(y1,y2, data2)

#trainData, testData
ind<-sample(2,nrow(data3),replace = TRUE,prob=c(0.7,0.3))
trainData<-data3[ind==1,]
testData<-data3[ind==2,]
rm(ind, data3)


#로지스틱model
model<-glm(CHCOCNCR~.,trainData,family="binomial")
summary(model)
p<-predict(model,newdata = testData,type = "response")
round(p)
table(round(p),testData$CHCOCNCR)
confusionMatrix(as.factor(round(p)), testData$CHCOCNCR)




library(caret)




cor1<-select(data1,HTM4,WTKG3)
round(cor(cor1),digits = 3)


