library(dplyr)

#차원축소
PHYSHLTH
MENTHLTH

cor(data1$GENHLTH,data1$MENTHLTH)
ggplot(data1) + geom_bar(mapping = aes(x=GENHLTH,y=MENTHLTH)) #GENHLTH 시각화

data1 %>% ggplot(aes(GENHLTH,MENTHLTH)) + geom_jitter() + geom_smooth(method = "lm")











#반응변수의 0과 1값의 크기차이가 크므로 2가지 샘플로 나눈후 trainData, testData 만든다.(동일비율)
table(data1$CHCOCNCR)
sam1<-filter(data1, CHCOCNCR == 0)
sam2<-filter(data1, CHCOCNCR == 1)

set.seed(1234)
ind1<-sample(2,nrow(sam1),replace = TRUE, prob=c(0.7,0.3))
ind2<-sample(2,nrow(sam2),replace = TRUE, prob=c(0.7,0.3))

trainData1<-sam1[ind1==1,]
testData1<-sam1[ind1==2,]
trainData2<-sam2[ind2==1,]
testData2<-sam2[ind2==2,]

trainData<-rbind(trainData1, trainData2)
testData<-rbind(testData1, testData2)
rm(sam1, sam2, testData1, testData2, trainData1, trainData2, ind1, ind2)







library(car)


data_lm<-lm(CHCOCNCR~.,data1)
rm(data_lm)
