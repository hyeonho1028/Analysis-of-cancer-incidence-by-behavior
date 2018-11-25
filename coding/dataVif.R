library(dplyr)

#�������
PHYSHLTH
MENTHLTH

cor(data1$GENHLTH,data1$MENTHLTH)
ggplot(data1) + geom_bar(mapping = aes(x=GENHLTH,y=MENTHLTH)) #GENHLTH �ð�ȭ

data1 %>% ggplot(aes(GENHLTH,MENTHLTH)) + geom_jitter() + geom_smooth(method = "lm")











#���������� 0�� 1���� ũ�����̰� ũ�Ƿ� 2���� ���÷� ������ trainData, testData �����.(���Ϻ���)
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