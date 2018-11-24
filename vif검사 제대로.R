set.seed(1234)
library(car)

#모델
model<-lm(CHCOCNCR~.,data1)


vif(model)
sqrt(vif(model))>2

summary(model)


for(i in 1:30){
  data1[,i]<-as.numeric(data1[,i])
}
str(data1)
table(data1$CHCOCNCR)


#카이제곱 검정
library(dplyr)
chisq.test(select(data1,CHCOCNCR,GENHLTH))

chisq.test(with(data1, table(GENHLTH, CHCOCNCR))) #분할표 만들어서 카이제곱검정
chisq.test(data1$CHCOCNCR,data1$GENHLTH) #카이제곱검정
chisq.test(data1$CHCOCNCR,data1[,5])$p.value
for(i in 1:30){
  print(chisq.test(data1$HLTHPLN1,data1[,i])$p.value)
}



table(data1$CHCOCNCR)

