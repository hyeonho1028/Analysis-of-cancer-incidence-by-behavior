library(dplyr)
library(data.table)

data<-tbl_df(data) 
data1<-data
write.table(data,"D:\\behavioral\\project\\data.txt",sep=",",row.names = FALSE,na="NA")
rm(data)
data<-read.table(choose.files(), sep = ",", header = TRUE)

head(data1)
names(data1)
summary(data1)
str(data1)

#CHCOCNCR 암발생 여부(O, X)
table(data1$CHCOCNCR)
sum(is.na(data1$CHCOCNCR))
boxplot(data1$CHCOCNCR)
data1$CHCOCNCR<-ifelse(data1$CHCOCNCR>=7,NA,data1$CHCOCNCR) #CHCOCNCR 7이상을 NA값으로 치환
data1 <- data1 %>% filter(!is.na(CHCOCNCR)) #CHCOCNCR결측값 제거
sum(is.na(data1$CHCOCNCR))
data1$CHCOCNCR<-data1$CHCOCNCR-1
data1<-data.table(data1)
boxplot(data1$CHCOCNCR)

#불필요한 변수제거
data1<-select(data1,-(FMONTH:SEQNO))
names(data1)
table(data1$HTM4)

summary(data1)



#null값이 많은 컬럼 제거(전체 데이터의 90% 이하 null개수)
colSums(is.na(data1))
data1<-as.data.frame(data1)


removeIdx = integer() #제거할 컬럼 인덱스
removeNullLength = 2858345*0.05 #NULL 특정 개수
for(i in seq_along(data1)){
  if( length(which(is.na(data1[ ,i]))) > removeNullLength ) removeIdx = c(removeIdx, i)
  }
# i번째 컬럼의 값들 중 null인 row의 수가 removeNullLength보다 크면 해당 컬럼의 인덱스를 removeIdx 벡터에 넣기
if( length(removeIdx) ) data1 = data1[,-removeIdx] #removeIdx에 들어 있는 컬럼 인덱스에 해당하는 컬럼 제거

rm(i,removeIdx,removeNullLength) #변수삭제



#결측값을 제외했을 때 남는 데이터 개수
b<-na.omit(data1)  #약 30만개의 손실이 일어난다. 30만개의 손실을 적용

data1<-na.omit(data1)
rm(b)
