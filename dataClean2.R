library(dplyr)

#상관분석
cor1<-select(data1,WEIGHT2,HEIGHT3,HTIN4,HTM4,WTKG3)
round(cor(cor1),digits = 3)

#HTIN4,HTM4의산점도
cor1 %>% ggplot(aes(HTIN4, HTM4)) + geom_jitter() + geom_smooth(method = "lm")

#차원축소 HTIN4,HTM4 이 거의 흡사하므로 HTIN4를 제거한다.
data1<-select(data1,-HTIN4)

#상관분석
cor1<-select(data1,WEIGHT2,HEIGHT3,HTM4,WTKG3)
round(cor(cor1),digits = 3)

#HEIGHT3,HTM4의산점도
cor1 %>% ggplot(aes(HEIGHT3,HTM4)) + geom_jitter() + geom_smooth(method = "lm")

#상관분석
a<-cor(data1)
a<-ifelse(abs(a)<=.3,NA,a)
a<-ifelse(abs(a)==1,NA,a)
class(a)
summary(a)
colSums(is.na(a))

a<-data.frame(a)
removeIdx = integer() #제거할 컬럼 인덱스
removeNullLength = 39 #NULL 특정 개수
for(i in seq_along(a)){
  if( length(which(is.na(a[ ,i]))) == removeNullLength ) removeIdx = c(removeIdx, i)
}
# i번째 컬럼의 값들 중 null인 row의 수가 removeNullLength보다 크면 해당 컬럼의 인덱스를 removeIdx 벡터에 넣기
if( length(removeIdx) ) a = a[,-removeIdx]

removeIdx = integer() #제거할 로우 인덱스
removeNullLength = 10 #NULL 특정 개수
for(i in 1:nrow(a)){
  if( length(which(is.na(a[i,]))) == removeNullLength ) removeIdx = c(removeIdx, i)
}
# i번째 컬럼의 값들 중 null인 row의 수가 removeNullLength와 같으면 해당 로우의 인덱스를 removeIdx 벡터에 넣기
if( length(removeIdx) ) a = a[-removeIdx,]

rm(i,removeIdx,removeNullLength)

a #상관분석 결과 -0.3 이하, 0.3 이상만 출력









