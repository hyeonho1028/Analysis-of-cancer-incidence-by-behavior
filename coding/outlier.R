if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
# 1. 이상치 결측치 확인 ----
# ...L 1) 결측값 제거 ----
colSums(is.na(data))
sum(colSums(is.na(data)))

removeIdx = integer() 

removeNullLength = nrow(data)*0.05
for(i in seq_along(data)){
  if( length(which(is.na(data[ ,i]))) > removeNullLength ) 
    removeIdx = c(removeIdx, i)
}

data = data[,-removeIdx]
# 결측값이 nrow의 95% 이상인 컬럼index를 removeIdx에 벡터형으로 할당
# 한 컬럼에 NA값이 95% 이상인 컬럼 제거
rm(i,removeIdx,removeNullLength) 

data <- na.omit(data)
# NA값을 가지고 있는 row 제거

# ...L 2) 필요없는 컬럼 제거 ----
View(data)
# 상관 없는 컬럼을 제거하기 위해 View를 보고 판단한다.
data <- data[,-c(1:7)]
# 상관 없는 컬럼 제거

# ...L 3) column remove and save.image ----
save.image('D:/project_r/project/rdata/data_eda.rdata') 
# 위 경로로 .rdata 저장 // R은 경고없이 덮어씌여지므로 주의!
# ...L 4) load ----
load('D:/project_r/project/rdata/data_eda.rdata')


# 2. Exponential data analysis ----
# ...L 1) 반응변수(CHCOCNCR)에 대한 이상값 제거 ----
sum(is.na(data$CHCOCNCR))
table(data$CHCOCNCR)
summary(data$CHCOCNCR)
# CHCOCNCR 1은 암에 걸린적 있음 // 2는 암에 걸린적 없음 //  7, 9의 경우 이상값
data$CHCOCNCR <- as.numeric(data$CHCOCNCR)
# factor 형 변수이다.
data$CHCOCNCR <- ifelse(data$CHCOCNCR >= 7, NA, data$CHCOCNCR)
# 이상값들을 NA로 변경
data <- na.omit(data)
# NA값(이상값) 제거


# ...L 2) 설명변수에 대한 이상값 제거 ----
data1 <- filter(data, CHCOCNCR==1)
data2 <- filter(data, CHCOCNCR==2)
par(mfrow = c(1, 2))
# 암 발생여부에 대한 그래프를 그리기 위해 데이터를 나눈다.

# ......L (1) GENHLTH ----
table(data$GENHLTH)
barplot(table(data1$GENHLTH)/length(data1$GENHLTH), ylim = c(0, 0.35))
barplot(table(data2$GENHLTH)/length(data2$GENHLTH), ylim = c(0, 0.35))
data$GENHLTH <- ifelse(data$GENHLTH >= 7, NA, data$GENHLTH)
data <- na.omit(data)
# 이상값인 7이상값을 NA로 만들고 제거

# ......L (2) PHYSHLTH ----
table(data$PHYSHLTH)
barplot(table(data1$PHYSHLTH)/length(data1$PHYSHLTH), ylim = c(0, 0.35))
barplot(table(data2$PHYSHLTH)/length(data2$PHYSHLTH), ylim = c(0, 0.35))
data <- select(data, -PHYSHLTH)
# 88의 경우 None 인데 너무 많은 이상값이 있기 때문에 제거한다.

##### ......L (3) MENTHLTH #####
table(data$MENTHLTH)
barplot(table(data1$MENTHLTH)/length(data1$MENTHLTH))
barplot(table(data2$MENTHLTH)/length(data2$MENTHLTH))
data <- select(data, -MENTHLTH)
# 88의 경우 None 인데 너무 많은 이상값이 있기 때문에 제거한다.

##### ......L (4) HLTHPLN1 #####
table(data$HLTHPLN1)
barplot(table(data1$HLTHPLN1)/length(data1$HLTHPLN1))
barplot(table(data2$HLTHPLN1)/length(data2$HLTHPLN1))


table(data$HLTHPLN1)



ggplot(data, aes(names(table(data$HLTHPLN1)), y=table(data$HLTHPLN1))) + 
  geom_bar(stat="identity", position = "dodge")



table(data$HLTHPLN1)/sum(table(data$HLTHPLN1))


ggplot(data, aes(x = names(table(data$HLTHPLN1)),
                 y = table(data$HLTHPLN1)/sum(table(data$HLTHPLN1)), 
                 fill = CHCOCNCR)) + 
  geom_bar(stat = "identity", position = 'dodge')


unique(data$HLTHPLN1)

ggplot(data, aes(x = c(1, 2, 7, 9), y = HLTHPLN1, fill = factor(CHCOCNCR))) + 
  geom_bar(stat = "identity", position = "dodge")


table(data$HLTHPLN1)

ggplot(data, aes(x = factor(CHCOCNCR), y = HLTHPLN1)) + 
  geom_bar(stat = "identity")


data.frame(table(data$HLTHPLN1))



data <- data.table::data.table(data)


--------------------------------------------------------------------
graph_cnt<-sqldf('select CHCOCNCR, count(*) as count
                  from data1
                  group by CHCOCNCR
                  order by CHCOCNCR')
levels(graph_cnt$CHCOCNCR) <- c("Yes", "No")
graph_cnt$count<-accounting(graph_cnt$count, digits = 0)

ggplot(graph_cnt, aes(x=CHCOCNCR, y=count))+
  geom_bar(stat="identity", fill="lightblue", colour="black")+
  geom_text(aes(label=count), vjust=1.5)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("암 발생여부")+
  scale_y_continuous(labels = scales::comma)




##### ......L (5) PERSDOC2 #####
##### ......L (6) MEDCOST #####
##### ......L (7) CHECKUP1 #####
##### ......L (8) CVDINFR4 #####
##### ......L (9) CVDCRHD4 #####
##### ......L (10) CVDSTRK3 #####
##### ......L (11) ASTHMA3 #####
##### ......L (12) CHCSCNCR #####
##### ......L (13) HAVARTH3 #####
##### ......L (14) ADDEPEV2 #####
##### ......L (15) CHCKIDNY #####
##### ......L (16) DIABETE3 #####
##### ......L (17) SMOKE100 #####
##### ......L (18) USENOW3 #####
##### ......L (19) VETERAN3 #####
##### ......L (20) MARITAL #####
##### ......L (21) CHILDREN #####
##### ......L (22) EDUCA #####
##### ......L (23) INCOME2 #####
##### ......L (24) WEIGHT2 #####
##### ......L (25) HEIGHT3 #####
##### ......L (26) RENTHOM1 #####
##### ......L (27) SEX #####
##### ......L (28) EXERANY2 #####
##### ......L (29) ALCDAY5 #####
##### ......L (30) QSTVER #####
##### ......L (31) QSTLANG #####
##### ......L (32) HTIN4 #####
##### ......L (33) HTM4 #####
##### ......L (34) DRNKANY5 #####

names(data)


data1<-data1[,-40]
colSums(is.na(data1))
#YEAR을 제거한 data1

boxplot(data1$GENHLTH)
table(data1$GENHLTH)

#범주형 변수가 대부분이므로 factor형으로 변환(수치형은 따로 as.numeric했음)
str(data1)
for (i in 1:37) {
  data1[,i]<-as.factor(data1[,i])
}
data1$WEIGHT2<-as.numeric(data1$WEIGHT2)
data1$HEIGHT3<-as.numeric(data1$HEIGHT3)
data1$HTIN4<-as.numeric(data1$HTIN4)
data1$HTM4<-as.numeric(data1$HTM4)
data1$WTKG3<-as.numeric(data1$WTKG3)

theme_update(plot.title = element_text(hjust = 0.5))#ggplot2의 title을 중앙에 배치

#반응변수(CHCOCNCR) - 암 발생여부(피부암 제외)
summary(data1$CHCOCNCR)
graph_cnt<-sqldf('select CHCOCNCR, count(*) as count
                  from data1
                  group by CHCOCNCR
                  order by CHCOCNCR')
levels(graph_cnt$CHCOCNCR) <- c("Yes", "No")
graph_cnt$count<-accounting(graph_cnt$count, digits = 0)

ggplot(graph_cnt, aes(x=CHCOCNCR, y=count))+
  geom_bar(stat="identity", fill="lightblue", colour="black")+
  geom_text(aes(label=count), vjust=1.5)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("암 발생여부")+
  scale_y_continuous(labels = scales::comma)
  
rm(graph_cnt)

  #설명변수
summary(data1$GENHLTH)

graph_cnt<-sqldf('select GENHLTH, count(*) as count
                  from data1
                 group by GENHLTH
                 order by GENHLTH')
levels(graph_cnt$GENHLTH) <- c("Excellent", "Very Good", "Good", "Fair", "Poor", "Don’t know", "Refused")
graph_cnt$count<-accounting(graph_cnt$count, digits = 0)

ggplot(graph_cnt, aes(x=GENHLTH, y=count))+
  geom_bar(stat="identity", fill="lightblue", colour="black")+
  geom_text(aes(label=count), vjust=1.5)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("건강상태")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::comma)

rm(graph_cnt) #GENHLTH 시각화

table(data1$PHYSHLTH)
summary(data1$PHYSHLTH)
sum(ifelse(data1$PHYSHLTH<77,1,0))
boxplot(data1$PHYSHLTH)
ggplot(data1) + geom_bar(mapping = aes(x=PHYSHLTH)) #PHYSHLTH 시각화
data1<-select(data1,-PHYSHLTH) #이상값이 많으므로 제거

table(data1$MENTHLTH)
summary(data1$MENTHLTH)
sum(ifelse(data1$MENTHLTH<77,1,0))
boxplot(data1$MENTHLTH)
ggplot(data1) + geom_bar(mapping = aes(x=MENTHLTH)) #MENTHLTH 시각화
data1<-select(data1,-MENTHLTH) #이상값이 많으므로 제거

table(data1$HLTHPLN1)
summary(data1$HLTHPLN1)
boxplot(data1$HLTHPLN1)
ggplot(data1) + geom_bar(mapping = aes(x=HLTHPLN1)) #HLTHPLN1 시각화

table(data1$PERSDOC2)
summary(data1$PERSDOC2)
boxplot(data1$PERSDOC2)
ggplot(data1) + geom_bar(mapping = aes(x=PERSDOC2)) #PERSDOC2 시각화

table(data1$MEDCOST)
summary(data1$MEDCOST)
boxplot(data1$MEDCOST)
ggplot(data1) + geom_bar(mapping = aes(x=MEDCOST)) #MEDCOST 시각화

table(data1$CHECKUP1)
summary(data1$CHECKUP1)
boxplot(data1$CHECKUP1)
ggplot(data1) + geom_bar(mapping = aes(x=CHECKUP1)) #CHECKUP1 시각화

table(data1$CVDINFR4)
summary(data1$CVDINFR4)
boxplot(data1$CVDINFR4)
ggplot(data1) + geom_bar(mapping = aes(x=CVDINFR4)) #CVDINFR4 시각화

table(data1$CVDCRHD4)
summary(data1$CVDCRHD4)
boxplot(data1$CVDCRHD4)
ggplot(data1) + geom_bar(mapping = aes(x=CVDCRHD4)) #CVDCRHD4 시각화

table(data1$CVDSTRK3)
summary(data1$CVDSTRK3)
boxplot(data1$CVDSTRK3)
ggplot(data1) + geom_bar(mapping = aes(x=CVDSTRK3)) #CVDSTRK3 시각화

table(data1$ASTHMA3)
summary(data1$ASTHMA3)
boxplot(data1$ASTHMA3)
ggplot(data1) + geom_bar(mapping = aes(x=ASTHMA3)) #ASTHMA3 시각화

table(data1$CHCSCNCR)
summary(data1$CHCSCNCR)
boxplot(data1$CHCSCNCR)
ggplot(data1) + geom_bar(mapping = aes(x=CHCSCNCR)) #CHCSCNCR 시각화

table(data1$HAVARTH3)
summary(data1$HAVARTH3)
boxplot(data1$HAVARTH3)
ggplot(data1) + geom_bar(mapping = aes(x=HAVARTH3)) #HAVARTH3 시각화

table(data1$ADDEPEV2)
summary(data1$ADDEPEV2)
boxplot(data1$ADDEPEV2)
ggplot(data1) + geom_bar(mapping = aes(x=ADDEPEV2)) #ADDEPEV2 시각화

table(data1$CHCKIDNY)
summary(data1$CHCKIDNY)
boxplot(data1$CHCKIDNY)
ggplot(data1) + geom_bar(mapping = aes(x=CHCKIDNY)) #CHCKIDNY 시각화

table(data1$DIABETE3)
summary(data1$DIABETE3)
boxplot(data1$DIABETE3)
ggplot(data1) + geom_bar(mapping = aes(x=DIABETE3)) #CHCKIDNY 시각화

table(data1$SMOKE100)
summary(data1$SMOKE100)
boxplot(data1$SMOKE100)
ggplot(data1) + geom_bar(mapping = aes(x=SMOKE100)) #SMOKE100 시각화

table(data1$USENOW3)
summary(data1$USENOW3)
boxplot(data1$USENOW3)
ggplot(data1) + geom_bar(mapping = aes(x=USENOW3)) #USENOW3 시각화

table(data1$VETERAN3)
summary(data1$VETERAN3)
boxplot(data1$VETERAN3)
ggplot(data1) + geom_bar(mapping = aes(x=VETERAN3)) #VETERAN3 시각화

table(data1$MARITAL)
summary(data1$MARITAL)
boxplot(data1$MARITAL)
ggplot(data1) + geom_bar(mapping = aes(x=MARITAL)) #MARITAL 시각화

table(data1$CHILDREN)
summary(data1$CHILDREN)
sum(ifelse(data1$CHILDREN<88,1,0))
boxplot(data1$CHILDREN)
ggplot(data1) + geom_bar(mapping = aes(x=CHILDREN)) #CHILDREN 시각화

table(data1$EDUCA)
summary(data1$EDUCA)
boxplot(data1$EDUCA)
ggplot(data1) + geom_bar(mapping = aes(x=EDUCA)) #EDUCA 시각화

table(data1$INCOME2)
summary(data1$INCOME2)
boxplot(data1$INCOME2)
ggplot(data1) + geom_bar(mapping = aes(x=INCOME2)) #INCOME2 시각화

table(data1$WEIGHT2)
summary(data1$WEIGHT2)
boxplot(data1$WEIGHT2)
ggplot(data1) + geom_bar(mapping = aes(x=WEIGHT2)) #WEIGHT2 시각화

table(data1$HEIGHT3)
summary(data1$HEIGHT3)
boxplot(data1$HEIGHT3)
ggplot(data1) + geom_bar(mapping = aes(x=HEIGHT3)) #HEIGHT3 시각화

table(data1$RENTHOM1)
summary(data1$RENTHOM1)
boxplot(data1$RENTHOM1)
ggplot(data1) + geom_bar(mapping = aes(x=RENTHOM1)) #RENTHOM1 시각화

table(data1$SEX)
summary(data1$SEX)
boxplot(data1$SEX)
ggplot(data1) + geom_bar(mapping = aes(x=SEX)) #SEX 시각화

table(data1$EXERANY2)
summary(data1$EXERANY2)
boxplot(data1$EXERANY2)
ggplot(data1) + geom_bar(mapping = aes(x=EXERANY2)) #EXERANY2 시각화

table(data1$SEATBELT)
summary(data1$SEATBELT)
boxplot(data1$SEATBELT)
ggplot(data1) + geom_bar(mapping = aes(x=SEATBELT)) #SEATBELT 시각화

table(data1$PNEUVAC3)
summary(data1$PNEUVAC3)
boxplot(data1$PNEUVAC3)
ggplot(data1) + geom_bar(mapping = aes(x=PNEUVAC3)) #PNEUVAC3 시각화

table(data1$ALCDAY5)
sum(ifelse(data1$ALCDAY5>200 & data1$ALCDAY5<777,1,0))
sum(ifelse(data1$ALCDAY5<200,1,0))
summary(data1$ALCDAY5)
boxplot(data1$ALCDAY5)
ggplot(data1) + geom_bar(mapping = aes(x=ALCDAY5)) #ALCDAY5 시각화

table(data1$HIVTST6)
summary(data1$HIVTST6)
boxplot(data1$HIVTST6)
ggplot(data1) + geom_bar(mapping = aes(x=HIVTST6)) #HIVTST6 시각화

table(data1$QSTVER)
summary(data1$QSTVER)
boxplot(data1$QSTVER)
ggplot(data1) + geom_bar(mapping = aes(x=QSTVER)) #QSTVER 시각화

table(data1$QSTLANG)
summary(data1$QSTLANG)
boxplot(data1$QSTLANG)
ggplot(data1) + geom_bar(mapping = aes(x=QSTLANG)) #QSTLANG 시각화

table(data1$HTIN4)
summary(data1$HTIN4)
boxplot(data1$HTIN4)
ggplot(data1) + geom_bar(mapping = aes(x=HTIN4)) #HTIN4 시각화

table(data1$HTM4)
summary(data1$HTM4)
boxplot(data1$HTM4)
ggplot(data1) + geom_bar(mapping = aes(x=HTM4)) #HTM4 시각화

table(data1$WTKG3)
summary(data1$WTKG3)
boxplot(data1$WTKG3)
ggplot(data1) + geom_bar(mapping = aes(x=WTKG3),binwidth = 100) #WTKG3 시각화

table(data1$DRNKANY5)
summary(data1$DRNKANY5)
boxplot(data1$DRNKANY5)
ggplot(data1) + geom_bar(mapping = aes(x=DRNKANY5)) #DRNKANY5 시각화

#암발생여부와 GENHLTH변수의 시각화
ggplot(data1) + geom_count(mapping = aes(x=GENHLTH, y=CHCOCNCR))
data1 %>% count(GENHLTH,CHCOCNCR) %>% ggplot(mapping = aes(x=GENHLTH,y=CHCOCNCR)) + geom_tile(mapping = aes(fill=n))

ggplot(data1) + geom_bar(mapping = aes(x=data1[,i]))
ggplot(data1, mapping = aes(data1[,2])) + geom_histogram(binwidth = 10)

ggplot(data1, mapping = aes(data1[,2])) + geom_freqpoly(binwidth = 10)
ggplot(data1, mapping = aes(x=data1[,2],y=..density..)) + geom_freqpoly(mapping =aes(colour=GENHLTH),binwidth = 10)


boxplot(data1[,2])


str(data1)
table(data1$GENHLTH)


library(dplyr)
for(i in 1:37){
  data1[,i]<-as.numeric(data1[,i])
}

str(data1)

data1$GENHLTH<-ifelse(data1$GENHLTH>=7,NA,data1$GENHLTH)
data1<-select(data1,-PHYSHLTH) #이상값이 많으므로 제거
data1$HLTHPLN1<-ifelse(data1$HLTHPLN1>=7,NA,data1$HLTHPLN1)
data1$PERSDOC2<-ifelse(data1$PERSDOC2>=7,NA,data1$PERSDOC2)
data1$MEDCOST<-ifelse(data1$MEDCOST>=7,NA,data1$MEDCOST)
data1$CHECKUP1<-ifelse(data1$CHECKUP1==7,NA,data1$CHECKUP1)
data1$CHECKUP1<-ifelse(data1$CHECKUP1==8,5,data1$CHECKUP1)
data1$CHECKUP1<-ifelse(data1$CHECKUP1==9,NA,data1$CHECKUP1)
data1$CVDINFR4<-ifelse(data1$CVDINFR4>=7,NA,data1$CVDINFR4)
data1$CVDSTRK3<-ifelse(data1$CVDSTRK3>=7,NA,data1$CVDSTRK3)
data1$ASTHMA3<-ifelse(data1$ASTHMA3>=7,NA,data1$ASTHMA3)
data1$CHCSCNCR<-ifelse(data1$CHCSCNCR>=7,NA,data1$CHCSCNCR)
data1$HAVARTH3<-ifelse(data1$HAVARTH3>=7,NA,data1$HAVARTH3)
data1$ADDEPEV2<-ifelse(data1$ADDEPEV2>=7,NA,data1$ADDEPEV2)
data1$CHCKIDNY<-ifelse(data1$CHCKIDNY>=7,NA,data1$CHCKIDNY)
data1$DIABETE3<-ifelse(data1$DIABETE3>=7,NA,data1$DIABETE3)
data1$SMOKE100<-ifelse(data1$SMOKE100>=7,NA,data1$SMOKE100)
data1$USENOW3<-ifelse(data1$USENOW3>=7,NA,data1$USENOW3)
data1$VETERAN3<-ifelse(data1$VETERAN3>=7,NA,data1$VETERAN3)
data1$MARITAL<-ifelse(data1$MARITAL>=9,NA,data1$MARITAL)
data1<-select(data1,-CHILDREN) #이상값이 많으므로 제거
data1$EDUCA<-ifelse(data1$EDUCA>=9,NA,data1$EDUCA)
data1$INCOME2<-ifelse(data1$INCOME2>=77,NA,data1$INCOME2)
data1<-select(data1,-WEIGHT2) #이상값이 많으므로 제거
data1<-select(data1,-HEIGHT3) #이상값이 많으므로 제거
data1$RENTHOM1<-ifelse(data1$RENTHOM1>=7,NA,data1$RENTHOM1)
data1$SEX<-ifelse(data1$SEX>=9,NA,data1$SEX)
data1$EXERANY2<-ifelse(data1$EXERANY2>=7,NA,data1$EXERANY2)
data1$SEATBELT<-ifelse(data1$SEATBELT>=7,NA,data1$SEATBELT)
data1$PNEUVAC3<-ifelse(data1$PNEUVAC3>=7,NA,data1$PNEUVAC3)
data1<-select(data1,-ALCDAY5) #이상값이 많으므로 제거
data1$HIVTST6<-ifelse(data1$HIVTST6>=7,NA,data1$HIVTST6)
data1<-select(data1,-QSTVER) #이상값이 많으므로 제거
data1<-select(data1,-QSTLANG) #이상값이 많으므로 제거
data1$DRNKANY5<-ifelse(data1$DRNKANY5>=7,NA,data1$DRNKANY5)

data1<-na.omit(data1)
colSums(is.na(data1))


table(data1$CHCOCNCR)
data1$CHCOCNCR<-data1$CHCOCNCR-1
str(data1)





for(i in 1:30){
  data1[,i]<-as.factor(data1[,i])
}
data1$WEIGHT2<-as.numeric(data1$WEIGHT2)
data1$HEIGHT3<-as.numeric(data1$HEIGHT3)
data1$HTIN4<-as.numeric(data1$HTIN4)
data1$HTM4<-as.numeric(data1$HTM4)
data1$WTKG3<-as.numeric(data1$WTKG3)




