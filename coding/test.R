library(dplyr)
library(reshape)
library(data.table)
library(SASxport)
library(Hmisc)
library(gmodels)


getwd()
setwd("D:\\behavioral\\project")
setwd("C:\\Users\\gusgh\\AppData\\Local\\Temp\\Rtmps7H9pv\\downloaded_packages")
list.files()


#데이터 불러오기
a<-read.xport("D:\\behavioral\\project\\2016.xpt")
d<-read.csv("D:\\behavioral\\project\\2013.csv")
a1<-data.table(a)
a2<-tbl_df(a)

#결측값
colSums(is.na(b1))
b1<-select(tbl_df(a1),X.STATE:FLSHTMY2)
str(b1)
summary(b1)
b1<-select(b1,-(FMONTH:IYEAR))
b1<-select(b1,-COLGHOUS,-STATERES,-SEQNO,-LADULT)
b1<-select(b1,-CCLGHOUS,-ASTHNOW,-DIABAGE2,-NUMPHON2,-PREGNANT,-STOPSMK2,-ECIGNOW)
names(a1)
b2<-select(tbl_df(a1),FLSHTMY2:CSRVDOC1)
colSums(is.na(b2))
b2<-select(b2,-HPLSTTST,-INSULIN,-BLDSUGAR,-FEETCHK2,-DOCTDIAB,-CHKHEMO3,-FEETCHK,-EYEEXAM,-DIABEYE,-DIABEDU,-PAINACT2,-QLMENTL2,-QLSTRES2,-QLHLTH2,-MEDICARE,-HLTHCVR1,-DELAYMED)
b2<-select(b2,-(DLYOTHER:MEDBILL1))
b2<-select(b2,-(CAREGIV1:CALRINFO))
b2<-select(b2,-(USEMRJNA:CSRVDOC1))
#row na값 40만개 이상 결측값 제거하다가 아닌듯 싶어서 스톱

#변수선택
b1<-data.table(a1)
colSums(!is.na(b1))
b1<-select(a1,X.STATE,LADULT,NUMADULT,NUMMEN,NUMWOMEN,CADULT,PVTRESD3,CCLGHOUS,HHADULT,GENHLTH,PHYSHLTH,MENTHLTH,HLTHPLN1,EXERANY2,SLEPTIM1,CVDINFR4,CVDCRHD4,CVDSTRK3,ASTHNOW,CHCSCNCR,CHCOCNCR,CHCCOPD1,HAVARTH3,ADDEPEV2,CHCKIDNY,LASTDEN3,RMVTETH3,SEX,EDUCA,RENTHOM1,EMPLOY1,CHILDREN,INCOME2,WEIGHT2,HEIGHT3,DEAF,SMOKE100,SMOKDAY2,USENOW3,ALCDAY5,AVEDRNK2,FLUSHOT6,PNEUVAC3,TETANUS,SEATBELT,DRNKDRI2,HADMAM,HOWLONG,HADPAP2,LASTPAP2,HPVTEST,HPLSTTST,HADHYST2,PCPSAAD2,PSATEST1,PSATIME,PCPSARS1,HIVTST6,HIVTSTD3,PDIABTST,INSULIN,BLDSUGAR,FEETCHK2,SXORIENT,TRNSGNDR)
b1$LADULT
table(b1$LADULT)

#폐질환여부와 흡연정도에 따른 카이제곱분석
b2<-select(b1,CHCCOPD1,SMOKDAY2)
b2<-select(b1,CHCCOPD1,SMOKE100)
b2<-select(b1,-SMOKE100)
b2<-na.omit(b2)
table(b2$CHCCOPD1)
table(b2$SMOKDAY2)
table(b2$SMOKE100)
b2$CHCCOPD1<-ifelse(b2$CHCCOPD1>=7,NA,b2$CHCCOPD1)
b2$SMOKDAY2<-ifelse(b2$SMOKDAY2>=7,NA,b2$SMOKDAY2)
b2$SMOKE100<-ifelse(b2$SMOKE100>=7,NA,b2$SMOKE100)


b2<-data.table(b2)
head(b2)
chisq.test(b2)
CrossTable(b2,margin(1,9))
with(Cars93, CrossTable(Type, Cylinders, expected=TRUE, chisq=TRUE))
with(b2, CrossTable(CHCCOPD1, SMOKDAY2, expected=TRUE, chisq=TRUE))
with(b2, CrossTable(CHCCOPD1, SMOKE100, expected=TRUE, chisq=TRUE))

str(b2)




describe()


#암(피부암제외)과 13개(X.STATE	SEX	EDUCA	RENTHOM1	EMPLOY1	EXERANY2	SLEPTIM1	WEIGHT2	HEIGHT3
#CVDSTRK3	DEAF	SMOKDAY2	ADDEPEV2)
b2<-select(b1,CHCOCNCR,X.STATE)
table(b2$CHCOCNCR)
table(b2$X.STATE)
b2$CHCOCNCR<-ifelse(b2$CHCOCNCR>=7,NA,b2$CHCOCNCR)
b2<-na.omit(b2)
chisq.test(b2)
chisq.test(b2$CHCOCNCR,b2$X.STATE)
with(b2, CrossTable(CHCOCNCR, X.STATE, expected=TRUE, chisq=TRUE))
CrossTable(b2$CHCOCNCR, b2$X.STATE, chisq=TRUE)


b2<-select(b1,CHCOCNCR,SEX)
table(b2$CHCOCNCR)
table(b2$SEX)
b2$CHCOCNCR<-ifelse(b2$CHCOCNCR>=7,NA,b2$CHCOCNCR)
b2$SEX<-ifelse(b2$SEX>=9,NA,b2$SEX)
b2<-na.omit(b2)
chisq.test(b2)
chisq.test(b2$CHCOCNCR,b2$SEX)
CrossTable(b2$CHCOCNCR, b2$SEX,chisq=TRUE)

b2<-select(b1,CHCOCNCR,EDUCA)
table(b2$EDUCA)
b2$CHCOCNCR<-ifelse(b2$CHCOCNCR>=7,NA,b2$CHCOCNCR)
b2$EDUCA<-ifelse(b2$EDUCA>=9,NA,b2$EDUCA)
b2<-na.omit(b2)
chisq.test(b2)
chisq.test(b2$CHCOCNCR,b2$EDUCA)
CrossTable(b2$CHCOCNCR, b2$EDUCA,expected=T,chisq=TRUE)

b2<-select(b1,CHCOCNCR,RENTHOM1)
table(b2$RENTHOM1)
b2$CHCOCNCR<-ifelse(b2$CHCOCNCR>=7,NA,b2$CHCOCNCR)
b2$RENTHOM1<-ifelse(b2$RENTHOM1>=7,NA,b2$RENTHOM1)
b2<-na.omit(b2)
chisq.test(b2)
chisq.test(b2$CHCOCNCR,b2$RENTHOM1)
CrossTable(b2$CHCOCNCR, b2$RENTHOM1,chisq=TRUE)

b2<-select(b1,CHCOCNCR,EXERANY2)
table(b2$EXERANY2)
b2$CHCOCNCR<-ifelse(b2$CHCOCNCR>=7,NA,b2$CHCOCNCR)
b2$EXERANY2<-ifelse(b2$EXERANY2>=7,NA,b2$EXERANY2)
b2<-na.omit(b2)
chisq.test(b2)
chisq.test(b2$CHCOCNCR,b2$EXERANY2)
CrossTable(b2$CHCOCNCR, b2$EXERANY2,expected=T,chisq=TRUE)

b2<-select(b1,CHCOCNCR,SLEPTIM1)
table(b2$SLEPTIM1)
b2$CHCOCNCR<-ifelse(b2$CHCOCNCR>=7,NA,b2$CHCOCNCR)
b2$SLEPTIM1<-ifelse(b2$SLEPTIM1>=77,NA,b2$SLEPTIM1)
b2<-na.omit(b2)
chisq.test(b2)
chisq.test(b2$CHCOCNCR,b2$SLEPTIM1)
CrossTable(b2$CHCOCNCR, b2$SLEPTIM1,expected=T,chisq=TRUE)

b2<-select(b1,CHCOCNCR,HEIGHT3)
table(b2$HEIGHT3)
b2$CHCOCNCR<-ifelse(b2$CHCOCNCR>=7,NA,b2$CHCOCNCR)
b2$HEIGHT3<-ifelse(b2$HEIGHT3>=9999,NA,b2$HEIGHT3)
b2<-na.omit(b2)
chisq.test(b2)
chisq.test(b2$CHCOCNCR,b2$HEIGHT3)
CrossTable(b2$CHCOCNCR, b2$HEIGHT3,expected=T,chisq=TRUE)

b2<-select(b1,CHCOCNCR,CVDSTRK3)
table(b2$CVDSTRK3)
b2$CHCOCNCR<-ifelse(b2$CHCOCNCR>=7,NA,b2$CHCOCNCR)
b2$CVDSTRK3<-ifelse(b2$CVDSTRK3>=9,NA,b2$CVDSTRK3)
b2<-na.omit(b2)
chisq.test(b2)
chisq.test(b2$CHCOCNCR,b2$CVDSTRK3)
CrossTable(b2$CHCOCNCR, b2$CVDSTRK3,expected=T,chisq=TRUE)

b2<-select(b1,CHCOCNCR,SMOKDAY2)
table(b2$SMOKDAY2)
b2$CHCOCNCR<-ifelse(b2$CHCOCNCR>=7,NA,b2$CHCOCNCR)
b2$SMOKDAY2<-ifelse(b2$SMOKDAY2>=7,NA,b2$SMOKDAY2)
b2<-na.omit(b2)
chisq.test(b2)
chisq.test(b2$CHCOCNCR,b2$SMOKDAY2)
CrossTable(b2$CHCOCNCR, b2$SMOKDAY2,expected=T,chisq=TRUE)

b2<-select(b1,CHCOCNCR,ADDEPEV2)
table(b2$ADDEPEV2)
b2$CHCOCNCR<-ifelse(b2$CHCOCNCR>=7,NA,b2$CHCOCNCR)
b2$ADDEPEV2<-ifelse(b2$ADDEPEV2>=9,NA,b2$ADDEPEV2)
b2<-na.omit(b2)
chisq.test(b2)
chisq.test(b2$CHCOCNCR,b2$ADDEPEV2)
CrossTable(b2$CHCOCNCR, b2$ADDEPEV2,expected=T,chisq=TRUE)


#독립성검정
#암(피부암제외)과 13개(X.STATE	SEX	EDUCA	RENTHOM1	EMPLOY1	EXERANY2	SLEPTIM1	WEIGHT2	HEIGHT3
#CVDSTRK3	DEAF	SMOKDAY2	ADDEPEV2)
b2<-select(b1,X.STATE,EDUCA)
table(b2$X.STATE)
table(b2$EDUCA)
b2$EDUCA<-ifelse(b2$EDUCA>=9,NA,b2$EDUCA)
b2<-na.omit(b2)
chisq.test(b2)
chisq.test(b2$X.STATE,b2$EDUCA)
with(b2, CrossTable(CHCOCNCR, X.STATE, expected=TRUE, chisq=TRUE))
CrossTable(b2$X.STATE, b2$EDUCA, chisq=TRUE)


#각 범주형 데이터를 Dummy지시변수로 변환
install.packages("dummies")
library(dummies)

#성별 더미변수 설정
b3<-transform(b2, SEX_1 = ifelse(SEX==1,1,0), SEX_2=ifelse(SEX==2,1,0))
b3<-na.omit(b3)
table(b3$CHCOCNCR)
table(b3$SEX)
head(b3)
table(b3$CHCOCNCR)
b3$CHCOCNCR<-b3$CHCOCNCR-1

#교육수준 더미변수 설정
b2<-select(b1,CHCOCNCR,SEX,EDUCA,SMOKDAY2)
b2$CHCOCNCR<-ifelse(b2$CHCOCNCR>=7,NA,b2$CHCOCNCR)
b2$SEX<-ifelse(b2$SEX>=7,NA,b2$SEX)
b2$EDUCA<-ifelse(b2$EDUCA>=9,NA,b2$EDUCA)
b2$SMOKDAY2<-ifelse(b2$SMOKDAY2>=7,NA,b2$SMOKDAY2)
b2<-na.omit(b2)
b3<-transform(b2, EDUCA_1 = ifelse(EDUCA==1,1,0), 
                  EDUCA_2=ifelse(EDUCA==2,1,0),
                  EDUCA_3=ifelse(EDUCA==3,1,0),
                  EDUCA_4=ifelse(EDUCA==4,1,0),
                  EDUCA_5=ifelse(EDUCA==5,1,0),
                  EDUCA_6=ifelse(EDUCA==6,1,0),
                  SMOKDAY2_1 = ifelse(EDUCA==1,1,0), 
                  SMOKDAY2_2=ifelse(EDUCA==2,1,0),
                  SMOKDAY2_3=ifelse(EDUCA==3,1,0))
head(b3)

#주성분분석
b3<-na.omit(b3)
head(b3)
o1<-prcomp(b3[,c(5:10)])
o2<-prcomp(b3[,c(11:13)])
#summary(prcomp(b3[,c(3:8)]),scale=T)
summary(o1)
print(o1)
summary(o2)
print(o2)


o<-prcomp(b3[,c(3:8)])
summary(o)
cor(b3$CHCOCNCR,o$x[,1])
qqnorm(o$x[,1])
print(prcomp(b3[,c(3:8)]))

#주성분분석
b4



#로지스틱회귀분석모형
r<-data.table(b3$CHCOCNCR,b3$SEX,o1$x[,1:6],o2$x[,1:2])
head(r)
r$V1<-r$V1-1
r$V2<-r$V2-1
colnames(r)<-c("V1","V2","P1","P2","P3","P4","P5","P6","P11","P12")
head(r)
b2$SEX<-ifelse(b2$SEX>=7,NA,b2$SEX)
r_train<-r[1:200000]
r_test<-r[200001:202788]

model<-glm(V1~V2+P1+P2+P3+P11+P12,r_train,family="binomial")
summary(model)
anova(model,test="Chisq")
p<-predict(model,newdata = r_test,type = "response")
round(p)
table(round(p),r_test$V1)

o<-glm(CHCOCNCR~EDUCA_1+EDUCA_2+EDUCA_3+EDUCA_4+EDUCA_5+EDUCA_6,data=b3,family=binomial)
summary(o)


#암(피부암제외)과 13개(X.STATE	SEX	EDUCA	RENTHOM1	EMPLOY1	EXERANY2	SLEPTIM1	WEIGHT2	HEIGHT3
#CVDSTRK3	DEAF	SMOKDAY2	ADDEPEV2)
b2<-select(b1,CHCOCNCR,X.STATE,	SEX,	EDUCA,	RENTHOM1,	EMPLOY1,	EXERANY2,	SLEPTIM1,	WEIGHT2,	HEIGHT3,
          CVDSTRK3,	DEAF,	SMOKDAY2,	ADDEPEV2)
head(b2)
b2$CHCOCNCR<-ifelse(b2$CHCOCNCR>=6,NA,b2$CHCOCNCR)
b2$CHCOCNCR<-b2$CHCOCNCR-1
b2<-na.omit(b2)
b2<-data.table(b2)
b2_train<-b2[1:200000]
b2_test<-b2[200001:202788]
model<-glm(CHCOCNCR~X.STATE	+SEX+	EDUCA+	RENTHOM1+	EMPLOY1+	EXERANY2+	SLEPTIM1+	WEIGHT2+	HEIGHT3
           +CVDSTRK3	+DEAF+	+SMOKDAY2+	ADDEPEV2,b2_train,family="binomial")
summary(model)
anova(model,test="Chisq")
p<-predict(model,newdata = b2_test,type = "response")
 round(p)
table(round(p),b2_test$CHCOCNCR )

o<-glm(CHCOCNCR~EDUCA_1+EDUCA_2+EDUCA_3+EDUCA_4+EDUCA_5+EDUCA_6,data=b3,family=binomial)
summary(o)



#랜덤포레스트
install.packages("randomForest")
library(randomForest)
b2<-na.omit(b2)
ind<-sample(2,nrow(b2),replace = TRUE,prob=c(0.7,0.3))
trainData<-b2[ind==1,]
testData<-b2[ind==2,]
rf<-randomForest(CHCOCNCR~.,data = trainData,ntree=10,proximity=TRUE)




cdplot(EDUCA_1~CHCOCNCR,b3)
cdplot(EDUCA_2~CHCOCNCR,b3)
cdplot(EDUCA_3~CHCOCNCR,b3)
cdplot(EDUCA_4~CHCOCNCR,b3)
cdplot(EDUCA_5~CHCOCNCR,b3)
cdplot(EDUCA_6~CHCOCNCR,b3)













#각 컬럼에 대한 히스토그램




























#변수명 수정
a2<-dplyr::rename(a2,"주"="X.STATE")
a2<-dplyr::rename(a2,"성인여부"="LADULT")
a2<-dplyr::rename(a2,"가구의 성인 수"="NUMADULT")
a2<-dplyr::rename(a2,"가구의 성인 남성수"="NUMMEN")
a2<-dplyr::rename(a2,"가구의 성인 여성수"="NUMWOMEN")
a2<-dplyr::rename(a2,"성인여부"="CADULT")
a2<-dplyr::rename(a2,"개인거주지에사는가(자택,아파트)"="PVTRESD3")
a2<-dplyr::rename(a2,"기숙사에 사는가"="CCLGHOUS")
a2<-dplyr::rename(a2,"가구의 18세 이상구성원"="HHADULT")
a2<-dplyr::rename(a2,"건강상태"="GENHLTH")
a2<-dplyr::rename(a2,"신체상태가 좋지않은 일수(한달)"="PHYSHLTH")
a2<-dplyr::rename(a2,"정신상태가 좋지않은 일수(한달)"="MENTHLTH")
a2<-dplyr::rename(a2,"건강보험가입여부"="HLTHPLN1")
a2<-dplyr::rename(a2,"운동여부(한달)"="EXERANY2")
a2<-dplyr::rename(a2,"수면시간"="SLEPTIM1")
a2<-dplyr::rename(a2,"심근 경색증 경험여부"="CVDINFR4")
a2<-dplyr::rename(a2,"협심증과 관상동맥질환 여부"="CVDCRHD4")
a2<-dplyr::rename(a2,"천식경험"="CVDSTRK3")
a2<-dplyr::rename(a2,"천식여부"="ASTHNOW")
a2<-dplyr::rename(a2,"피부암외의 암진단여부"="CHCOCNCR")
a2<-dplyr::rename(a2,"피부암여부"="CHCSCNCR")
a2<-dplyr::rename(a2,"폐질환여부"="CHCCOPD1")
a2<-dplyr::rename(a2,"관절,류마티스관절염,통풍,루푸,섬유근육통여부"="HAVARTH3")
a2<-dplyr::rename(a2,"우울증여부"="ADDEPEV2")
a2<-dplyr::rename(a2,"신장질환여부"="CHCKIDNY")
a2<-dplyr::rename(a2,"치과방문(마지막)"="LASTDEN3")
a2<-dplyr::rename(a2,"영구치제거정도"="RMVTETH3")
a2<-dplyr::rename(a2,"성별"="SEX")
a2<-dplyr::rename(a2,"학력"="EDUCA")
a2<-dplyr::rename(a2,"거주형태"="RENTHOM1")
a2<-dplyr::rename(a2,"근로형태"="EMPLOY1")
a2<-dplyr::rename(a2,"청소년거주명수"="CHILDREN")
a2<-dplyr::rename(a2,"몸무게"="WEIGHT2")
a2<-dplyr::rename(a2,"신장"="HEIGHT3")
a2<-dplyr::rename(a2,"연간가구소득"="INCOME2")
a2<-dplyr::rename(a2,"청각장애유무"="DEAF")
a2<-dplyr::rename(a2,"적어도100개비의 흡연"="SMOKE100")
a2<-dplyr::rename(a2,"흡연정도"="SMOKDAY2")
a2<-dplyr::rename(a2,"스너프사용정도"="USENOW3")
a2<-dplyr::rename(a2,"음주횟수(월)"="ALCDAY5")
a2<-dplyr::rename(a2,"음주정도(월)"="AVEDRNK2")
a2<-dplyr::rename(a2,"폐렴발병여부"="PNEUVAC3")
a2<-dplyr::rename(a2,"독감백신투약여부"="FLUSHOT6")
a2<-dplyr::rename(a2,"파상풍발작여부"="TETANUS")
a2<-dplyr::rename(a2,"자동차탑승시안전벨트여부"="SEATBELT")
a2<-dplyr::rename(a2,"음주운전횟수(월)"="DRNKDRI2")
a2<-dplyr::rename(a2,"유방X사진촬영경험"="HADMAM")
a2<-dplyr::rename(a2,"유방X선촬영경험일자(마지막)"="HOWLONG")
a2<-dplyr::rename(a2,"팹테스트경험여부"="HADPAP2") #팹테스트(자궁경부암검사)
a2<-dplyr::rename(a2,"팹테스트(마지막)"="LASTPAP2")
a2<-dplyr::rename(a2,"HPV검사여부"="HPVTEST") #HPV테스트(자궁암 및 유방암검사)
a2<-dplyr::rename(a2,"HPV검사여부(마지막)"="HPLSTTST")
a2<-dplyr::rename(a2,"자궁적출술여부"="HADHYST2")
a2<-dplyr::rename(a2,"전립선긍정적대화"="PCPSAAD2") #PSA검사(전립선특이항원검사)
a2<-dplyr::rename(a2,"전립선검사여부"="PSATEST1")
a2<-dplyr::rename(a2,"전립선검사여부(마지막)"="PSATIME")
a2<-dplyr::rename(a2,"전립선검를한이유"="PCPSARS1")
a2<-dplyr::rename(a2,"HIV검사여부"="HIVTST6")
a2<-dplyr::rename(a2,"HIV검사일(마지막)"="HIVTSTD3")