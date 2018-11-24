#EDA

#ggplot 2가지 변수에 대한 boxplot x축에 들어가는 변수의 경우 수치형이 아니여야함
ggplot(b, aes(x=CHCOCNCR,y=INCOME2)) + geom_boxplot()
b<-subset(data1,select=c(CHCOCNCR,INCOME2))
b
class(b)
class(b$CHCOCNCR)
b$CHCOCNCR
head(b)
summary(b)
b<-tbl_df(b)
b$CHCOCNCR<-as.factor(b$CHCOCNCR)
b$CHCOCNCR<-ifelse(b$CHCOCNCR==0,"zero","one")


#with = false 를 넣을시 data.table 의 연산가능