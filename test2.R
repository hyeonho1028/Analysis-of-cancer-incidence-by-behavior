library(dplyr)
library(reshape)
library(data.table)
library(SASxport)
library(Hmisc)
library(gmodels)


a1<-tbl_df(a)
names(a1)

#변수선택
#암(피부암제외)과 13개(X.STATE	SEX	EDUCA	RENTHOM1	EMPLOY1	EXERANY2	SLEPTIM1	WEIGHT2	HEIGHT3
#CVDSTRK3	DEAF	SMOKDAY2	ADDEPEV2)
b1<-select(a1,CHCOCNCR,X.STATE,	SEX,	EDUCA,	RENTHOM1,	EMPLOY1,	EXERANY2,	SLEPTIM1,	WEIGHT2,	HEIGHT3,
           CVDSTRK3,	DEAF,	SMOKDAY2,	ADDEPEV2)


#로지스틱회귀모형
is.na(b1)
colSums(is.na(b1))
b1$CHCOCNCR<-b1$CHCOCNCR-1
b1$CHCOCNCR<-ifelse(b1$CHCOCNCR>=6,NA,b1$CHCOCNCR)
table(b1$CHCOCNCR)
ind<-sample(2,nrow(b1),replace = TRUE,prob=c(0.7,0.3))
trainData<-b1[ind==1,]
testData<-b1[ind==2,]
model<-glm(CHCOCNCR~X.STATE	+SEX+	EDUCA+	RENTHOM1+	EMPLOY1+	EXERANY2+	SLEPTIM1+	WEIGHT2+	HEIGHT3
           +CVDSTRK3	+DEAF+	+SMOKDAY2+	ADDEPEV2,trainData,family="binomial")
summary(model)
p<-predict(model,newdata = testData,type = "response")
round(p)
table(round(p),testData$CHCOCNCR )


set.seed(1234)

#의사결정트리
install.packages("tree")
install.packages("e1071")
install.packages("caret")
install.packages("party")
library(party)
library(caret)
library(tree)
library(e1071)
treemod<-tree(CHCOCNCR~X.STATE	+SEX+	EDUCA+	RENTHOM1+	EMPLOY1+	EXERANY2+	SLEPTIM1+	WEIGHT2+	HEIGHT3+CVDSTRK3	+DEAF+	+SMOKDAY2+	ADDEPEV2,data = trainData)
treemod<-tree(CHCOCNCR~X.STATE	+SEX+	EDUCA+	RENTHOM1+	EMPLOY1+	EXERANY2+	SLEPTIM1+	WEIGHT2+	HEIGHT3+CVDSTRK3	+DEAF+	+SMOKDAY2+	ADDEPEV2,data = trainData2)
trainData2<-na.omit(trainData)
treemod<-ctree(CHCOCNCR~X.STATE	+SEX+	EDUCA+	RENTHOM1+	EMPLOY1+	EXERANY2+	SLEPTIM1+	WEIGHT2+	HEIGHT3+CVDSTRK3	+DEAF+	+SMOKDAY2+	ADDEPEV2,data = trainData2)
plot(treemod)


plot(treemod)
text(treemod)

cv.trees<-cv.tree(treemod,FUN = prune.misclass)
cv.tree
treepred<-predict(treemod,testData)
confusionMatrix(treepred,testData$CHCOCNCR)
table(round(treepred),testData$CHCOCNCR )

treemod
pred<-predict(treemod,testData)
table(round(pred),testData$CHCOCNCR)



#랜덤포레스트
is.na(b1)
colSums(is.na(b1))
b1$CHCOCNCR<-b1$CHCOCNCR-1
b1$CHCOCNCR<-ifelse(b1$CHCOCNCR>=6,NA,b1$CHCOCNCR)
table(b1$CHCOCNCR)
summary(b1$CHCOCNCR)
b1<-data.table(b1)
b1$CHCOCNCR<-na.omit(b1$CHCOCNCR)
b1<-na.omit(b1)

head(b1)
set.seed(1)

ind<-sample(2,nrow(b1),replace = TRUE,prob=c(0.7,0.3))
trainData<-b1[ind==1,]
testData<-b1[ind==2,]

library(randomForest)
model1<-randomForest(CHCOCNCR~X.STATE	+SEX+	EDUCA+	RENTHOM1+	EMPLOY1+	EXERANY2+	SLEPTIM1+	WEIGHT2+	HEIGHT3+CVDSTRK3	+DEAF+	+SMOKDAY2+	ADDEPEV2,data = trainData)
model2<-randomForest(CHCOCNCR~.,data = trainData,ntree=300,mtry=14,na.action = na.omit)
model2<-randomForest(CHCOCNCR~.,data = trainData,ntree=1,mtry=14,na.action = na.omit)
model2<-randomForest(CHCOCNCR~.,data = trainData,importance=T,na.action = na.omit)



data(iris)
names(iris)
set.seed(1234)
ind<-sample(2,nrow(iris),replace = TRUE,prob=c(0.75,0.25))
trainData<-iris[ind==1,]
testData<-iris[ind==2,]

model1<-randomForest(Species~.,data = trainData,importance=T,ntree=10000,mtry=4)

model1
confusionMatrix(model1,testData)
importance(model1)

rf.pred<-predict(model1,testData)
table(rf.pred,testData$Species)


model1<-cforest(Species~.,data = trainData,controls = cforest_unbiased(mtry = 2, ntree=100)) 
varimp(model1)
rf.pred<-predict(model1,testData,OOB = TRUE,type="response")
table(rf.pred,testData$Species)





