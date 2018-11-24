##### 1. 11년 부터 16년 까지의 파일 합 #####
##### ..L package library #####
library(SASxport)    
library(dplyr)        
library(ggplot2)
library(dplyr)
library(sqldf)
library(gcookbook)
library(formattable)

a <- read.table("D:/project_r/data/2011.csv", sep = ",", header = TRUE)
b <- read.table("D:/project_r/data/2012.csv", sep = ",", header = TRUE)
c <- read.table("D:/project_r/data/2013.csv", sep = ",", header = TRUE)
d <- read.table("D:/project_r/data/2014.csv", sep = ",", header = TRUE)
e <- read.table("D:/project_r/data/2015.csv", sep = ",", header = TRUE)
f <- read.xport("D:/project_r/data/2016.xpt")  #연도별 파일 불러오기

a <- fread("D:/project_r/data/2011.csv", sep = ",", header = TRUE)
b <- fread("D:/project_r/data/2012.csv", sep = ",", header = TRUE)
c <- fread("D:/project_r/data/2013.csv", sep = ",", header = TRUE)
d <- fread("D:/project_r/data/2014.csv", sep = ",", header = TRUE)
e <- fread("D:/project_r/data/2015.csv", sep = ",", header = TRUE) #연도별 파일 불러오기

a <- select(a, which(colnames(a) %in% colnames(b)))
# a와 b의 공통컬럼을 찾은 후 a에 반환
a <- select(a, which(colnames(a) %in% colnames(c)))
a <- select(a, which(colnames(a) %in% colnames(d)))
a <- select(a, which(colnames(a) %in% colnames(e)))
a <- select(a, which(colnames(a) %in% colnames(f)))
# a, b, c, d, e, f의 동일한 컬럼을 a에 반환

b <- select(b, which(colnames(b) %in% colnames(a)))
c <- select(c, which(colnames(c) %in% colnames(a)))
d <- select(d, which(colnames(d) %in% colnames(a)))
e <- select(e, which(colnames(e) %in% colnames(a)))
f <- select(f, which(colnames(f) %in% colnames(a)))
# b, c, d, e, f도 동일한 컬럼만 반환

a <- as.data.frame(a)
b <- as.data.frame(b)
c <- as.data.frame(c)
d <- as.data.frame(d)
e <- as.data.frame(e)
f <- as.data.frame(f)
# rbind하기 위해 data.frame 형식으로 변경

data <- rbind(a, b, c, d, e, f)
rm(a, b, c, d, e, f)


##### 2. save.image#####
save.image('D:/project_r/project/rdata/data.rdata') 
# 위 경로로 .rdata 저장 // R은 경고없이 덮어씌여지므로 주의!
#####...L load #####
load('D:/project_r/project/rdata/data.rdata')
