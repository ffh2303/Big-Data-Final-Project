library(dplyr)
library(data.table)
library(caret)

mydata<-read.csv("master.csv",TRUE,",")
mydata<- select(mydata,App,Category,Rating,Genres,Reviews,Installs,Type,Price,Content.Rating,Android.Ver)

    
null_values<-is.na(mydata)
null_values
sum_null <- sum(is.na(mydata))
sum_null
sum_data <-sum(nrow(mydata))
sum_data
null_percent <-(sum_null/sum_data)*100
null_percent
newtable <-data.frame(na.omit(mydata))
rownames(newtable)<-NULL
newtable
index = sort(sample(nrow(newtable),nrow(newtable)*0.8))
train_data<-newtable[index,]
test_data<-newtable[-index,]
rownames(train_data)<-NULL
rownames(test_data)<-NULL

write.csv(train_data, file="~/CSC 3303_Big Data Analytics/Project/Training_set.csv")
write.csv(test_data, file="~/CSC 3303_Big Data Analytics/Project/Test_set.csv")
