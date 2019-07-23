library(dplyr)
library(data.table)
library(caret)
library(ggplot2)
library(tidyr)
library(broom)

data_train<-read.csv("Clean_set v2.csv",TRUE,",")
data_train

#----------------Skip this--------------------
tidy_data <- data_train %>% filter(Category %in% c("GAME","SOCIAL"))%>% 
  select(Category,Rating,Installs,Reviews, Price, Type)
head(tidy_data)

tidy_data2 <- data_train%>% 
  select(Category,Rating,Installs,Reviews, Price, Type)

#----------------Ignore this------------------------------
write.csv(tidy_data, file="~/CSC 3303_Big Data Analytics/Project/2May2019.csv")
#--------------------------------------------------------
pairs.panels(tidy_data)
pairs.panels(tidy_data2)

str(tidy_data)
str(tidy_data2)

class(tidy_data$Rating)
class(tidy_data$Type)
class(tidy_data$Reviews)
class(tidy_data$Category)
#---------------------To scatter plot-----------------------


ggplot(filter(tidy_data,Category%in% c("GAME","SOCIAL")),
       aes(x=Rating,
           y=Price,
           color=Category))+
  geom_point()

#-------------------To second scatter plot--------------------
ggplot(filter(tidy_data,Category%in% c("GAME","SOCIAL")),
       aes(x=Rating,
           y=Reviews,
           color=Category))+
  geom_point()

#---------------------To third scatter plot--------------------


ggplot(filter(tidy_data,Category%in% c("GAME","SOCIAL")),
       aes(x=Rating,
           y=Installs,
           color=Category))+
  geom_point()

ggplot(filter(tidy_data2),
       aes(x=Category,
           y=Installs,
           color=Category))+
  geom_point()



#------------------------K-Means (ignore)----------------------------------

test_1<-data_train%>%select(Category,Rating,Reviews)

test_1$Category=NULL

test_1

test_cluster<-kmeans(test_1,10)

test_cluster1<-dbscan

test_cluster
test_cluster$size

plot(tidy_data[c("Installs","Rating")],col=test_cluster$cluster)

#------------------------Multiple Linear Regression-----------------------

head(tidy_data)

linear_model1 <- lm(Rating ~ Installs + Reviews + Price + Category, data = tidy_data)

linear_model2 <- lm(Rating ~ Installs + Reviews + Price + Type + Category, data = tidy_data)

linear_model3 <- lm(Type ~ Installs + Reviews + Rating + Category, data = tidy_data)

linear_model4 <- lm(Price ~ Installs + Reviews + Rating + Category, data = tidy_data)

linear_model5 <- lm(Rating ~ Installs + Reviews + Price + Type + Category, data = tidy_data)

summary(linear_model1)
summary(linear_model2)
summary(linear_model3)
summary(linear_model4)
summary(linear_model5)

#--------Prediction of rating in some context------------------------ 
predict(linear_model1, data.frame("Installs" =5000000, "Reviews" =10000, "Price" = 
                                    0, "Type" = 0, "Category" = "GAME"))

predict(linear_model2, data.frame("Installs" =5000000, "Reviews" =10000, "Price" = 
                                    0, "Type" = 0, "Category" = "GAME"))

predict(linear_model3, data.frame("Installs" =512120, "Reviews" =15322,
                                  "Rating"=5, "Category" = "GAME"))


predict(linear_model4, data.frame("Installs" =5000000, "Reviews" =100000,
                                  "Rating"=5, "Category" = "GAME"))

predict(linear_model1, data.frame("Installs" =500000000, "Reviews" =410112, "Price" = 
                                    31.99, "Type" = 1, "Category" = "GAME"))

predict(linear_model1, data.frame("Installs" =500000000, "Reviews" =1000000, "Price" = 
                                    1.99, "Type" = 1, "Category" = "GAME"))

#---------------------Visualization---------------------------------------


ggplot(augment(linear_model5), aes(x=Installs,
                                   y=Rating,
                                   color = Category)) + 
  geom_point() + 
  geom_line(aes(y= .fitted), size=1) + 
  labs(x="Number of installations",
       y="Rating",
       title = "LM for Model 5")


ggplot(augment(linear_model5), aes(x=Reviews,
                                   y=Rating,
                                   color = Category)) + 
  geom_point() + 
  geom_line(aes(y= .fitted), size=1) + 
  labs(x="Number of installations",
       y="Rating",
       title = "LM for Model 5")


ggplot(augment(linear_model3), aes(x=Reviews,
                                   y=Rating,
                                   color = Category)) + 
  geom_point() + 
  geom_line(aes(y= .fitted), size=1) + 
  labs(x="Number of installations",
       y="Rating",
       title = "LM for Model 5")




