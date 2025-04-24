install.packages("dplyr")
install.packages("mlbench")
install.packages("ROCR")
install.packages("caTools")
install.packages("caret")


library(dplyr)
library(mlbench)
library(ROCR)
library(caTools)
library(caret)
library(datasets)
library(ggplot2)


data("PimaIndiansDiabetes")
?PimaIndiansDiabetes

View(PimaIndiansDiabetes)
data1 =PimaIndiansDiabetes

data1

summary(data1)

data1$diabetes  = gsub("pos","1",data1$diabetes)
data1$diabetes = gsub("neg","0",data1$diabetes)

data1$diabetes =as.factor(data1$diabetes)

summary(data1)


attach(data1)

#splittig and model making is occure
#/////////////////////////////////////////////////////////////

split<-sample.split(data1,SplitRatio = 0.8)
split

train_reg<-subset(data1,split =="TRUE")
test_reg <-subset(data1,split=="FALSE")


logistic_model <-glm(diabetes~pregnant +glucose+pressure+triceps+ insulin+mass+pedigree+age,data=train_reg,family="binomial")

summary(logistic_model)


#using model to predict data from test

test_reg$predict_reg <- predict(logistic_model,test_reg,type="response")

View(test_reg)

test_reg$predict_reg1 <- ifelse(test_reg$predict_reg>0.5,1,0)

View(test_reg)


ggplot(test_reg,aes(x=pregnant+glucose+pressure+triceps+insulin+mass+pedigree+age,y=predict_reg1)) +geom_point(alpha=.5) +
  stat_smooth(method="glm",se=FALSE,method.args = list(family=binomial()))


confmat =table(test_reg$diabetes,test_reg$predict_reg1)
confusionMatrix(confmat,positive="1",mode="everything")



