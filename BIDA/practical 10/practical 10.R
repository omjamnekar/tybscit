install.packages("dplyr")
install.packages("mlbench")
install.packages("caTools")
install.packages("ROCR")
install.packages("ggplot2")
install.packages("caret")

install.packages("lattice")
library(dplyr)
library(mlbench)
library(caTools)
library(ROCR)
library(ggplot2)
library(caret)
library(lattice)

data("PimaIndiansDiabetes")

?PimaIndiansDiabetes
View(PimaIndiansDiabetes)

data1 =PimaIndiansDiabetes

str(PimaIndiansDiabetes)

summary(PimaIndiansDiabetes)


data1$diabetes  =gsub("pos","1",data1$diabetes)
data1$diabetes  =gsub("neg","0",data1$diabetes)


data1$diabetes= as.factor(data1$diabetes)

summary(data1)

attach()



split<- sample.split(data1,SplitRatio = 0.8)

split


train_reg<-subset(data1,split="TRUE")
test_reg<-subset(data1,split="FALSE")

logistic_model<-glm(diabetes ~ pregnant + glucose +pressure +triceps+insulin +mass + pedigree+age,data =train_reg , family="binomial")

logistic_model


summary(logistic_model)

test_reg$predict_reg<-predict(logistic_model,test_reg,type="response")

View(test_reg)

test_reg$predict_reg1<ifelse(test_reg$predict_reg>0.5,1,0)

View(test_reg)

ggplot(test_reg ,aes(x=pregnant+glucose+pressure+triceps+insulin+mass+predigree +age,y=predict_reg1))
+geom_point(alpha=.5)+

  stat_smooth(method="glm",se=FALSE,method.args = list(family=binomial()))



confmat =table(test_reg$predict_reg1)

confusionMatrix(confmat,positive="1",mode="everything")
