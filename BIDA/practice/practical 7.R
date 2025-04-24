install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)


?iris
View(iris)
summary(iris)
table(iris)


set.seed(1034)
s=sample(c(1:150),siz=100)

train_data= iris[s,]
test_data=iris[-s,]

table(train_data$Species)
train_data

table(train_data$Species)

m2 =rpart(train_data$Species~.,data=train_data,method = "class")
rpart.plot(m2)

test_data$Predict = predict(m2,test_data,type = "class")

table(test_data$Species,test_data$Predict)

