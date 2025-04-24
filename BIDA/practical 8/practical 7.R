install.packages("tidyverse")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("conflicted")


library(tidyverse)
library(FactoMineR)
library(factoextra)
library(conflicted)

data(iris)

iris1=iris[,-5]
iris_label=iris[,5]


set.seed(1212)

wss =NULL

for(i in 1:10){
  
  fit= kmeans(iris1,centers = i)
  wss=c(wss,fit$tot.withinss)
}

plot(1:10,wss,type="o")


#/////////////////////

fviz_nbclust(iris1,kmeans,nstart=100,method="wss") + geom_vline(xintercept=3, linetype=1)

m2 =kmeans(iris,3)

m2$cluster

View(me)

table(iris$Species,m2$cluster)

fviz_cluster(m2 ,kmean,geom ="point",ellipse.type = "euclid")

