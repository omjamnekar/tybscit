install.packages("tidyverse")
install.packages("FactoMineR")
install.packages("factoextra")

library(tidyverse)
library(factoextra)
library(FactoMineR)


?iris
data(iris)

iris1 =iris[,-5]
iris_label=iris[,5]

wss =NULL

for(i in 1:10){
  fit= kmeans(iris1,i)
  wss =c(wss,fit$tot.withinss)
}

set.seed(1212)

plot(1:10,wss,type = "o")

fviz_nbclust(iris1,kmeans,nstart=100,method="wss")+geom_vline(xintercept=3,typeline=1)

m2=kmeans(iris1,centers = 3)

View(m2)

table(iris$Species,m2$cluster)

fviz_cluster(m2,data = iris1,geom = c("point"),ellipse.type = "euclid")
