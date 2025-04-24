weight =c(63,64,66,68,69,71,72,73,74,75)
height =c(127,121,143,146,150,153,160,168,174,176)

data1 =data.frame(weight,height)

m1 =lm(formula=weight~height,data = data1)

m1

summary(m1)

plot(height,weight,main = "Best fit line")
abline(m1,col="Blue")


a=data.frame(height=170)
result =predict(m1,newdata = a)

print(result)
 