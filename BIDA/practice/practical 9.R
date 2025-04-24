Height=c(27,26,42,47,57,58,60,64,70,73)
weight=c(121,125,130,134,138,140,145,149,150,153)


data1 =data.frame(Height,weight)


data1

m1 = lm(formula=weight~Height,data = data1)

m1

plot(Height,weight,main = "Best Fit line")
abline(m1,col="Blue")


a= data.frame(Height=170)

result= predict(m1,newdata=a)

print(result)