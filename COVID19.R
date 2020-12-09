library(purrr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(plotly)

COVID19 = read.table("COVID19.txt", header = T, sep = "\t")
COVID19[652,c(5:7)] = 0


COVID19$Sex = ifelse(COVID19$Sex == "Male", 1, 2)
COVID19$AgeGroup = as.factor(COVID19$AgeGroup)
COVID19$AgeGroup = factor(COVID19$AgeGroup,levels(COVID19$AgeGroup),1:7)
COVID19$AgeGroup = as.integer(COVID19$AgeGroup)
COVID19 = COVID19[-which(COVID19$AgeGroup == 1),]
COVID19 = COVID19[-which(COVID19$AgeGroup == 2),]
COVID19 = COVID19[-which(COVID19$AgeGroup == 3),]
COVID19$AgeGroup = factor(COVID19$AgeGroup,4:7,1:4)
View(COVID19)
Y = COVID19$COVIDProp
factorA = COVID19$AgeGroup
factorB = COVID19$Sex
factorA = as.integer(factorA)
a = length(unique(factorA))
b = length(unique(factorB))
Yijbar = matrix(0,nrow = a,ncol = b)
for(i in 1:a)
{
  for(j in 1:b)
  {
    Yijbar[i,j] = mean(Y[factorA==i&factorB==j])
  }
}
u = mean(Y)
Yidotbar = apply(Yijbar,MARGIN = 1,mean)
Ydotjbar  = apply(Yijbar,MARGIN = 2,mean)
alpha = Yidotbar-u
beta = Ydotjbar-u
gamma = matrix(0,nrow = a,ncol = b)
for(i in 1:a)
{
  for(j in 1:b)
  {
    gamma[i,j] = Yijbar[i,j]-Yidotbar[i]-Ydotjbar[j]+u
  }
}

Yhat = rep(0,length(Y))
for(i in 1:a)
{
  for(j in 1:b)
  {
    Yhat[factorA==i&factorB==j] = Yijbar[i,j]
  }
}

e = Y-Yhat
plot(Yhat, e, pch = 19, xlab = "Fitted Values", ylab = "errors", main = "Residual Plot")
qqnorm(e)
qqline(e)

n = 52
SSA = n*b*sum(alpha^2)
SSB = n*a*sum(beta^2)
SSAB = n*sum(gamma^2)
Yhat = rep(0,length(Y))
for(i in 1:a)
{
  for(j in 1:b)
  {
    Yhat[factorA==i&factorB==j] = Yijbar[i,j]
  }
}
e = Y-Yhat
SSE = sum((Y-Yhat)^2)
SSTotal = sum((Y-u)^2)
AnovaTable = matrix(0,nrow = 5,ncol = 3)
AnovaTable[1,1] = SSA
AnovaTable[1,2] = a-1
AnovaTable[1,3] = SSA/(a-1)
AnovaTable[2,1] = SSB
AnovaTable[2,2] = b-1
AnovaTable[2,3] = SSB/(b-1)
AnovaTable[3,1] = SSAB
AnovaTable[3,2] = (a-1)*(b-1)
AnovaTable[3,3] = SSAB/((a-1)*(b-1))
AnovaTable[4,1] = SSE
AnovaTable[4,2] = a*b*(n-1)
AnovaTable[4,3] = SSE/(a*b*(n-1))
AnovaTable[5,1] = SSTotal
AnovaTable[5,2] = n*a*b-1
AnovaTable[5,3] = '-'
AnovaTable = as.data.frame(AnovaTable)
rownames(AnovaTable) = c('factor A','factor B','interaction AB','Error','Total')
colnames(AnovaTable) = c('SS','df','MS')
library(pander)
pander(pandoc.table(AnovaTable))

Yijmedian = matrix(0,nrow = a,ncol = b)
for(i in 1:a)
{
  for(j in 1:b)
  {
    Yijmedian[i,j] = median(Y[factorA==i&factorB==j])
  }
}

Yijmedian


# median = c(rep(Yijmedian[1,1],52), rep(Yijmedian[1,2],52), rep(Yijmedian[2,1],52), rep(Yijmedian[2,2],52), rep(Yijmedian[3,1],52), rep(Yijmedian[3,2],52), rep(Yijmedian[4,1],52), rep(Yijmedian[4,2],52), rep(Yijmedian[5,1],52), rep(Yijmedian[5,2],52), rep(Yijmedian[6,1],52), rep(Yijmedian[6,2],52), rep(Yijmedian[7,1],52), rep(Yijmedian[7,2],52))


