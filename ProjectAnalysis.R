rm(list=ls())
install.packages("xlsx")
install.packages("MASS")
library(xlsx)
library(MASS)

Data<- read.xlsx("openclose.xlsx",1)
attach(Data)
head(Data)
Data$date <- as.Date(Data$date, "%m/%d/%Y")
day <- weekdays(as.Date(Data$date))
date<-Data$date

j=3## it is gonna change 1 to n to change the stock symbol.
i=2*j

open<-Data[i]
close<-Data[i+1]

re<-log(close/open)

mydata<-data.frame(date,day,re)
attach(mydata)
head(mydata)

bf=c(NULL,NULL,NULL,NULL,NULL,NULL,NULL)
nf=c(NULL,NULL,NULL,NULL,NULL,NULL,NULL)

for (i in 1:nrow(mydata)) {
  if((mydata$day[i]=="Friday") & (as.POSIXlt(mydata$date[i])$mday!=13))
  {
    nf=rbind(nf,mydata[i,])
  }
  
}

for (i in 1:nrow(mydata)) {
  if((mydata$day[i]=="Friday") & (as.POSIXlt(mydata$date[i])$mday==13))
  {
    bf=rbind(bf,mydata[i,])
  }
  
}

names(bf)[3]<-paste("re")
names(nf)[3]<-paste("re")

#statistics summary

BF<-bf$re
meanbf=mean(BF)
sdbf=sd(BF)
n=nrow(bf)

NF<-nf$re
meannf=mean(NF)
sdnf=sd(NF)
m=nrow(nf)


#independence
mat <-cbind(table(BF),table(NF))
chisq.test(mat)


#test the equality of two population variance


var.test(BF, NF) 

#test the equality of two mean (when we do not know the variances are equal)
t.test(BF, NF, alternative="two.sided", var.equal=FALSE)

#test the equality of two mean if variance is equal
t.test(BF, NF, alternative="two.sided", var.equal=TRUE)

#test the alterntaive hyppthesis that difference in means is less than 0 
#(when we do not know the variances are equal) 
t.test(BF, NF, alternative="less", var.equal=FALSE)

#test the alterntaive hyppthesis that difference in means is less than 0 
#variance is equal
t.test(BF, NF, alternative="less", var.equal=TRUE)

#bernoulli test:
#Null: true probability of success (means are different) is equal to p:
binom.test(8, 10, p = 0.5, conf.level = 0.95)

#binom.test(8, 10, p = 0.5,
           #alternative = c("two.sided", "less", "greater"),
           #conf.level = 0.95)


# histograms:
attach(mtcars)
par(mfrow=c(1,2))
X<-bf$re
Y<-nf$re
mean=mean(X)
sd=sd(X)
hist(X,freq=FALSE,main="Histogram of B.F")
curve(dnorm(x, mean, sd), add=TRUE, lwd=2) 
meany=mean(Y)
sdy=sd(Y)
hist(Y,freq=FALSE,main="Histogram of N.F")
curve(dnorm(x, meany, sdy), add=TRUE, lwd=2)




