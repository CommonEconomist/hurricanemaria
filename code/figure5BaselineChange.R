# date created 2019.08.19
# last update  2020.01.09
library(brms)
library(beepr)

d<-read.csv("data/data.csv")
x<-d[,c('month','deaths','year')]  # subset data

# FIT MODEL
year<-2010:2015                    # year vector
N=length(year)                     # number of years
Jan<-Feb<-Mar<-Apr<-May<-Jun<-Jul<-Aug<-Sep<-Oct<-Nov<-Dec<-list()

for(i in 1:N){
  print(i)
  In<-x[x$year>=year[i] & x$year!=2017,] # subset from year i (and 2017)

  mdl<-brm(deaths~factor(month), In, 
           autocor = cor_ar(~ month, p=1), seed = 42)
  set.seed(42);v<-predict(mdl, x[x$year==2017,], summary=FALSE) 
  
  Jan[[i]]<-x$deaths[x$year==2017 & x$month==1]-v[,1]
  Feb[[i]]<-x$deaths[x$year==2017 & x$month==2]-v[,2]
  Mar[[i]]<-x$deaths[x$year==2017 & x$month==3]-v[,3]
  Apr[[i]]<-x$deaths[x$year==2017 & x$month==4]-v[,4]
  May[[i]]<-x$deaths[x$year==2017 & x$month==5]-v[,5]
  Jun[[i]]<-x$deaths[x$year==2017 & x$month==6]-v[,6]
  Jul[[i]]<-x$deaths[x$year==2017 & x$month==7]-v[,7]
  Aug[[i]]<-x$deaths[x$year==2017 & x$month==8]-v[,8]
  Sep[[i]]<-x$deaths[x$year==2017 & x$month==9]-v[,9]
  Oct[[i]]<-x$deaths[x$year==2017 & x$month==10]-v[,10]
  Nov[[i]]<-x$deaths[x$year==2017 & x$month==11]-v[,11]
  Dec[[i]]<-x$deaths[x$year==2017 & x$month==12]-v[,12]
}
beep(2)

# PLOT
# NB - bit inefficient at the moment.
par(mar=c(5,5,2,2), mfrow=c(3,4), las=1, bty="n", pty="s",
    cex.lab=2, cex.axis=1.5,cex.main=2)

# January
plot(density(Jan[[1]]), axes=F, xlim=c(-800, 1200), ylim=c(0,.005),
     xlab="", ylab="Density", main="January", lwd = 3)
lines(density(Jan[[2]]), lty=2, lwd=1.5)
lines(density(Jan[[3]]), lty=2, lwd=1.5)
lines(density(Jan[[4]]), lty=2, lwd=1.5)
lines(density(Jan[[5]]), lty=2, lwd=1.5)
lines(density(Jan[[6]]), lty=2, lwd=1.5)
abline(v=0)
axis(1, tick=F)

# February
plot(density(Feb[[1]]), axes=F, xlim=c(-800, 1200), ylim=c(0,.005),
     xlab="", ylab="", main="February", lwd=3)
lines(density(Feb[[2]]), lty=2, lwd=1.5)
lines(density(Feb[[3]]), lty=2, lwd=1.5)
lines(density(Feb[[4]]), lty=2, lwd=1.5)
lines(density(Feb[[5]]), lty=2, lwd=1.5)
lines(density(Feb[[6]]), lty=2, lwd=1.5)
abline(v=0)
axis(1, tick=F)

# March
plot(density(Mar[[1]]), axes=F, xlim=c(-800, 1200), ylim=c(0,.005),
     xlab="", ylab="", main="March", lwd=3)
lines(density(Mar[[2]]), lty=2, lwd=1.5)
lines(density(Mar[[3]]), lty=2, lwd=1.5)
lines(density(Mar[[4]]), lty=2, lwd=1.5)
lines(density(Mar[[5]]), lty=2, lwd=1.5)
lines(density(Mar[[6]]), lty=2, lwd=1.5)
abline(v=0)
axis(1, tick=F)

# April
plot(density(Apr[[1]]), axes=F, xlim=c(-800, 1200), ylim=c(0,.005),
     xlab="", ylab="", main="April",lwd=3)
lines(density(Apr[[2]]), lty=2, lwd=1.5)
lines(density(Apr[[3]]), lty=2, lwd=1.5)
lines(density(Apr[[4]]), lty=2, lwd=1.5)
lines(density(Apr[[5]]), lty=2, lwd=1.5)
lines(density(Apr[[6]]), lty=2, lwd=1.5)
abline(v=0)
axis(1, tick=F)

# May
plot(density(May[[1]]), axes=F, xlim=c(-800, 1200), ylim=c(0,.005),
     xlab="", ylab="Density", main="May",lwd=3)
lines(density(May[[2]]), lty=2, lwd=1.5)
lines(density(May[[3]]), lty=2, lwd=1.5)
lines(density(May[[4]]), lty=2, lwd=1.5)
lines(density(May[[5]]), lty=2, lwd=1.5)
lines(density(May[[6]]), lty=2, lwd=1.5)
abline(v=0)
axis(1, tick=F)

# June
plot(density(Jun[[1]]), axes=F, xlim=c(-800, 1200), ylim=c(0,.005),
     xlab="", ylab="", main="June",lwd=3)
lines(density(Jun[[2]]), lty=2, lwd=1.5)
lines(density(Jun[[3]]), lty=2, lwd=1.5)
lines(density(Jun[[4]]), lty=2, lwd=1.5)
lines(density(Jun[[5]]), lty=2, lwd=1.5)
lines(density(Jun[[6]]), lty=2, lwd=1.5)
abline(v=0)
axis(1, tick=F)

# July
plot(density(Jul[[1]]), axes=F, xlim=c(-800, 1200), ylim=c(0,.005),
     xlab="", ylab="", main="July",lwd=3)
lines(density(Jul[[2]]), lty=2, lwd=1.5)
lines(density(Jul[[3]]), lty=2, lwd=1.5)
lines(density(Jul[[4]]), lty=2, lwd=1.5)
lines(density(Jul[[5]]), lty=2, lwd=1.5)
lines(density(Jul[[6]]), lty=2, lwd=1.5)
axis(1, tick=F)
abline(v=0)

# August
plot(density(Aug[[1]]), axes=F, xlim=c(-800, 1200), ylim=c(0,.005),
     xlab="", ylab="", main="Augustus",lwd=3)
lines(density(Aug[[2]]), lty=2, lwd=1.5)
lines(density(Aug[[3]]), lty=2, lwd=1.5)
lines(density(Aug[[4]]), lty=2, lwd=1.5)
lines(density(Aug[[5]]), lty=2, lwd=1.5)
lines(density(Aug[[6]]), lty=2, lwd=1.5)
axis(1, tick=F)
abline(v=0)

# September
plot(density(Sep[[1]]), axes=F, xlim=c(-800, 1200), ylim=c(0,.005),
     xlab="Estimated excess deaths", ylab="Density", main="September",lwd=3)
lines(density(Sep[[2]]), lty=2, lwd=1.5)
lines(density(Sep[[3]]), lty=2, lwd=1.5)
lines(density(Sep[[4]]), lty=2, lwd=1.5)
lines(density(Sep[[5]]), lty=2, lwd=1.5)
lines(density(Sep[[6]]), lty=2, lwd=1.5)
axis(1, tick=F)
abline(v=0)

# October
plot(density(Oct[[1]]), axes=F, xlim=c(-800, 1200), ylim=c(0,.005),
     xlab="Estimated excess deaths", ylab="", main="October",lwd=3)
lines(density(Oct[[2]]), lty=2, lwd=1.5)
lines(density(Oct[[3]]), lty=2, lwd=1.5)
lines(density(Oct[[4]]), lty=2, lwd=1.5)
lines(density(Oct[[5]]), lty=2, lwd=1.5)
lines(density(Oct[[6]]), lty=2, lwd=1.5)
axis(1, tick=F)
abline(v=0)

# November
plot(density(Nov[[1]]), axes=F, xlim=c(-800, 1200), ylim=c(0,.005),
     xlab="Estimated excess deaths", ylab="", main="November",lwd=3)
lines(density(Nov[[2]]), lty=2, lwd=1.5)
lines(density(Nov[[3]]), lty=2, lwd=1.5)
lines(density(Nov[[4]]), lty=2, lwd=1.5)
lines(density(Nov[[5]]), lty=2, lwd=1.5)
lines(density(Nov[[6]]), lty=2, lwd=1.5)
axis(1, tick=F)
abline(v=0)

# December
plot(density(Dec[[1]]), axes=F, xlim=c(-800, 1200), ylim=c(0,.005),
     xlab="Estimated excess deaths", ylab="", main="December",lwd=3)
lines(density(Dec[[2]]), lty=2, lwd=1.5)
lines(density(Dec[[3]]), lty=2, lwd=1.5)
lines(density(Dec[[4]]), lty=2, lwd=1.5)
lines(density(Dec[[5]]), lty=2, lwd=1.5)
lines(density(Dec[[6]]), lty=2, lwd=1.5)
axis(1, tick=F)
abline(v=0)

## FIN