# last update 2020.01.09
library(zoo)

# DATA
d<-read.csv("data/data.csv")
d<-d[order(d$year, d$month),]             
x<-ts(d$deaths, start=c(2010,1), frequency=12)   #raw

# PLOT
par(mar=c(5,5,3,3), mfrow=c(1,2), bty='n', las=1, pty='s', 
    cex.lab=2, cex.axis=1, cex.main=2)

# a. Raw data
plot(x, type='n', cex=2, pch=4, axes=F, xlab="", ylab="", main="a.")
abline(h=mean(x),lty=2,lwd=2)
abline(h=mean(x) + 2*sd(x),lty=3,lwd=2)
lines(x, type='b', pch=NA )
text(x, labels=rep(month.abb[1:12],8), cex=1.1)
axis(1,tick=F, at=2010:2017, labels=2010:2017)
axis(2,tick=F,line=-1)

# b. Per month
plot(d$deaths[d$year!=2017], d$month[d$year!=2017],
     type='p', cex=1.5, pch=5, xlim=c(min(d$deaths), max(d$deaths)), 
     col="grey50", axes=F, xlab="", ylab="", main="b.")
points(d$deaths[d$year==2017], d$month[d$year==2017], pch=19, cex=1.5)
axis(2, tick=F, at=1:12, label=month.abb[1:12]); axis(1, tick=F, line=-1)

## FIN