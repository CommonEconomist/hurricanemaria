# date created 2019.08.23
# last update  2020.01.29
library(brms)
library(data.table)
library(vioplot)

d<-read.csv("data/data.csv")

# FIT MODEL
m<-brm(deaths ~ factor(month), d[d$year!=2017,],
       autocor = cor_ar(~ month, p=1), seed = 42)

post<-posterior_samples(m)
y<-m$data[,1]
yhat<-fitted(m)[,1]

# PLOT
par(mar=c(5,5,2,2), pty='s', bty='n', las=1, mfrow=c(1,2),
    cex.lab=2, cex.axis=1.5, cex.main=2)

# a. Observed vs. fitted
plot(yhat, y, xlim=c(min(y, yhat), max(y, yhat)), cex=2, pch=4,
     ylim=c(min(y, yhat), max(y, yhat)), axes=FALSE,
     main="a.", ylab="Fitted", xlab="Observed")
abline(a=0, b=1, lwd=2)
axis(1, tick=FALSE)
axis(2, tick=FALSE, line=-2)

# b. Posterior density
plot(0:1,0:1, type="n", xlim=c(-515, 225), ylim=c(1,11), 
     axes=FALSE, ann=FALSE)
abline(v=0, lwd=2)
abline(h=1:11, lty=3, col="grey50")
vioplot(post[,2:12], horizontal=T, col="grey60", 
        border=NA, lty=0, drawRect=F, add=T, main="b.", cex.main=2,
        xlab="Deaths relative to January", cex.lab=2)
axis(1, tick=F)
axis(2, tick=F, at = 1:11, label = month.abb[2:12])

## FIN