# last update 2020.01.09
library(beepr)
library(brms)
d<-read.csv("data/data.csv")

# FIT MODEL
# NB - sensitive to 'fatal errors'
year<-2010:2017                    # year vector
N=length(year)                     # number of years
u<-list()                          # list for results

for(i in 1:N){
  print(i)
  In<-d[d$year!=year[i] & d$year!=2017,] # leave out year i
  Out<-d[d$year==year[i],]               # out-of-sample (year i)
  
  mdl<-brm(deaths ~ factor(month), In,
           autocor = cor_ar(~ month, p=1), seed = 42) 
  
  set.seed(42);v<-predict(mdl, Out)  # predict outcome year i
  u[[i]]<-Out$deaths-v[,1]           # calculate difference
  beep(1)
}
beep(2)

# PLOT
m<-rep(1:12, N)       # months
y<-rep(year, each=12) # years
dff<-unlist(u)        # difference observed vs. predicted

par(mar=c(5,5,2,2), las=1, bty='n', mfrow=c(1,2), 
    cex.lab=2, cex.main=2, cex.axis=1.5)

# a. Difference
plot(dff, m, type='n', cex=1.7, lwd=2, xlim=c(min(dff), max(dff)), axes=F, 
     xlab="Differences with observed deaths", ylab="", main="a.")
abline(v=0, lty=1, lwd=2)
abline(h=1:12, lty=3, col="grey50")
points(dff[y!=2017], m[y!=2017], pch=5, cex=2, col="grey50")
points(dff[y==2017], m[y==2017], pch=19, cex=2)

axis(2, tick=F, at=1:12, label=month.abb[1:12])
axis(1, tick=F)

# b. Predicted values 2017
plot(Out$deaths, Out$month, xlim=c(2000, 3100),
     type='n', axes=F, xlab="Predicted deaths",ylab="", main="b.")
abline(h=1:12, lty=3, col="grey50")
segments(v[,3], Out$month, v[,4], Out$month, lwd=15, lend=1, col="grey60")
points(Out$deaths, Out$month, pch=19, cex=1.5)
axis(1,tick=F)

# PREDICTON ERROR
# Error ratio: September-October
error.post<-dff[y==2017 & m==9 | y==2017 & m==10 ]    # error Sep/Oct 2017
error.pre<-abs(dff[m==9 & y!=2017 |m==10 & y!=2017 ]) # error other <2017
mean(error.post)/mean(error.pre)                      # ratio: 4.5

error.post<-dff[y==2017 & m>=9]          # error post-hurrican months
error.pre<-abs(dff[m>=9 & y!=2017])      # error other months <2017
mean(error.post)/mean(error.pre)         # ratio: 2.3

## FIN