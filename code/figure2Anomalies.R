# date created 2019.08.23
# last update  2020.01.09
library(plyr)
library(zoo)
d<-read.csv("data/data.csv")

# Calculate standardised anomalies (excl. 2017)
av<-ddply(d[d$year<2017,], .(month), summarise,
          deaths.m = mean(deaths), deaths.sd = sd(deaths))
d<-merge(d, av, all.x=T)
d<-d[order(d$year, d$month),]
z<-ts((d$deaths-d$deaths.m)/d$deaths.sd, start=c(2010,1), frequency=12) 

# Plot
par(mar=c(5,5,3,3), bty='n', las=1, cex.lab=2, cex.axis=2)
plot(z, ylim=c(min(z), max(z)+.5), type='n',
     lwd=2, axes=F, xlab="", ylab="",  main= "")    
rect(2017.6, 0, 2017.95,max(z)+.5, col='grey80', border=NA)
points(z, type="h")
axis(1, tick=F, at=2010:2017, label=2010:2017)
rug(index(z)[c(1,13,25,37,49,61,73,85)], ticksize=.03, lwd=1.5, side=1)
axis(2, tick=F, line=-1)
text(index(z)[93:96], z[93:96]+.2, labels= month.abb[9:12], cex=1.1)
text(index(z)[c(58, 85)], z[c(58, 85)]+.2, labels= c("Oct", "Jan"), cex=1.1)

## FIN