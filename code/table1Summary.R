# date created 2019.08.27
# last update  2020.01.09
library(plyr)
d<-read.csv("data/data.csv")

# Calculate standardised anomalies
av<-ddply(d[d$year<2017,], .(month), summarise,
          deaths.m = mean(deaths), deaths.sd = sd(deaths))
d<-merge(d, av, all.x=T)
d<-d[order(d$year, d$month),]
d$diff<-d$deaths-d$deaths.m
d[,c("year", "month", "deaths.m", "deaths.sd", "diff")][d$year==2017,]

## FIN