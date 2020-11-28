# last update 2019.10.08
library(brms)
library(data.table)
d<-data.table(read.csv("data/data.csv"))

# Create variables

d[, `:=`(L.deaths=shift(deaths, 1, type="lag"),
         season.hurricane = ifelse(month>=6 & month <=11, 1, 0),
         season.dry = ifelse(month>=12 | month <=3, 1, 0))]

# Fit models
m1<-brm(deaths ~ factor(month), d[d$year!=2017,], seed = 42) 
m2<-brm(deaths ~ factor(month), d[d$year!=2017,],
        autocor = cor_ar(~ month, p=1),
        seed = 42) 
m3<-brm(deaths ~ L.deaths + season.hurricane + season.dry,
        d[d$year!=2017,],seed = 42)

# Predict outcome
mort<-d$deaths[d$year==2017]

p1<- -sweep(predict(m1, d[d$year==2017,], summary=FALSE), 2, mort)
p2<- -sweep(predict(m2, d[d$year==2017,], summary=FALSE), 2, mort)
p3<- -sweep(predict(m3, d[d$year==2017,], summary=FALSE), 2, mort)

# Plot results
par(mfrow=c(1,2), pty='s', las=1, bty='n')

# Sep
plot(density(p1[,9], from = min(p1[,9]), to = max(p1[,9])),
     xlim=c(30,1030), ylim=c(0, .0035))
lines(density(p2[,9], from = min(p2[,9]), to = max(p2[,9])))
lines(density(p3[,9], from = min(p3[,9]), to = max(p3[,9])))

# Sep-Dec
plot(density(p1[,9:12], from = min(p1[,9:12]), to = max(p1[,9:12])),
     xlim=c(-520,1100), ylim=c(0, .0035))
lines(density(p2[,9:12], from = min(p2[,9:12]), to = max(p2[,9:12])))
lines(density(p3[,9:12], from = min(p3[,9:12]), to = max(p3[,9:12])))

## FIN