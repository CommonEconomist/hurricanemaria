# last update 2020.01.09
library(brms)
library(data.table)
d<-data.table(read.csv("data/data.csv"))

m2<-brm(deaths ~ factor(month), d[d$year!=2017,][-58],
        autocor = cor_ar(~ month, p=1),
        seed = 42) 
set.seed(42);Yhat<-predict(m2, d[d$year==2017,], summary=FALSE)
y<-d$deaths[d$year == 2017]
em<-sweep(Yhat, 2, y)*-1 #change sign as it subtracts observed from predicted


# Sep-Oct: 970; 95%[510; 1430]
mean(em[,9])+mean(em[,10]) 
quantile(em[,9], probs = c(.025,.975)) +
  quantile(em[,10], probs = c(.025,.975)) 

# Sep: 630; 95%[420; 850]
mean(em[,9])
quantile(em[,9], probs = c(.025,.975)) 

# Oct: 340; 95%[90; 580]
mean(em[,10])
quantile(em[,10], probs = c(.025,.975)) 

## FIN