# date created 2020.01.30
library(brms)
d<-read.csv("data/data.csv")

# ESTIMATE
In<-d[d$year!=2017,] # leave out 2017
Out<-d[d$year==2017,]# out-of-sample (2017)
y<-Out$deaths
mdl<-brm(deaths ~ factor(month), In,
         autocor = cor_ar(~ month, p=1), seed = 42) 

set.seed(42);Yhat<-predict(mdl, Out, summary = FALSE)  # predict outcome 
em<-sweep(Yhat, 2, y)*-1 #change sign as it subtracts observed from predicted

# RESULTS
# Sep: 620; 95%[400; 850]; 50%[550, 700]
mean(em[,9])
quantile(em[,9], probs = c(.025,.975, .25, .75)) 

# Oct: 280; 95%[40; 540]; 50%[200, 370]
mean(em[,10])
quantile(em[,10], probs = c(.025,.975, .25, .75)) 

# Combined: 910: 95%[430; 1400]; 50%[740; 1070]
mean(em[,9])+mean(em[,10]) 
quantile(em[,9], probs = c(.025,.975, .25, .75)) +
  quantile(em[,10], probs = c(.025,.975, .25, .75)) 

# Peculiar case of January: 300; 95%[80; 540]; 50%[230; 380]
mean(em[,1])
quantile(em[,1], probs = c(.025,.975, .25, .75)) 

y[1]-quantile(Yhat[,1], probs=.975)

# Nov: -20; 95%[-280; 230]; 50%[-110; 60] 
mean(em[,11])
quantile(em[,11], probs = c(.025,.975, .25, .75)) 

# Dec: 40; 95%[-180; 270]; 50%[-40; 120] 
mean(em[,12])
quantile(em[,12], probs = c(.025,.975, .25, .75)) 

# Combined: 920; 95%[-30; 1900]; 50%[600; 1250]
mean(em[,9])+mean(em[,10])+mean(em[,11])+mean(em[,12]) 
quantile(em[,9], probs = c(.025,.975, .25, .75)) +
  quantile(em[,10], probs = c(.025,.975, .25, .75))+
  quantile(em[,11], probs = c(.025,.975, .25, .75))+
  quantile(em[,12], probs = c(.025,.975, .25, .75))

## FIN