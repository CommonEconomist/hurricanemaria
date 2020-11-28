# date created 2020.01.30
library(brms)
d<-read.csv("data/data.csv")

# Prior information
mmx<-as.matrix(cbind(d$deaths[d$year ==2010], d$deaths[d$year==2011]))
mmx[2:12,]<-mmx[2:12,]-mean(mmx[1,])  

apply(mmx, 1, mean)
apply(mmx, 1, sd)

# Prior
p0<-c(set_prior("normal(2550, 230)", class ="Intercept"),
      set_prior("normal(-260, 100)",class= "b", coef = "factormonth2"),
      set_prior("normal(60, 200)",class= "b", coef = "factormonth3"),
      set_prior("normal(-190, 110)",class= "b", coef = "factormonth4"),
      set_prior("normal(-90, 20)",class= "b", coef = "factormonth5"),
      set_prior("normal(-120, 30)",class= "b", coef = "factormonth6"),
      set_prior("normal(-90, 10)",class= "b", coef = "factormonth7"),
      set_prior("normal(-50, 90)",class= "b", coef = "factormonth8"),
      set_prior("normal(-230, 60)",class= "b", coef = "factormonth9"),
      set_prior("normal(-70, 10)",class= "b", coef = "factormonth10"),
      set_prior("normal(-120, 120)",class= "b", coef = "factormonth11"),
      set_prior("normal(160, 100)",class= "b", coef = "factormonth12"))

# Fit model
In<-d[d$year> 2010 & d$year < 2017,] 
Out<-d[d$year==2017,]# out-of-sample (2017)
y<-Out$deaths
mdl<-brm(deaths ~ factor(month), 
         data = In,
         prior = p0,
         autocor = cor_ar(~ month, p=1), seed = 42) 

# Excess deaths
set.seed(42);Yhat<-predict(mdl, Out, summary = FALSE)  # predict outcome 
em<-sweep(Yhat, 2, y)*-1 #change sign as it subtracts observed from predicted

# Results
# Sep: 620; 95%[390; 870]; 50%[540, 700]
mean(em[,9])
quantile(em[,9], probs = c(.025,.975, .25, .75)) 

# Oct: 260; 95%[0; 530]; 50%[170, 350]
mean(em[,10])
quantile(em[,10], probs = c(.025,.975, .25, .75)) 

# Combined: 880: 95%[390; 1390]; 50%[700; 1050]
mean(em[,9])+mean(em[,10]) 
quantile(em[,9], probs = c(.025,.975, .25, .75)) +
  quantile(em[,10], probs = c(.025,.975, .25, .75)) 


# Combined: 900; 95%[-90; 1910]; 50%[560; 1240]
mean(em[,9])+mean(em[,10])+mean(em[,11])+mean(em[,12]) 
quantile(em[,9], probs = c(.025,.975, .25, .75)) +
  quantile(em[,10], probs = c(.025,.975, .25, .75))+
  quantile(em[,11], probs = c(.025,.975, .25, .75))+
  quantile(em[,12], probs = c(.025,.975, .25, .75))

## FIN