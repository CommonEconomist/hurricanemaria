# date created 2019.08.20
# last update  2020.01.09
library(brms)
library(data.table)
d<-data.table(read.csv("data/data.csv"))

# Create variables
d[, `:=`(L.deaths=shift(deaths, 1, type="lag"),
         season.hurricane = ifelse(month>=6 & month <=11, 1, 0),
         season.dry = ifelse(month>=12 | month <=3, 1, 0))]

# FIT MODELS
m0<-brm(deaths ~ 1, d[d$year!=2017,], seed = 42) 
m1<-brm(deaths ~ factor(month), d[d$year!=2017,], seed = 42) 
m2<-brm(deaths ~ factor(month), d[d$year!=2017,],
        autocor = cor_ar(~ month, p=1),
        seed = 42) 
m3<-brm(deaths ~ L.deaths + season.hurricane + season.dry,
        d[d$year!=2017,],seed = 42)

# Summary
print(m0, prob=.5)
print(m1, prob=.5)
print(m2, prob=.5)
print(m3, prob=.5)

# Goodness-of-fit: LOO
loo(m0)  # 1085 (15)
loo(m1)  # 1062 (15)
loo(m2)  # 1040 (17)
loo(m3)  # 1055 (17)

# Goodness-of-fit: RMSE
sqrt(mean((m0$data[1]-fitted(m0)[,1])^2)) # 150
sqrt(mean((m1$data[1]-fitted(m1)[,1])^2)) # 114
sqrt(mean((m2$data[1]-fitted(m2)[,1])^2)) # 98
sqrt(mean((m3$data[1]-fitted(m3)[,1])^2)) # 130

## FIN