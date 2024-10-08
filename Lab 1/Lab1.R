#Data Analytics Lab 1

EPI_data <- read.csv("~/Documents/Data_Analytics/epi2024results06022024.csv")
View(EPI_data)
attach(EPI_data) 
EPI.new
tf <- is.na(EPI.new) # records True values if the value is NA
E <- EPI.new[!tf] # filters out NA values, new array
summary(EPI.new) # stats
fivenum(EPI.new,na.rm=TRUE) 
stem(EPI.new) # stem and leaf plot
hist(EPI.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw=1.))
rug(EPI.new)
boxplot(EPI.new, APO.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw="SJ")) #Smoothens out the curve
rug(EPI.new)
x<-seq(20,80,1)
q<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,q) #NOt good due to being too high
lines(x,.4*q)
q<-dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,.12*q)

#Cumulative Density Function
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE)

# Quantile-Quantile
qqnorm(EPI.new); qqline(EPI.new)

# Make a Q-Q plot against the generating distribution

qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn")
qqline(EPI.new)

qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn")
qqline(EPI.new)

#APO.new Data

APO.new
af <- is.na(APO.new) # records True values if the value is NA
A <- APO.new[!af] # filters out NA values, new array
summary(APO.new) # stats
fivenum(APO.new,na.rm=TRUE) 
stem(APO.new) # stem and leaf plot
hist(APO.new)
hist(APO.new, seq(5., 110., 1.0), prob=TRUE)
lines(density(APO.new,na.rm=TRUE,bw=1.))
rug(APO.new)
boxplot(APO.new, EPI.new)
hist(APO.new, seq(5., 110., 1.0), prob=TRUE)
lines(density(APO.new,na.rm=TRUE,bw="SJ")) #Smoothens out the curve
rug(APO.new)
x<-seq(5,110,1)
q<- dnorm(x,mean=68, sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=90, sd=5,log=FALSE)
lines(x,.12*q)

#Cumulative Density Function
plot(ecdf(APO.new), do.points=FALSE, verticals=TRUE)

# Quantile-Quantile
qqnorm(APO.new); qqline(APO.new)

# Make a Q-Q plot against the generating distribution

qqplot(rnorm(250), APO.new, xlab = "Q-Q plot for norm dsn")
qqline(APO.new)

qqplot(rt(250, df = 5), APO.new, xlab = "Q-Q plot for t dsn")
qqline(APO.new)

#WRS.new Data

WRS.new
wf <- is.na(WRS.new) # records True values if the value is NA
W <- WRS.new[!wf] # filters out NA values, new array
summary(WRS.new) # stats
fivenum(WRS.new,na.rm=TRUE) 
stem(WRS.new) # stem and leaf plot
hist(WRS.new)
hist(WRS.new, seq(5., 100., 1.0), prob=TRUE)
lines(density(WRS.new,na.rm=TRUE,bw=1.))
rug(WRS.new)
boxplot(WRS.new, APO.new)
hist(WRS.new, seq(5., 110., 1.0), prob=TRUE)
lines(density(WRS.new,na.rm=TRUE,bw="SJ")) #Smoothens out the curve
rug(WRS.new)
x<-seq(5,100,1)
q<- dnorm(x,mean=15, sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=40, sd=5,log=FALSE)
lines(x,.12*q)

#Cumulative Density Function
plot(ecdf(WRS.new), do.points=FALSE, verticals=TRUE)

# Quantile-Quantile
qqnorm(WRS.new); qqline(WRS.new)

# Make a Q-Q plot against the generating distribution

qqplot(rnorm(250), WRS.new, xlab = "Q-Q plot for norm dsn")
qqline(WRS.new)

qqplot(rt(250, df = 5), WRS.new, xlab = "Q-Q plot for t dsn")
qqline(WRS.new)

