### Lab 2 ###
### Exercise 1: Fitting a distribution ###

#Set a working directory
setwd("/Users/dirtydave/Documents/Data_Analytics/Lab 2")

#Read in the csv data
EPI_data <- read.csv("epi2024results06022024.csv", header=TRUE)
epi_weight <- read.csv("epi2024weights.csv")
View(EPI_data)
attach(EPI_data)

#Check for NA values and records true
tf <- is.na(EPI.new)

#Filters out NA values
Epi_filter <- EPI.new[!tf]

#Summary of data
summary(Epi_filter)
fivenum(Epi_filter, na.rm=TRUE)

## Make a Q-Q plot against the generating distribution by:
help("qqnorm")
x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)),EPI.new)
qqline(EPI.new)

## Cumulative Density Function
plot(ecdf(Epi_filter), do.points=FALSE)
plot(ecdf(rnorm(1000,45,10)), do.points=FALSE) 
lines(ecdf(Epi_filter))

## Fitting for Biodiversity & Habitat (BDH)

bf <- is.na(BDH.new)
summary(BDH.new)
fivenum(BDH.new, na.rm = TRUE)
x <- seq(0., 90., 1.0)
qqplot(qnorm(ppoints(200)),x)
qqline(x)
qqplot(qnorm(ppoints(200)),BDH.new)
qqline(BDH.new)

plot(ecdf(BDH.new), do.points=FALSE)
plot(ecdf(rnorm(1000,47,10)), do.points=FALSE)
lines(ecdf(BDH.new))

## Fitting for Forests (ECS)

ef <- is.na(ECS.new)
ECS_filter <- ECS.new[!ef]
summary(ECS_filter)
fivenum(ECS_filter)
x <- seq(10., 90., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)), ECS_filter)
qqline(ECS_filter)

plot(ecdf(ECS_filter), do.points=FALSE)
plot(ecdf(rnorm(1000, 54, 10)), do.points=FALSE)
lines(ecdf(ECS_filter))

## Fitting for Air Pollution (APO.new)

af <- is.na(APO.new)
summary(APO.new)
fivenum(APO.new)
x <- seq(0., 110., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)), APO.new)
qqline(APO.new)

plot(ecdf(APO.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 67, 10)), do.points=FALSE)
lines(ecdf(APO.new))

## Comparing Distributions
boxplot(EPI.old,EPI.new, names=c("EPI.old","EPI.new"))
boxplot(EPI.new, BDH.new, names=c("EPI.new", "BDH.new"))
boxplot(BDH.new, ECS_filter, names=c("BDH.new", "ECS.new"))
boxplot(ECS_filter, APO.new, names=c("ECS.new", "APO.new"))

plot(ecdf(rnorm(1000,45,10)), do.points=FALSE)
lines(ecdf(EPI.new))

plot(ecdf(EPI.old), do.points=FALSE, main="EPI.old vs EPI.new ECDF")
lines(ecdf(EPI.new))

#EPI.new vs BDH.new
plot(ecdf(EPI.new), do.points=FALSE, main="EPI.new vs BDH.new ECDF")
lines(ecdf(BDH.new))

#BDH.new vs ECS.new
plot(ecdf(BDH.new), do.points=FALSE, main="BDH.new vs ECS.new ECDF")
lines(ecdf(ECS_filter))

#ECS.new vs APO.new
plot(ecdf(ECS_filter), do.points=FALSE, main="ECS.new vs APO.new ECDF")
lines(ecdf(APO.new))

### Exercise 2: Linear Models ###

populations_2023 <-read.csv("countries_populations_2023.csv")
View(populations_2023)

#drop countries not in epi data
populations <- populations_2023[-which(!populations_2023$Country %in% EPI_data$country),]

#Sort populations by country
populations <- populations[order(populations$Country),]

#Drop countries not in populations
epi.results.sub <- EPI_data[-which(!EPI_data$country %in% populations$Country),]

#Sort epi.results.sub by country
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]

#Only keep necessary columns
epi.results.sub <- epi.results.sub[,c("country", "EPI.old", "EPI.new", "APO.new", "BDH.old", "BDH.new")]

#Convert populations to numeric
epi.results.sub$population <- as.numeric(populations$Population)

#compute population log base 10
epi.results.sub$population_log <- log10((epi.results.sub$population))

### Linear Model In R ###
#EPI.new = a(population_log) + b

attach(epi.results.sub)
lin.mod.epinew <- lm(EPI.new~population_log,epi.results.sub)
plot(EPI.new~population_log)
abline(lin.mod.epinew)

summary(lin.mod.epinew)
plot(lin.mod.epinew)

ggplot(epi.results.sub, aes(x = population_log, y = EPI.new)) +
  geom_point() + stat_smooth(method = "lm")

ggplot(lin.mod.epinew, aes(x = .fitted, y = .resid)) +
  geom_point() + geom_hline(yintercept = 0) +
  labs(title = 'Residual vs Fitted Values Plot', x = 'Fitted Values', y = "Residuals")

##APO.new linear model
lin.mod.aponew <- lm(APO.new~population_log,epi.results.sub)
plot(APO.new~population_log)
abline(lin.mod.aponew)

summary(lin.mod.aponew)
plot(lin.mod.aponew)

ggplot(epi.results.sub, aes(x = population_log, y = APO.new)) +
  geom_point() + stat_smooth(method = "lm")

ggplot(lin.mod.aponew, aes(x = .fitted, y = .resid)) +
  geom_point() + geom_hline(yintercept = 0) +
  labs(title = 'Residual vs Fitted Values Plot', x = 'Fitted Values', y = "Residuals")

## BDH.old Linear model

lin.mod.bdhold <- lm(BDH.old~population_log,epi.results.sub)
plot(BDH.old~population_log)
abline(lin.mod.bdhold)

summary(lin.mod.bdhold)
plot(lin.mod.bdhold)

ggplot(epi.results.sub, aes(x = population_log, y = BDH.old)) +
  geom_point() + stat_smooth(method = "lm")

ggplot(lin.mod.bdhold, aes(x = .fitted, y = .resid)) +
  geom_point() + geom_hline(yintercept = 0) +
  labs(title = 'Residual vs Fitted Values Plot', x = 'Fitted Values', y = "Residuals")

## BDH.new

lin.mod.bdhnew <- lm(BDH.new~population_log,epi.results.sub)
plot(BDH.new~population_log)
abline(lin.mod.bdhnew)

summary(lin.mod.bdhnew)
plot(lin.mod.bdhnew)

ggplot(epi.results.sub, aes(x = population_log, y = BDH.new)) +
  geom_point() + stat_smooth(method = "lm")

ggplot(lin.mod.bdhnew, aes(x = .fitted, y = .resid)) +
  geom_point() + geom_hline(yintercept = 0) +
  labs(title = 'Residual vs Fitted Values Plot', x = 'Fitted Values', y = "Residuals")
