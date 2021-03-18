######################################################
############# Econometrics - Time Series #############
############## Simulation AR(1) Process ##############
######################################################

n<-100
e <- rnorm(n,0,1)
y<-e
for(t in 2:n) y[t]<-1+0.7*y[t-1]+e[t]
AR<-ts(y)
ts.plot(AR, main="AR(1) Process")
summary(AR)
var(AR)
sd(AR)

par(mfrow=c(1,2))
facs <- acf(AR, main="ACF", lag.max = 10, ylim=c(-1,1))
facs
facp <- pacf(AR, main="PACF", lag.max = 10, ylim=c(-1,1))
facp

##############################
# Using function "arima.sim" #
##############################
AR <- arima.sim(list(order=c(1,0,0), ar=0.7), n=100)
ts.plot(AR, main="AR(1) Process")
summary(AR)
var(AR)
sd(AR)

par(mfrow=c(1,2))
facs <- acf(AR, main="ACF", lag.max = 10, ylim=c(-1,1))
facs
facp <- pacf(AR, main="PACF", lag.max = 10, ylim=c(-1,1))
facp

#############################################
# Unique Graph - With Serie and ACF - PACF #
#############################################
layout(matrix(c(1,1,2,3), 2, 2, byrow=TRUE))
AR <- arima.sim(list(order=c(1,0,0), ar=0.7), n=100)
plot(AR, main=(expression(AR(1) Process~~~~~~~~~phi==0.7)))
facs <- acf(AR, main="ACF", lag.max = 10, ylim=c(-1,1))
facs
facp <- pacf(AR, main="PACF", lag.max = 10, ylim=c(-1,1))
facp
