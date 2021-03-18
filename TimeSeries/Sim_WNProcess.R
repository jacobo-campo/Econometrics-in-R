######################################################
############# Econometrics - Time Series #############
########### Simulation White Noise Procces ###########
######################################################

wn <- rnorm(100,0,1)
wn

# Graph
ts.plot(wn,main="Proceso Ruido Blanco", col = "red")

## Visual matrix 1x2
par(mfrow=c(1,2))
acf(wn, main="FACS", ylim=c(-1,1), xlim=c(0,10))
pacf(wn, main="FACP", ylim=c(-1,1), xlim=c(0,10))

# Descriptive Moments
hist(wn, main="Histogram of White Noise", breaks=20, freq=FALSE, col="grey")
summary(wn)
sd <- sd(wn)
