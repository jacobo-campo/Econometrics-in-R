######################################################
############ Financial Econometrics - TS #############
############# Box - Jenkins Methodology ##############
################# ARCH-GARCH Models ##################
######################################################
# Data "oil.xls"

############
# Packages #
############
install.packages("tseries")
install.packages("forecast")
install.packages("lmtest")
install.packages("rugarch")
install.packages("FinTS")
install.packages("aTSA")
install.packages("lubridate")
install.packages("AnalyzeTS")

library(xts)
library(tseries)
library(forecast)
library(lmtest)
library(rugarch)
library(FinTS)
library(aTSA)
library(lubridate)
library(AnalyzeTS)

########
# Data #
########
y <- read.table(file = "clipboard", sep = "\t", header=TRUE)
spot <- ts(y[,2])

ts(spot, 
   freq=365.25/7, 
   start=decimal_date(ymd("1987-05-15")))

# Difference
p = 100*diff(log(spot), differences = 1)

par(mfrow=c(1,2))
ts.plot(spot,main="Time Serie Graphic (Spot)", col = "blue")
ts.plot(p,main="Time Serie Graphic (p)", col = "red")

par(mfrow=c(1,1))
hist(p, main="p Histogram", breaks=20, freq=FALSE, col="grey")

##################
# Identification #
##################
par(mfrow=c(1,2))
facs <- acf(p,main="FACS", lag.max = 10, ylim=c(-1,1))
facs
facp <- pacf(p,main="FACP", lag.max = 10,ylim=c(-1,1))
facp

##############
# Estimación #
##############
######################
# Estimation - ARIMA #
######################

mod1 <- arima(p, order=c(0,0,3),
              fixed=c(NA,rep(0,1),NA,
                      NA),
              method="ML")
mod1
summary(mod1)
coeftest(mod1)

######################
# Diagnostic - ARIMA #
######################
tsdiag(mod1)
Box.test(mod1$residuals,lag=1)

#autocorr.res(mod1, rezago.max=36)

# ARCH-LM Test
arima.ArchTest <- ArchTest(mod1$residuals, lags=8, demean=TRUE)
arima.ArchTest

res2_arima <- mod1$residuals^2
par(mfrow=c(1,2))
facs <- acf(res2_arima,main="FACS", lag.max = 36, ylim=c(-1,1))
facs
facp <- pacf(res2_arima,main="FACP", lag.max = 36,ylim=c(-1,1))
facp

#################################
# Estimation - ARCH-GARCH Model #
#################################

model.garch <- ugarchspec(mean.model=list(armaOrder=c(0,1), include.mean=TRUE),variance.model=list(garchOrder=c(1,1)),
                          distribution.model = "ged")

garch_fit <- ugarchfit(spec = model.garch, data = p)
garch_fit
coef(garch_fit)

e_garch_sd <- garch_fit@fit$residuals/garch_fit@fit$sigma
par(mfrow=c(1,2))
facs <- acf(e_garch_sd,main="FACS", lag.max = 36, ylim=c(-1,1))
facs
facp <- pacf(e_garch_sd,main="FACP", lag.max = 36,ylim=c(-1,1))
facp

e_garch_sd2 <- e_garch_sd^2
par(mfrow=c(1,2))
facs <- acf(e_garch_sd2,main="FACS", lag.max = 36, ylim=c(-1,1))
facs
facp <- pacf(e_garch_sd2,main="FACP", lag.max = 36,ylim=c(-1,1))
facp

# Conditional Volatility (SD)
par(mfrow=c(1,2))
plot.ts(garch_fit@fit$sigma, col = "darkgreen", main = "Conditional Volatility (SD)")
plot(sigma(garch_fit), ylab="sigma(t)", col="blue", main = "Conditional Volatility (SD)")

sigma2 <- (sigma(garch_fit))^2
sigma2 <- ts(sigma2, start = c(1), frequency = 1)
par(mfrow=c(1,1))
plot(sigma2, ylab="sigma^2(t)", col="blue", main = "Conditional Volatility (Variance)")

plot(garch_fit, which = "all")


############
# Forecast # 
############

par(mfrow=c(1,1))

# Mean (ARIMA)
forecast1<-ugarchforecast(garch_fit,n.ahead = 4)
plot(forecast1, which=1)

# Variance (Volatility)
forecast1<-ugarchforecast(garch_fit,n.ahead = 4)
plot(forecast1, which=3)
