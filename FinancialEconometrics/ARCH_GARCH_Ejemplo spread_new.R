######################################################
######### Econometría II - Series de Tiempo ##########
############# Metodología Box - Jenkins ##############
################ Modelos ARCH-GARCH ##################
######################################################
# Datos "quarterly.xls"

############
# Paquetes #
############
install.packages("tseries")
install.packages("forecast")
install.packages("lmtest")
install.packages("rugarch")
install.packages("FinTS")

library(xts)
library(tseries)
library(forecast)
library(lmtest)
library(rugarch)
library(FinTS)

################
# Cargar Datos #
################
quarterly <- read.table(file = "clipboard", sep = "\t", header=TRUE)
quarterly <- ts(quarterly, start = c(1960,1), frequency = 4)

# Variable "Spread"
r5 <- quarterly[,6]
tbill <- quarterly[,4]
spread <- r5-tbill

par(mfrow=c(1,1))
ts.plot(spread,main="Gráfica de la Serie de Tiempo", col = "blue")

class(p)
summary(p)
start(p)    
end(p)
frequency(p) 
hist(spread, main="", breaks=20, freq=FALSE, col="grey")

##################
# Identificación #
##################
par(mfrow=c(1,2))
facs <- acf(spread,main="FACS", lag.max = 10, ylim=c(-1,1))
facs
facp <- pacf(spread,main="FACP", lag.max = 10,ylim=c(-1,1))
facp

######################
# Estimación - ARIMA #
######################

mod1 <- arima(spread, order=c(2,0,1))
mod1

#######################
# Diagnostico - ARIMA #
#######################
tsdiag(mod1)
Box.test(mod1$residuals,lag=1)

# ARCH-LM Test
arima.ArchTest <- ArchTest(mod1$residuals, lags=5, demean=TRUE)
arima.ArchTest

res2_arima <- mod1$residuals^2
par(mfrow=c(1,2))
facs <- acf(res2_arima,main="FACS", lag.max = 36, ylim=c(-1,1))
facs
facp <- pacf(res2_arima,main="FACP", lag.max = 36,ylim=c(-1,1))
facp

##################################
# Estimación - Modelo ARCH-GARCH #
##################################

model.garch <- ugarchspec(mean.model=list(armaOrder=c(2,1), include.mean=TRUE),variance.model=list(garchOrder=c(1,1)),
                          distribution.model = "ged")

garch_fit <- ugarchfit(spec = model.garch, data = spread)
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

# Volatilidad Condicional
par(mfrow=c(1,2))
plot.ts(garch_fit@fit$sigma, col = "darkgreen", main = "Volatilidad Condicional (SD)")

plot(sigma(garch_fit), ylab="sigma(t)", col="blue", main = "Volatilidad Condicional (SD)")

sigma2 <- (sigma(garch_fit))^2
sigma2 <- ts(sigma2, start = c(1960,1), frequency = 4)
par(mfrow=c(1,1))
plot(sigma2, ylab="sigma^2(t)", col="blue", main = "Volatilidad Condicional (Variance)")

plot(garch_fit, which = "all")

##############
# Pronóstico # 
##############

par(mfrow=c(1,1))

# Media
forecast1<-ugarchforecast(garch_fit,n.ahead = 4)
plot(forecast1, which=1)

# Varianza
forecast1<-ugarchforecast(garch_fit,n.ahead = 4)
plot(forecast1, which=3)
