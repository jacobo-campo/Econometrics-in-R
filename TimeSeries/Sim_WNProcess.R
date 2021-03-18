######################################################
######### Econometría II - Series de Tiempo ##########
########## Generando un proceo ruido blanco ##########
######################################################

wn <- rnorm(100,0,1)
wn

ts.plot(wn,main="Proceso Ruido Blanco", col = "red")

## Definiendo una matriz de 1x2
par(mfrow=c(1,2))
acf(wn, main="FACS", ylim=c(-1,1), xlim=c(0,10))
pacf(wn, main="FACP", ylim=c(-1,1), xlim=c(0,10))

summary(wn)
sd <- sd(wn)
