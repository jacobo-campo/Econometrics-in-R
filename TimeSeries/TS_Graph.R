######################################################
######### Econometría II - Series de Tiempo ##########
########## Generando un proceo ruido blanco ##########
######################################################
# Archivo "pibcol.xls"

############
# Paquetes #
############
install.packages("tseries")
library(tseries)

################
# Cargar Datos #
################
pib <- read.table(file = "clipboard", sep = "\t", header=TRUE)
y <- ts(pib, start = c(1950), frequency = 1)

####################
# Transformaciones #
####################

# Logaritmo
ly <- log(y)

# Diferencia
dy = diff(y, differences = 1)
dly = diff(ly, differences = 1)

############
# Gráficas #
############
par(mfrow=c(2,2))
ts.plot(y, main="Serie de Tiempo - Y")
ts.plot(ly, main="Serie de Tiempo - LY", col = "blue")
ts.plot(dy, main="Serie de Tiempo - DY", col = "darkgreen")
ts.plot(dly, main="Serie de Tiempo - DLY", col = "red")
