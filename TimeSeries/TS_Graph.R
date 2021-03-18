######################################################
############# Econometrics - Time Series #############
################ Graphic Time Series #################
######################################################
# Archive "pibcol.xls"

############
# Packages #
############
install.packages("tseries")
library(tseries)

########
# Data #
########
pib <- read.table(file = "clipboard", sep = "\t", header=TRUE)
y <- ts(pib, start = c(1950), frequency = 1)

###################
# Transformations #
###################

# Logarithm
ly <- log(y)

# Difference
dy = diff(y, differences = 1)
dly = diff(ly, differences = 1)

############
# Graphics #
############
par(mfrow=c(2,2))
ts.plot(y, main="Time Series - Y")
ts.plot(ly, main="Time Series - LY", col = "blue")
ts.plot(dy, main="Time Series - DY", col = "darkgreen")
ts.plot(dly, main="Time Series - DLY", col = "red")
