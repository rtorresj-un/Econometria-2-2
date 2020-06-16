install.packages("tseries")
install.packages("urca")
install.packages("fBasics")
library(tseries)
library(urca)
library(stats)
library(lmtest)
library(urca)
library(fBasics)

IPC
ipc=ts(IPC$...1,start = c(2008,1),frequency = 12)
ipc
acf()
pacf()
Box.test(y, lag = 13, type = c ("Ljung-Box"))