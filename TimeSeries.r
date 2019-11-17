# ---------> complete and run the following code for this assignment <-------
##
R code for STA457 assignment 1
# copyright by Christina (Qi) Deng
# date: Jan 21, 2017
#
## Q1:
# load in data
dollar <- scan ("/Users/christinadeng/Desktop/Winter 2017/STA 457/Assignment 1/dollar.txt")
# get the time series of log(dollar)
dollar <- ts(log(dollar))
# define the first difference
returns <- diff(dollar)
# (a) Plot the correlogram and periodogram of the orginal data (i.e. dollar)
acf(dollar,lag.max=20)
spec.pgram(dollar,demean=T,detrend=F)
# (b) Plot the correlogram and periodogram of the first differences.
acf(returns, lag.max=20)
spec.pgram(returns,demean=T,detrend=F) # max not at 0
# (c) Analysis
# (d) Plot the correlogram and periodogram of the absolute values of the first difference
acf(abs(returns), lag.max=20)
spec.pgram(abs(returns),demean=T,detrend=F) # max at 0
# Q2:
# load in data
CO2 <- scan("/Users/christinadeng/Desktop/Winter 2017/STA 457/Assignment 1/CO2.txt")
CO2trend <- scan("/Users/christinadeng/Desktop/Winter 2017/STA 457/Assignment 1/CO2-trend.txt")
# (a) Plot the periodogram of the time series.
spec.pgram((CO2),demean=T,detrend=F)
# (b)Plot periodogram of the detrended data.
detrended=CO2-CO2trend
spec.pgram((detrended), demean=T,detrend=F)


# ---------> complete and run the following code for this assignment <-------
##
R code for STA457 assignment 2
# copyright by Christina (Qi) Deng
# date: Feb 14, 2017
#
# Q2:
# load in data
flour <- scan ("/Users/christinadeng/Desktop/Winter 2017/STA 457/ Assignment 2/flour.txt")
# get the time series of flour data
flour <- ts(flour)
# (a) get time series plot of the data
par(mfrow=c(1,2))
plot(flour,main="Monthly values of the Kansas City flour price index",xlab="month")
log_flour <- ts(log(flour))
plot(log_flour,main="Monthly values of the Kansas City Log flour price index",xlab="month")
# (b) Plot the correlogram, partial correlogram and periodogram of the data.
acf(flour,lag.max=20)
pacf(flour, lag.max=20)
spec.pgram(flour,demean=T,detrend=F)
# (c) Which of the following models seems to be most appropriate for these data: an autore-gressive model, a moving install.packages("forecast")
library(forecast)
auto.arima(flour)
# get MA(1), AR(1), AND Random Walk Model
MA1 <- arima(flour,c(0,0,1))
AR1 <- arima (flour, c(1,0,0))
RW <- arima(flour,c(0,1,0))
# generate AIC output
MA1$aic
AR1$aic
RW$aic
# do Box.test to check their p-value
Box.test(MA1$residuals)
Box.test(AR1$residuals)
Box.test(RW$residuals)


# ---------> complete and run the following code for this assignment <-------
##
R code for STA457 assignment 3
# copyright by Christina (Qi) Deng
# date: March 14, 2017
#
#1(a)
library(tseries)
dollar <- scan ("/Users/christinadeng/Desktop/Winter 2017/STA 457/Assignment 1/dollar.txt")
dollar <- ts(log(dollar))
adf.test(dollar,k=5)
adf.test(dollar,k=10)
adf.test(dollar,k=15)
#(b)
dollar_d = diff(dollar)
bartlett <- function(x,plot=F) {
x <- as.vector(x)
x <- x - mean(x)
n <- length(x)
m <- floor(n/2)+1
s <- Mod(fft(x)[1:m])^2/n
sums <- sum(s)
cumper <- cumsum(s)/sums
v <- c(0:(m-1))/(m-1)
upper <- v + 2*sqrt(v*(1-v))/sqrt(n/2)
lower <- v - 2*sqrt(v*(1-v))/sqrt(n/2)
a <- max(abs(cumper-v))*sqrt(n/2)
sgn <- 1
j <- 1
incr <- 2*exp(-2*a^2)
pval <- incr
while ( incr > 1e-5 ) {
sgn <- -sgn
j <- j + 1
incr <- 2*exp(-2*a^2*j^2)
pval <- pval + sgn*incr
}
tit <- paste("Bartlett's statistic=",round(a,3),"p-value=",round(pval,4))
if (plot) {
plot(v,cumper,xlab=" ",ylab="Cumulative periodogram",pch=20)
abline(0,1)
lines(v,upper,lty=2)
lines(v,lower,lty=2)
title(sub=tit)
}
bartlett <- a
pvalue <- pval
r <- list(stat=bartlett,p.value=pvalue)
r
}
bartlett(dollar_d)
Box.test(dollar_d, lag=5,type="Ljung")
Box.test(dollar_d, lag=10, type="Ljung")
Box.test(dollar_d, lag=15, type="Ljung")
#2(a)
yield = scan("/Users/christinadeng/Desktop/Winter 2017/STA 457/Assignment 3/yield.txt")
yield_d = diff(ts(yield))
adf.test(yield, k=5)
adf.test(yield, k=10)
adf.test(yield, k=15)
bartlett(yield_d)
Box.test(yield_d, lag=10, type="Ljung")
#(b)
arima1=arima(yield, order=c(1,1,1))
arima2=arima(yield, order=c(2,1,2))
arima3=arima(yield, order=c(3,1,3))
arima1$aic
arima2$aic
arima3$aic
#(d)
tsdiag(arima3)
qqnorm(arima3$residuals)
qqline(arima3$residuals,col="purple", pch=21)
#3(a)
fatalities <- scan("/Users/christinadeng/Desktop/Winter 2017/STA 457/Assignment 3/fatalities.txt")
fatalities <- ts(log(fatalities))
fata_d <- diff(diff(fatalities),12)
par(mfrow=c(1,2))
acf(fata_d)
pacf(fata_d)
f1=arima(fatalities,order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12))
bartlett(f1$residuals)
#(b)
P_f1 <- predict(f1,n.ahead=12)
plot(exp(P_f1$pred),ylim=c(45, 325), ylab="number of traffic fatalities", main="Forcast the number of traffic lines(exp(P_f1$pred + 1.96*P_f1$se),lty=2)
lines(exp(P_f1$pred - 1.96*P_f1$se),lty=2)
#(c)
fatalities2 <- fatalities[1:168]
f2=arima(fatalities2,order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12))
P_f2 <- predict(f2,n.ahead=12)
plot(exp(P_f2$pred),ylim=c(45, 325), ylab="number of traffic fatalities", main="Forcast the number of traffic lines(exp(P_f2$pred + 1.96*P_f2$se),lty=2)
lines(exp(P_f2$pred - 1.96*P_f2$se),lty=2)
points(c(169:180),exp(fatalities[169:180]))

# ---------> complete and run the following code for this assignment <-------
##
R code for STA457 assignment 4
# copyright by Christina (Qi) Deng
# date: March 27, 2017
#
#1(a)
fatal <- scan("/Users/christinadeng/Desktop/Winter 2017/STA 457/Assignment 4/fatalities.txt")
fatal <- ts(fatal,start=c(1960,1),end=c(1974,12),freq=12)
r <- stl(fatal,s.window = "periodic", robust = T, t.window = 41)
plot(r, main="stl with s='periodic' & t=41")
r <- stl(fatal,s.window = 3, robust = T, t.window = 51)
plot(r, main="stl with s=3 & t=51")
r <- stl(fatal,s.window = 5, robust = T, t.window = 61)
plot(r,main="stl with s=5 & t=61")
r <- stl(fatal,s.window = 7, robust = T, t.window = 71)
plot(r,main="stl with s=7 & t=71")
#(b)
remainder1=stl(fatal,s.window = 7,t.window = 71)$time.series[,"remainder"]
acf(remainder1)
pacf(remainder1)
remainder2=stl(fatal,s.window = 3,t.window = 41)$time.series[,"remainder"]
acf(remainder2)
pacf(remainder2)
#2(a)
spec.parzen <- function(x,maxlag,nfreq,plot=T) {
f <- 1
if (is.ts(x)) f <- frequency(x)
x <- as.vector(x[!is.na(x)])
n <- length(x)
x <- x - mean(x)
if (missing(maxlag)) maxlag <- n - 1
if (maxlag >= n) maxlag <- n - 1
if (maxlag <= 1) maxlag <- 2
k <- ceiling(log(n+maxlag)/log(2))
nn <- 2^k
m <- nn/2
if (missing(nfreq)) nfreq <- m
if (nfreq < m) nfreq <- m
if (nfreq > m) {
k <- 1 + ceiling(log(nfreq)/log(2))
nn <- 2^k
nfreq <- nn/2
}
x <- c(x,rep(0,nn-n))
if (missing(maxlag)) maxlag <- n - 1
if (maxlag >= n) maxlag <- n - 1
if (maxlag <= 1) maxlag <- 2
k1 <- floor(maxlag/2)
k2 <- maxlag
k <- c(0:k1)
window <- 1 - 6*(k/maxlag)^2 + 6*(k/maxlag)^3
k <- c((k1+1):k2)
window <- c(window,2*(1-k/maxlag)^3)
if (maxlag < nn) window <- c(window,rep(0,nn-maxlag-1))
spec <- Mod(fft(x))^2/n
ac <- Re(fft(spec,inv=T))/nn
ac <- ac*window
spec <- 2*Re(fft(ac))-ac[1]
spec <- 10*log10(spec[1:(nfreq+1)])
freq <- f*c(0:nfreq)/nn
se <- 4.342945*sqrt(0.539*maxlag/n)
if (plot) {
lims <- c(min(spec - 1.96*se),max(spec + 1.96*se))
plot(freq,spec,type="l",xlab="frequency",ylab="spectrum",ylim=lims)
lines(freq,spec + 1.96*se,lty=2)
lines(freq,spec - 1.96*se,lty=2)
ttl <- paste("Parzen's window with M=",maxlag)
title(sub=ttl)
}
std.err <- se
r <- list(frequency=freq,spec=spec,M=maxlag,std.err=std.err)
r}
speech <- ts(scan("/Users/christinadeng/Desktop/Winter 2017/STA 457/Assignment 4/speech.txt"),frequency=r <- spec.parzen(speech,maxlag=10,plot=T)
r <- spec.parzen(speech,maxlag=30,plot=T)
r <- spec.parzen(speech,maxlag=60,plot=T)
r <- spec.parzen(speech,maxlag=90,plot=T)
r <- spec.parzen(speech,maxlag=100,plot=T)
#(b)
r <- spec.ar(speech,order=10,method="burg")
r <- spec.ar(speech,order=20,method="burg")
r <- spec.ar(speech,order=30,method="burg")
r <- spec.ar(speech,method="yw")
#3(a)
library(tseries)
barrick <- scan("/Users/christinadeng/Desktop/Winter 2017/STA 457/Assignment 4/barrick.txt")
adf.test(ts(barrick))
m1<-arima(barrick,c(0,1,1))
m1$aic
m2<-arima(barrick,c(0,1,2))
m2$aic
Box.test(m1$residuals,type="Ljung", lag=10)
Box.test(m2$residuals,type="Ljung", lag=10)
#(b)
library('fGarch')
R=m2$residuals
F1=garchFit(R~garch(1,0),data=R,trace=F)
summary(F1)
F2=garchFit(R~garch(2,0),data=R,trace=F)
summary(F2)
F3=garchFit(R~garch(3,0),data=R,trace=F)
summary(F3)
F4=garchFit(R~garch(4,0),data=R,trace=F)
summary(F4)
F5=garchFit(R~garch(5,0),data=R,trace=F)
summary(F5)
#(c)
f1=garchFit(R~garch(1,1),data=R,trace=F)
summary(f1)
f2=garchFit(R~garch(1,2),data=R,trace=F)
summary(f2)
f3=garchFit(R~garch(1,3),data=R,trace=F)
summary(f3)
f4=garchFit(R~garch(1,4),data=R,trace=F)
summary(f4)
f5=garchFit(R~garch(1,5),data=R,trace=F)
summary(f5)


