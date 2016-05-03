## Pfizer stock price forecasting

require(quantmod)
require(forecast)
require(urca)
require(tseries)

##load the data
getSymbols('PFE') # get daily Pfizer stock data from Yahoo Finance
chartSeries(PFE,subset='2010::2014') # plot Pfizer daily closing stock prices with trading volume


##Calculate the log-return
ret_p <- na.omit(diff(log(PFE$PFE.Close)))
r_p <- ret_p["2010::2014"]
n <- length(r_p)
plot(r_p, main = 'The daily logged return of Pfizer stock from 2010 to 2014')

##Check for the trend
t.test(r_p)
summary(ur.df(r_p, type='trend', lags=20, selectlags="BIC")) # ADF Unit Root Test

##Check for the seasonality
par(mfrow=c(3,1)) # plot three plots vertically in one page
acf(r_p, lag=20) # ACF shows an oscillation, indicative of a seasonal series.
pacf(r_p, lag=20)
spec.pgram(r_p) # spikes show seasonality

##Fit the ARIMA model
fit_p <- arima(r_p,order=c(1,0,0))
summary(fit_p)
Box.test(fit_p$residuals,lag=10,type='Ljung') # H0 residuals are indenpendent
tsdiag(fit_p) # obtain 3 plots of model checking

fit_p2 <-  arima(r_p, order=c(0,1,1))
summary(fit_p2)
Box.test(fit_p2$residuals,lag=10,type='Ljung')
tsdiag(fit_p2)

fit_p3 <- arima(r_p, order=c(1,1,1))
summary(fit_p3)
Box.test(fit_p3$residuals,lag=10,type='Ljung')
tsdiag(fit_p3)

##Diagnostic
res <- fit_p$residuals
shapiro.test(res)

par(mfrow=c(2,1))
hist(res)
lines(density(res))
qqnorm(res)          
qqline(res)

##Forecasting of lag 10
l=10  # number of lags for forecasting
h=20  # number of training data shown in the plot

par(mfrow=c(1,1))
fore <- forecast(fit_p,l)
summary(fore)
plot(fore, h, axes=FALSE,ylab="logged return",xlab="date",type="b")
lines(c(n+0:l),ret_p["2014-12-31::"][1+0:l],type="b")  
date=c(index(r_p[n-0:(h-1)]),index(ret_p["2015-01-01::"][1:l])) 

#add x-axis and y-axis
axis(1, at = c(n-h+1:(h+l)), labels = date, cex.axis=0.6)
axis(2, cex.axis=0.6)
box()

##Calculating the Forecasts of closing price
fore.mean <- as.vector(fore$mean) #Change the estimated mean to a vector
#change the last closing price in the training data to a number
lastprice <- as.numeric(PFE$PFE.Close["2014-12-31"]) 
fore.price <- Reduce(function(x,y) {x*exp(y)}, fore.mean, init=lastprice, accumulate=T) 

#95% Upper and Lower bond for closing price
lower <- fore.price[c(1+1:l)]*exp(fore$lower[,2])
upper <- fore.price[c(1+1:l)]*exp(fore$upper[,2])

##Plot the forecasting closing price and the real value
plot(date,PFE$PFE.Close[date],type="b",ylab="Closing price",ylim=c(30,33.5),main="Forecats of the Closing Price by lag of 10")
period <- index(ret_p["2014-12-31::"][1+0:l]) #the forecast period
lines(period,fore.price,type="b",col="red")
lines(period[1+1:l],upper,col="blue")
lines(period[1+1:l],lower,col="blue")
legend("topleft", c("Forecasting price","Closing price","95% CI"), col=c("red","black","blue"), text.col=c("red","black","blue"),lty=c(4,4,1),pch = c(1,1,NA),inset = 0.02)

##Forecasting with lag of 1
r2 <- c(r_p,ret_p["2015-01-01::"][1:l])
fore2.mean=ret_p["2014-12-31::"][1+0:l]
fore2.upper=vector()
fore2.lower=vector()

## Loop to overlay early forecasts
for (j in seq(0, l-1, by=1)) {
  b.fit  <- auto.arima(r2[1:(n+j)]) 
  b.pred <- forecast(b.fit, 1)
  fore2.mean[j+2]=b.pred$mean
  fore2.upper=rbind(fore2.upper,b.pred$upper)
  fore2.lower=rbind(fore2.lower,b.pred$lower)
}

fore2 <- cbind(fore2.mean[1+1:l],fore2.upper,fore2.lower)
colnames(fore2) <- c("Forecasts","H80","H95","L80","L95")
fore2

##Plotting
plot(date,r2[date],type="b",ylab="logged return",ylim=c(-0.04,0.05),main="Forecasts of logged return with lag of 1")
lines(period,fore2.mean,type="b",col="red")
lines(period[1+1:l],fore2.mean[1+1:l]+fore2.upper[,1],col="blue")
lines(period[1+1:l],fore2.mean[1+1:l]+fore2.lower[,1],col="blue")
legend("topleft", c("Forecasting return","Real return","95% CI"), col=c("red","black","blue"),text.col=c("red","black","blue"),lty=c(4,4,1),pch = c(1,1,NA),inset = 0.02)

##Calculating the Forecasts of closing price
fore2.mean2 <- as.vector(fore2.mean[1+1:l])
fore2.price <- Reduce(function(x,y) {x*exp(y)},fore2.mean2, init=lastprice, accumulate=T)
lower2 <- fore2.price[c(1+1:l)]*exp(fore2.lower[,2])
upper2 <- fore2.price[c(1+1:l)]*exp(fore2.upper[,2])

#######################################################

##Plot the forecasting closing price and the real value
par(mfrow=c(1,1))
plot(date,PFE$PFE.Close[date],type="b",ylab="Closing price",ylim=c(30,33.5),
     main="Forecasts of the Closing Price by lag of 1")
period <- index(ret_p["2014-12-31::"][1+0:l]) #the forecast period
lines(period,fore2.price,type="b",col="red")
lines(period[1+1:l],upper,col="blue")
lines(period[1+1:l],lower,col="blue")
legend("topleft", c("Forecasting price","Closing price","95% CI"), col=c("red","black","blue"),
       text.col=c("red","black","blue"),lty=c(4,4,1), pch = c(1,1,NA),inset = .05)

##Calculating the sum square error
sum((fore.mean-as.vector(ret_p[period[1+1:l]]))^2)  #SSE of lag10
sum((fore2.mean2-as.vector(ret_p[period[1+1:l]]))^2)  #SSE of lag1

sum((fore.price[1+1:l]-as.vector(PFE$PFE.Close[period[1+1:l]]))^2)
sum((fore2.price[1+1:l]-as.vector(PFE$PFE.Close[period[1+1:l]]))^2)


