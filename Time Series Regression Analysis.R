## Time Series Regression analysis

require(tseries)
require(lmtest)
require(nlme)
require(urca)

soi <- scan("soi.dat")
rec <- scan("recruit.dat")
soi <- ts (soi)
rec <- ts(rec)
par(mfrow=c(2,1))
plot(soi)
plot(rec)

summary(ur.df(soi, type='trend', selectlags="BIC"))
summary(ur.df(rec, type='trend', selectlags="BIC"))

summary(reg <- lm(rec~soi))
dwtest(reg, alternative="two.sided")

par(mfrow=c(1,1))
ccfvalues <- ccf (soi, rec)
ccfvalues
alldata1 <- data.frame(rec, soilag5 = lag(soi,-5), soilag6=lag(soi,-6), soilag7=lag(soi,-7), 
                       soilag8=lag(soi,-8), soilag9=lag(soi,-9), soilag10=lag(soi,-10))
reg1 <- lm(rec~soilag5+soilag6+soilag7+soilag8+soilag9+soilag10, data = alldata1)
summary (reg1)

par(mfrow=c(2,1))
acf(residuals(reg1))
pacf(residuals(reg1))

alldata2 <- cbind(rec,reclag1=lag(rec,-1), reclag2=lag(rec,-2), soilag5 = lag(soi,-5),
                  soilag6=lag(soi,-6), soilag7=lag(soi,-7), soilag8=lag(soi,-8), 
                  soilag9=lag(soi,-9), soilag10=lag(soi,-10))
reg2 <- lm(rec~reclag1+reclag2+soilag5+soilag6+soilag7+soilag8+soilag9+soilag10,
           data = alldata2)
summary (reg2)


reg3 <- lm(rec~reclag1+reclag2+soilag5+soilag6, data = alldata2)
summary (reg3)

par(mfrow=c(2,1))
acf(residuals(reg3))
pacf(residuals(reg3))
Box.test(reg3$residuals,lag=25,type='Ljung')
jarque.bera.test(resid(reg3))

res <- reg3$residuals
par(mfrow=c(2,1))
hist(res)
lines(density(res))
qqnorm(res)          
qqline(res)

