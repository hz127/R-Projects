## Survival Analysis of Bone Marrow Transplatation Data
## competing events: death vs relapse
## Model the cumulative incidence fuction
## Model the cause specific hazard 
## FG model

library("timereg") 
library("cmprsk")

follic <- read.csv("follic.txt", header=TRUE)
follic <- as.data.frame(follic)
attach(follic)

evcens <- as.numeric(resp=="NR"| relsite != "")
crcens <- as.numeric(resp == "CR" & relsite == "" & stat == 1)
cause <-ifelse(evcens==1,1,ifelse(crcens==1, 2, 0))

table(causes)

stage <- as.numeric(clinstg == 2)
chemo <- as.numeric(ch == "Y")
times1 <- sort(unique(dftime[cause == 1]))

# The following code is for finding the cumulative incidence curve using the comp.risk function
out1 <- comp.risk(Event(dftime, cause, cens.code = 0)~ +1, data=follic, 
                  cause = 1, n.sim = 5000, model="additive")
pout1 <- predict(out1, X=1)

# The following code is for finding the cumulative incidence curves using the cuminc function
group <- rep(1, nrow(follic))
fit <- cuminc(dftime, cause, group, cencode = 0)

# Now we plot results from both methods
par(mfrow = c(1, 2))
plot(fit,main = "cmprsk", xlab = "Years (a)")
legend(0, .95, c("Cause 1","Cause 2"), lty = 1:2)
plot(pout1,xlim = c(0, 30), xlab = "Years (b)", main = "timereg", uniform = 3, se = 2)


# FG method 
outfg<-comp.risk(Event(dftime,cause)~const(stage)+const(age)+const(chemo)+const(hgb), 
                 data=follic, cause=1,resample.iid=1,model="prop",n.sim=5000)
summary(fg2)

newdata <- data.frame(stage = c(0, 1), age = c(40, 60), chemo = c(0, 1),hgb = c(138, 138))
poutfg <- predict(outfg, newdata)
plot(poutfg, multiple = 1, se = 0, uniform = 0, col = 1:2, lty = 1:2)
title(main = "Fine-Gray Model Predictions")
legend(17, 1.0, c("Stage I", "Stage II"), col = 1:2, lty = 1:2)


