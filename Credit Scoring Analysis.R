## Credit Scoring Analysis.                                                                                                          09/2014 – 12/2014                                                                     
## Built statistical models to predict which customer has high probablity of default.
## Models included: logistic regression, classification tree, bagging, random forest, gradient boosting.

library(ROCR)
library(tree)
library(MASS)
library(randomForest)
library(gbm)

##read data
data <- read.csv("germancredit.csv")
head(data)
names(data)

attach(data)
data$GoodCredit<-as.factor(ifelse(data$GoodCredit==1,'Good','Bad'))


##partition data
goodData<-subset(data,data$GoodCredit=='Good')
badData<-subset(data,data$GoodCredit=='Bad')

set.seed(12)
d1=sort(sample(nrow(goodData),floor(nrow(goodData)*0.7)))
d2=sort(sample(nrow(badData),floor(nrow(badData)*0.7)))

train1<-goodData[d1,]
train2<-badData[d2,]
train<-rbind(train1,train2)

test1<-goodData[-d1,]
test2<-badData[-d2,]
test<-rbind(test1,test2)

acy<-rep(0,6)
auc<-rep(0,6)

##logistic regression
fit1<-glm(GoodCredit~.,data=train,family=binomial())
step <- stepAIC(fit1, direction="both")
step$anova

##find optimal=fit2
fit2<-glm(GoodCredit ~ checkingstatus1 + duration2 + history3 + purpose4 + 
            amount5 + savings6 + employ7 + installment8 + others10 + 
            age13 + otherplans14 + cards16 + tele19 + foreign20,data=train,family=binomial())


##performance
test$score<-predict(fit2,test,type="response")
test$GoodCredit<-as.factor(ifelse(test$GoodCredit=='Good',1,0))
pred1<-prediction(test$score,test$GoodCredit)
perf1 <- performance(pred1,"tpr","fpr")
auc1<-performance(pred1,"auc")
auc[1] <- 0.7761453

##Test Classification accuracy
PredictScore<-predict(fit2,test,type="response")
predtest<-as.numeric(ifelse(PredictScore<=0.5,'0','1'))
ft1 = table(test$GoodCredit,predtest)
ft1
acy[1]<-sum(diag(ft1))/nrow(test)

##0.7541528

##tree-based method
fit3<-tree(GoodCredit~.,data=train)
windows()
plot(fit3)
text(fit3,cex=0.5,pretty=0)


##Test Classification accuracy
pred<-predict(fit3,test,type="class")
ft2<-table(pred,test$GoodCredit)
ft2
acy[2]<-sum(diag(ft2))/nrow(test)
##0.7275748 


##performance
test$score<-predict(fit3,test)[,2]
pred2<-prediction(test$score,test$GoodCredit)
perf2<- performance(pred2,"tpr","fpr")
auc2<-performance(pred2,"auc")
auc2
auc[2]<-0.6844392


##cross-validation pruning
set.seed(170)
cvfit3<-cv.tree(fit3,FUN=prune.misclass)
windows()
par(mfrow=c(1,2))
plot(cvfit3$size,cvfit3$dev,type="b")
plot(cvfit3$k,cvfit3$dev,type="b")

##opt:size=4
fit4<-prune.misclass(fit3,best=4)
summary(fit4)
windows()
plot(fit4)
text(fit4,cex=0.5,pretty=0)

##performance
test$score<-predict(fit4,test)[,2]
pred3<-prediction(test$score,test$GoodCredit)
perf3<- performance(pred3,"tpr","fpr")
auc3<-performance(pred3,"auc")
auc3
auc[3]<- 0.7135334



####Test Classification accuracy
pred<-predict(fit4,test,type="class")
ft3<-table(pred,test$GoodCredit)
ft3
acy[3]<-sum(diag(ft3))/nrow(test)
##0.7275748


##bagging
set.seed(1)
fit5<-randomForest(GoodCredit~.,data=train,mtry=20,importance=TRUE,keep.forest=TRUE)
fit5


##accuracy performance
pred<-predict(fit5,test,type="class")
ft4<-table(test$GoodCredit,pred)
ft4
acy[4]<-sum(diag(ft4))/nrow(test)
##0.7641196

##performance
test$score<-predict(fit5,test,type="prob")[,2]
pred4<-prediction(test$score,test$GoodCredit)
perf4<- performance(pred4,"tpr","fpr")
auc4<-performance(pred4,"auc")
auc4
auc[4]<- 0.7135334


##random forest
set.seed(122)
fit8<-randomForest(GoodCredit~.,data=train,mtry=8,importance=TRUE,keep.forest=TRUE)
fit8

##accuracy
pred<-predict(fit8,test,type="class")
ft5<-table(test$GoodCredit,pred)
ft5
acy[5]<-sum(diag(ft5))/nrow(test)
##0.7707641

importance(fit8)
varImpPlot(fit8)

##performance
test$score<-predict(fit8,test,type="prob")[,2]
pred5<-prediction(test$score,test$GoodCredit)
perf5<-performance(pred5,"tpr","fpr")
auc5<-performance(pred5,"auc")
auc5
auc[5]<- 0.7964718


##gradient boosting
set.seed(1)
newtrain<-train
newtest<-test
newtrain$GoodCredit<-as.factor(ifelse(newtrain$GoodCredit=='Good',1,0))
newtest$GoodCredit<-as.factor(ifelse(newtest$GoodCredit=='Good',1,0))
newtrain$GoodCredit<-as.character(newtrain$GoodCredit)
newtest$GoodCredit<-as.character(newtest$GoodCredit)

fit9=gbm(GoodCredit~.,data=newtrain,n.trees=5000,interaction.depth=4)
summary(fit9)

##prediction accuracy
prd<-predict.gbm(object=fit9,newdata=newtest,n.trees=5000,type="response")
newpred<-ifelse(prd<=0.5,0,1)
ft6<-table(newtest$GoodCredit,newpred)
ft6
acy[6]<-sum(diag(ft6))/nrow(newtest)
##0.8039867

##performance
pred6<-prediction(prd,test$GoodCredit)
perf6<-performance(pred6,"tpr","fpr")
auc6<-performance(pred6,"auc")
auc6
auc[6]<- 0.8014218

##compare test accuracy and auc
acy
auc


##comparison ROC
windows()
plot(perf1,col="red",lty=1,main="ROC of Tree-Based Method vs. Other Methods")
plot(perf2, col='blue',lty=2,add=TRUE)
legend(0.6,0.6,c('logistic regression','unpruned tree','pruned tree with size=4','bagging','randomForest','gradientBoosting'),
       col=c('red','blue','green','orange','purple','black'),lwd=3)
plot(perf3, col='green',lty=3,add=TRUE)
plot(perf4, col='orange',lty=4,add=TRUE)
plot(perf5, col='purple',lty=5,add=TRUE)
plot(perf6, col='black',lty=5,add=TRUE)
abline(0,1,col='grey',lty=3)


windows()
plot(perf1,col="red",lty=1,main="ROC of Tree-Based Method vs.Logistic Regression ")
plot(perf2, col='blue',lty=2,add=TRUE)
legend(0.6,0.6,c('logistic regression','unpruned tree','pruned tree with size=4'),
       col=c('red','blue','green'),lwd=3)
plot(perf3, col='green',lty=3,add=TRUE)


windows()
plot(perf2,col="red",lty=1,main="ROC of Tree vs. random Forest")
plot(perf3, col='blue',lty=2,add=TRUE)
legend(0.6,0.6,c('unpruned tree','pruned tree with size=4','random Forest'),
       col=c('red','blue','green'),lwd=3)
plot(perf5, col='green',lty=5,add=TRUE)
abline(0,1,col='grey',lty=3)

windows()
plot(perf2,col="red",lty=1,main="ROC of Tree vs. bagging")
plot(perf3, col='blue',lty=2,add=TRUE)
legend(0.6,0.6,c('unpruned tree','pruned tree with size=4','bagging'),
       col=c('red','blue','orange'),lwd=3)
plot(perf4, col='orange',lty=4,add=TRUE)
abline(0,1,col='grey',lty=3)


windows()
plot(perf2,col="red",lty=1,main="ROC of Tree vs. gradient boosting")
plot(perf3, col='blue',lty=2,add=TRUE)
legend(0.6,0.6,c('unpruned tree','pruned tree with size=4','gradient boosting'),
       col=c('red','blue','black'),lwd=3)
plot(perf6, col='black',lty=4,add=TRUE)
abline(0,1,col='grey',lty=3)






