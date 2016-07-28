library(readxl)
library(ggplot2)
library(dplyr)
library(MASS)
library(caret)
library(ROCR)
library(randomForest)
library(e1071)
library(gbm)
library(glmnet)
library(pscl)

data <- read_excel("data_zappos.xlsx")
head(data)
str(data)
dim(data)
summary(data)

data1 <- data %>%      
  group_by(day) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(average_user = mean(count), median_user = median(count), max_user = max(count), min_user = min(count))
head(data1, 10)      
ggplot(data1, aes(day, count)) + geom_line() + xlab("Date") + ylab("Daily Users") + ggtitle("Daily User Counts in 2013")
data$month <- as.Date(cut(data$day, breaks = 'month'))

data2 <- data %>%      
  group_by(month) %>% 
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  mutate(average_user = mean(count), median_user = median(count), max_user = max(count), min_user = min(count))
ggplot(data2, aes(month, count)) + geom_bar(aes(fill = data2$month), stat = "identity") + xlab("Date") + 
  ylab("Monthly Users") + ylim(0,3500) + geom_text(aes(label = data2$count), vjust = -0.2) +
  ggtitle("Monthly User Counts in 2013")
data$week <- as.Date(cut(data$day, breaks = 'week'))

data3 <- data %>%      
  group_by(week) %>% 
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  mutate(average_user = mean(count), median_user = median(count), max_user = max(count), min_user = min(count))
data %>%      
  group_by(week) %>% 
  summarise(count=n()) %>%
  arrange(count) %>%
  slice(1:5)
ggplot(data3, aes(week, count)) + geom_bar(stat = "identity") + xlab("Date") + ylab("weekly Users") + 
  ggtitle("Weekly User Counts in 2013")
data4 <- as.data.frame(table(data$site))
ggplot(data4, aes(Var1, Freq)) + geom_bar(aes(fill = data4$Var1), stat = "identity", width = 0.7) + 
  geom_text(aes(label = data4$Freq), vjust = -0.2) + xlab("Sites") + ggtitle("Sites visisted by Users in 2013")
data5 <- data %>%      
  group_by(site, new_customer) %>% 
  summarise(count=n()) %>%
  arrange(desc(count))
data5[is.na(data5)] <- 2
ggplot(data5, aes(site, count, fill = factor(new_customer))) + geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = data5$count), vjust = 0, position = position_dodge(0.9), size=3.5) +
  ggtitle("Sites visited by different kinds of users in 2013")
data6 <- data[is.na(data)] <- 'Unknown'
data6 <- as.data.frame(table(data$platform))
data6 <- data6[order(data6$Freq),]
ggplot(data6, aes(Var1, Freq)) + geom_bar(aes(fill = data6$Var1), stat = "identity", width = 0.7) + 
  geom_text(aes(label = data6$Freq), vjust = -0.2) + xlab("Platform") + ggtitle("Type of Devices Used by Users in 2013") +
  scale_x_discrete(limits = data6$Var1)
data7 <- data %>% 
  group_by(new_customer) %>%
  summarise(count = n())
data7[is.na(data7)] <- 2  
ggplot(data7, aes(factor(new_customer), count)) + geom_bar(aes(fill = data7$new_customer), stat = "identity", width = 0.5) + 
  geom_text(aes(label = data7$count), vjust = -0.2) + xlab("User Type") + ggtitle("Type of Users in 2013") 
data8 <- data[,c(5,7,9,10)] 
sapply(data, function(x) sum(is.na(x)))
data9 <- apply(data8, 2, sum)
conversion_rate <- data9['orders'] / data9['visits']
bounce_rate <- data9['bounces'] / data9['visits']
add_to_cart_rate <- data9['add_to_cart'] / data9['visits']

## order counts in 2013
data_o <- data %>%      
  group_by(month) %>% 
  summarise(count=sum(orders)) %>%
  arrange(desc(count))
ggplot(data_o, aes(month, count)) + geom_bar(aes(fill = data_o$month), stat = "identity", width = 25) + 
  geom_text(aes(label = data_o$count), vjust = -0.2) + xlab("Date") + ggtitle("Orders count in 2013") 

### data preparation for modeling
### logistic regression
data10 <- data[, -c(1, 8, 13, 14)]   ## remove column day, gross_sales and blank columns
data10$new_customer[is.na(data10$new_customer)] <- 2   ## assign 2 to unknown customers
data10$platform[is.na(data10$platform)] <- 'Unknown'   ## assign Unknown to missing values
sapply(data10, function(x) sum(is.na(x)))  ## make sure there is no missing value
data10$target <- ifelse(data10$orders == 0, 0, 1)  ## when orders > 0, assign 1.
table(data10$target)   ## 0: 12032  1: 9029
data10$new_customer <- factor(data10$new_customer)
data10$target <- factor(data10$target)
data.mod <- data10[, -6]
x_lg <- model.matrix(target~., data.mod) ## remove column orders and prepare x matrix
sapply(data10, class)
set.seed(123)
train_ind <- sample(1:nrow(x_lg), 0.7*nrow(x_lg))
train_lg <- x_lg[train_ind, ]
test_lg <- x_lg[-train_ind, ]
y_train_lg <- data10$target[train_ind]
y_test_lg <- data10$target[-train_ind]
train_lgs <- data.frame(train_lg, y_train_lg)
test_lgs <- data.frame(test_lg, y_test_lg)
lg.mod <- glm(y_train_lg~., data = train_lgs, family = "binomial")
summary(lg.mod)

## model selection using AIC
step <- stepAIC(lg.mod, direction = 'both')
step$anova  ## best model is lg.mod
## model performance
predict_v<-predict(lg.mod, newdata = subset(test_lgs, select = c(1:27)),type="response")
pred_test <-ifelse(predict_v <= 0.5, 0, 1)
confusionMatrix(pred_test, reference = y_test_lg)  ## accuracy: 0.9546

pred1 <- prediction(predict_v, y_test_lg)
prf1 <- performance(pred1, measure = "tpr", x.measure = "fpr")
plot(prf1)
auc1 <- performance(pred1, measure = "auc")@y.values[[1]]  ## 0.9917

## random forest 
set.seed(123)
rf.mod <- randomForest(y_train_lg ~ ., data = train_lgs, mtry = 3, ntree=500, importance = T)
plot(rf.mod)
varImpPlot(rf.mod, sort = T, main = 'variable improtance')
var.imp <- data.frame(importance(rf.mod))
var.imp[order(var.imp$MeanDecreaseGini, decreasing = T), ]
rf.pred <- predict(rf.mod, newdata = subset(test_lgs, select = c(1:27)),type="response")
confusionMatrix(rf.pred, reference = y_test_lg)  ## accuracy: 0.9794
## roc
rf.pred2 <- predict(rf.mod, newdata = subset(test_lgs, select = c(1:27)),type="prob")
rf.pred3 <- prediction(rf.pred2[,2], y_test_lg)
rf.prf <- performance(rf.pred3, measure = "tpr", x.measure = "fpr")
plot(rf.prf, col = 1, main = "ROC Curve")  ## ROC for random forest
auc.rf <- performance(rf.pred3, measure = "auc")@y.values[[1]]  ## auc: 0.9984
plot(prf1, col = 2, add = T)  ## ROC for logistic regression


## SVM
set.seed(123)
svmfit <- tune(svm, y_train_lg~., data = train_lgs, kernel = "radial", ranges = list(cost = c(10^seq(-2, 3, by = 1))),
               gamma = c(0.5, 1, 2, 3, 4, 5))
summary(svmfit)
svm.mod <- svm(y_train_lg~., data = train_lgs, kernel = "radial", cost = 10)
summary(svm.mod)
svm.pred <- predict(svm.mod, newdata = subset(test_lgs, select = c(1:27)), type="response")
confusionMatrix(svm.pred, reference = y_test_lg, positive = '1')  ## accuracy: 0.8014

## svm roc
svm.mod2 <- svm(y_train_lg~., data = train_lgs, kernel = "radial", cost = 10 , decision.values = T)
svm.fitted <- attributes(predict(svm.mod, test_lgs, decision.values = T))$decision.values
y_test_svm <- ifelse(y_test_lg == 0, 1, 0)
svm.pred2 <- prediction(svm.fitted, y_test_svm)
svm.prf <- performance(svm.pred2, measure = "tpr", x.measure = "fpr")
plot(svm.prf, col = 3, add = T)
auc.svm <- performance(svm.pred2, measure = "auc")@y.values[[1]]

## gbm
set.seed(123)
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)
gbm.mod <- train(y_train_lg~., data = train_lgs, method = "gbm", trControl = fitControl, verbose = F)
summary(gbm.mod)
gbm.pred <- predict(gbm.mod, newdata = subset(test_lgs, select = c(1:27)),type="raw")
confusionMatrix(gbm.pred, reference = y_test_lg)  ## 0.9805
## gbm roc
gbm.pred2 <- predict(gbm.mod, newdata = subset(test_lgs, select = c(1:27)),type="prob")
gbm.pred3 <- prediction(gbm.pred2[,2], y_test_lg)
gbm.prf <- performance(gbm.pred3, measure = "tpr", x.measure = "fpr")
auc.gbm <- performance(gbm.pred3, measure = "auc")@y.values[[1]]  ## auc: 0.9976
plot(gbm.prf, col = 4, add = T)  ## ROC for gbm
legend("bottomright", c('RF', 'LR', 'SVM', 'GBM'), col = c(1:4), lwd = 3, pch = 1, cex = 0.4)


## linear regression

## data preparation
data_lr <- data10[, -11]
x <- model.matrix(orders~., data_lr)[, -6]
y <- data_lr$orders
train_lr <- x[train_ind, ]
test_lr <- x[-train_ind, ]
y_train <- y[train_ind]
y_test <- y[-train_ind]
data_lm <- data.frame(cbind(train_lr, y_train))
lm.mod <- lm(y_train~., data = data_lm)
summary(lm.mod)
par(mfrow = c(2,2))
plot(lm.mod)
lm.pred1 <- predict(lm.mod, data.frame(test_lr))
mean((lm.pred1 - y_test)^2) # 1154.561

boxcox(lm((y_train + 1)~., data = data_lm), lambda = seq(-1,1, by = 0.1))
lm.mod2 <- lm((y_train + 1)^(-0.5)~., data = data_lm)
summary(lm.mod2)
lm.pred2 <- predict(lm.mod2, data.frame(test_lr))
mean((((lm.pred2)^(-2) - 1) - y_test)^2)

## ridge
grid <- 10^seq(10, -2, length = 100)
set.seed(123)
ridge.mod <- glmnet(train_lr, y_train, alpha = 0, lambda = grid, thresh = 1e-12)
cv.out <- cv.glmnet(train_lr, y_train, alpha = 0)
best_lambda <- cv.out$lambda.min
ridge.pred <- predict(ridge.mod, s = best_lambda, newx = test_lr)
mean((ridge.pred - y_test)^2) # 3752.768
## refit ridge on the full data
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = best_lambda)

## lasso
set.seed(123)
lasso.mod <- glmnet(train_lr, y_train, alpha = 1, lambda = grid)
cv.lasso <- cv.glmnet(train_lr, y_train, alpha = 1)
best_lambda_lasso <- cv.lasso$lambda.min
lasso.pred <- predict(lasso.mod, s = best_lambda_lasso, newx = test_lr)
mean((lasso.pred - y_test)^2) # 1423.601
out_lasso <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out_lasso, type = "coefficients", s = best_lambda_lasso)

# Possion Model
ggplot(data, aes(orders)) + geom_histogram(binwidth = 0.05) + scale_x_log10() + ggtitle("Histogram of orders in log10 scale")
p.lm <- glm(y_train~., data = data_lm, family = "poisson")
summary(p.lm)
## Test goodness-of-fit of the model with a chi-square test based on the residual deviance and degree of freedom
1 - pchisq(summary(p.lm)$deviance, summary(p.lm)$df.residual) ## p = 0, Possion model doesn't fit the data.
#step_p <- stepAIC(p.lm, direction = 'both')
#step_p$anova

## negative binomial model
nb.lm <- glm.nb(o,data = data11)
#1 - pchisq(summary(nb.lm)$deviance, summary(nb.lm)$df.residual)
summary(nb.lm)

p.lmtry <- glm(orders~add_to_cart, data = data11, family = "poisson")
summary(p.lmtry)
1 - pchisq(summary(p.lmtry)$deviance, summary(p.lmtry)$df.residual)

##zero inflated poisson
zip.lm <- zeroinfl(orders ~ visits  + bounces + add_to_cart + product_page_views + search_page_views| site + 
                     visits + bounces + add_to_cart + product_page_views + search_page_views,
                   data = data11, link = "logit", dist = "poisson")
p.pred <- predict(p.lm, data.frame(test_lr))
mean((exp(p.pred) - y_test)^2) # 5364.339
# mean((exp(p.pred) - y_test)^2 / exp(p.pred))




