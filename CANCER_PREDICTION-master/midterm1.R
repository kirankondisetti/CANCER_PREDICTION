#question 2
#promising variables 
train = read.csv("C:\\Users\\KIRAN KONDISETTI\\Desktop\\CancerData.csv ")
test = read.csv('C:\\Users\\KIRAN KONDISETTI\\Desktop\\CancerHoldoutData.csv ')
library(ggplot2)
mydata <- train[, -c(8)]
cormat<-signif(cor(mydata),2)
cormat
install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(cormat)
#missing values
library(Amelia)  
sum(is.na(train$PctSomeCol18_24))
missmap(train, main="Train Data - Missings Map",
        col=c("yellow", "black"), legend=FALSE)       
missmap(test, main="Test Data - Missings Map",
        col=c("yellow", "black"), legend=FALSE)       

#treating missing values
#method1 - neglecting the coloumn
LR3 = lm(TARGET_deathRate~incidenceRate+medIncome+povertyPercent+MedianAge+MedianAgeMale+MedianAgeFemale+AvgHouseholdSize+PercentMarried+PctNoHS18_24+PctHS18_24+PctBachDeg18_24+PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctWhite+PctBlack+PctAsian+PctOtherRace+PctMarriedHouseholds
         , data =train)
summary(LR3)
LR3.pred= predict(LR3 ,newdata= test )
msetrain_n=mean((train$TARGET_deathRate-fitted(LR3))^2)
msetrain_n
msetest_n=mean(((test$TARGET_deathRate) - (LR3.pred))^2)
msetest_n

#Method2 - inputing median
train = read.csv("C:\\Users\\KIRAN KONDISETTI\\Desktop\\CancerData.csv ")
test = read.csv('C:\\Users\\KIRAN KONDISETTI\\Desktop\\CancerHoldoutData.csv ')
train$PctSomeCol18_24[is.na(train$PctSomeCol18_24) ]=  median(train$PctSomeCol18_24, na.rm= TRUE)
test$PctSomeCol18_24[is.na(test$PctSomeCol18_24)]=  median(test$PctSomeCol18_24, na.rm= TRUE)
LR2 = lm(TARGET_deathRate~incidenceRate+medIncome+povertyPercent+MedianAge+MedianAgeMale+MedianAgeFemale+AvgHouseholdSize+PercentMarried+PctNoHS18_24+PctHS18_24+PctSomeCol18_24+PctBachDeg18_24+PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctWhite+PctBlack+PctAsian+PctOtherRace+PctMarriedHouseholds
         , data =train)
summary(LR2)
LR2.pred= predict(LR2 ,newdata= test)
LR2.pred
msetrain_median=mean((train$TARGET_deathRate-fitted(LR2))^2)
msetrain_median
msetest_median=mean(((test$TARGET_deathRate) - (LR2.pred))^2)
msetest_median

#method3- Inputing the mean 
train = read.csv("C:\\Users\\KIRAN KONDISETTI\\Desktop\\CancerData.csv ")
test = read.csv('C:\\Users\\KIRAN KONDISETTI\\Desktop\\CancerHoldoutData.csv ')
train$PctSomeCol18_24[is.na(train$PctSomeCol18_24) ]=  mean(train$PctSomeCol18_24, na.rm= TRUE)
test$PctSomeCol18_24[is.na(test$PctSomeCol18_24)]=  mean(test$PctSomeCol18_24, na.rm= TRUE)
LR1  = lm(TARGET_deathRate~incidenceRate+medIncome+povertyPercent+MedianAge+MedianAgeMale+MedianAgeFemale+AvgHouseholdSize+PercentMarried+PctNoHS18_24+PctHS18_24+PctBachDeg18_24+PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctWhite+PctBlack+PctAsian+PctOtherRace+PctMarriedHouseholds
          , data =train)
summary(LR1)
LR1.pred= predict(LR1 ,newdata= test)
msetrain1=mean((train$TARGET_deathRate-fitted(LR1))^2)
msetrain1 
msetest1=mean(((test$TARGET_deathRate) - (LR1.pred))^2)
msetest1  

attach(train)
#removing insignificant variables
fix(train)
LR4  = lm(TARGET_deathRate~incidenceRate+medIncome+PctHS18_24+PctOtherRace+PctBachDeg18_24+PctPrivateCoverage+PctPublicCoverageAlone+povertyPercent, data =train)
LR4.pred= predict(LR4 ,newdata= test)
msetrain_sign=mean((train$TARGET_deathRate - fitted(LR4))^2)
msetrain_sign
msetest_sign=mean(((test$TARGET_deathRate) - (LR4.pred))^2)
msetest_sign

#finding outliers
OutVals = boxplot(train, plot=FALSE)$out
OutVals1 = boxplot(medIncome, plot=FALSE)$out
plot(OutVals1)
plot(OutVals)
boxplot(train)
library(outliers)
outlier(medIncome)

#treating outlies- by using capping
y = c(1,2,3,4,5,6,7,9,10,11,12,14,15,16,17,18,19,20,21,22)
for (i in y)
{
x <- train[,i]
qnt <- quantile(x, probs=c(.25, .75))
caps <- quantile(x, probs=c(.05, .95))
H <- 1.5 * IQR(x)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
train[,i] = x
}
for (i in y)
{
  x <- test[,i]
  qnt <- quantile(x, probs=c(.25, .75))
  caps <- quantile(x, probs=c(.05, .95))
  H <- 1.5 * IQR(x)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  test[,i] = x
}
boxplot(train)
LR5  = lm(TARGET_deathRate~incidenceRate+medIncome+povertyPercent+MedianAge+MedianAgeMale+MedianAgeFemale+AvgHouseholdSize+PercentMarried+PctNoHS18_24+PctHS18_24+PctBachDeg18_24+PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctWhite+PctBlack+PctAsian+PctOtherRace+PctMarriedHouseholds
          
          , data =train)
summary(LR5)
LR5.pred= predict(LR5 ,newdata= test)
msetrain2=mean((train$TARGET_deathRate-fitted(LR5))^2)
msetrain2 
msetest2=mean(((test$TARGET_deathRate) - (LR5.pred))^2)
msetest2   

#finding collinearity

#install.packages('olsrr')
train = read.csv("C:\\Users\\KIRAN KONDISETTI\\Desktop\\CancerData.csv ")
test = read.csv('C:\\Users\\KIRAN KONDISETTI\\Desktop\\CancerHoldoutData.csv ')
train$PctSomeCol18_24[is.na(train$PctSomeCol18_24) ]=  median(train$PctSomeCol18_24, na.rm= TRUE)
test$PctSomeCol18_24[is.na(test$PctSomeCol18_24)]=  median(test$PctSomeCol18_24, na.rm= TRUE)
library(olsrr)
ols_vif_tol(LR3)
#treating collinearity - neglecting the variables

LR6  = lm(TARGET_deathRate~incidenceRate+medIncome+MedianAge+AvgHouseholdSize+PctBlack+PctAsian+PctOtherRace, data =train)
summary(LR6)
LR6.pred= predict(LR6 ,newdata= test)
msetrain3=mean((train$TARGET_deathRate-fitted(LR6))^2)
msetrain3
msetest3=mean(((test$TARGET_deathRate) - (LR6.pred))^2)
msetest3   


#after treating everything
train = read.csv("C:\\Users\\KIRAN KONDISETTI\\Desktop\\CancerData.csv ")
test = read.csv('C:\\Users\\KIRAN KONDISETTI\\Desktop\\CancerHoldoutData.csv ')
y = c(1,2,3,4,5,6,7,9,10,11,12,14,15,16,17,18,19,20,21,22)
for (i in y)
{
  x <- train[,i]
  qnt <- quantile(x, probs=c(.25, .75))
  caps <- quantile(x, probs=c(.05, .95))
  H <- 1.5 * IQR(x)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  train[,i] = x
}
for (i in y)
{
  x <- test[,i]
  qnt <- quantile(x, probs=c(.25, .75))
  caps <- quantile(x, probs=c(.05, .95))
  H <- 1.5 * IQR(x)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  test[,i] = x
}
LR7  = lm(TARGET_deathRate~incidenceRate+medIncome+MedianAge+AvgHouseholdSize+PctBlack+PctAsian+PctOtherRace
          , data =train)
summary(LR7)
LR7.pred= predict(LR7 ,newdata= test)
msetrain4=mean((train$TARGET_deathRate-fitted(LR7))^2)
msetrain4 #optimum msetrain
msetest4=mean(((test$TARGET_deathRate) - (LR7.pred))^2)
msetest4   #optimum msetest

#inputing non-linear terms
attach(train)
LR8  = lm(TARGET_deathRate~incidenceRate+sqrt(medIncome)+povertyPercent+MedianAge+sqrt(MedianAgeMale)+MedianAgeFemale+AvgHouseholdSize+(PercentMarried)^2+PctNoHS18_24^3+PctHS18_24+PctBachDeg18_24+PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctWhite+PctBlack+PctAsian+PctOtherRace+PctMarriedHouseholds
          :medIncome, data =train)
summary(LR8)
LR8.pred= predict(LR8 ,newdata= test)
msetrain5=mean((train$TARGET_deathRate-fitted(LR8))^2)
msetrain5 #optimum msetrain
msetest5=mean(((test$TARGET_deathRate) - (LR8.pred))^2)
msetest5   #optimum msetest

# model diagnosis
par(mfrow=c(2,2))
plot(LR1)
plot(LR5)
plot(LR6)

#trainmse vs testmse
trainMSE= c(459,411,409,409)
testMSE= c(460,414,416,416)
#1= collinearity,2= neglecting,  3= optimum in x, 4= outliers,
x= c(1,2,3,4)
plot(x,trainMSE, ylab='trainMSE and testMSE')
lines(testMSE, col = 'red')
lines(trainMSE, col='blue')

#question 3
library(FNN)
library(class)
set.seed(1)
train = read.csv("C:\\Users\\KIRAN KONDISETTI\\Desktop\\CancerData.csv ")
test = read.csv('C:\\Users\\KIRAN KONDISETTI\\Desktop\\CancerHoldoutData.csv ')
for (i in y)
{
  x <- train[,i]
  qnt <- quantile(x, probs=c(.25, .75))
  caps <- quantile(x, probs=c(.05, .95))
  H <- 1.5 * IQR(x)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  train[,i] = x
}
for (i in y)
{
  x <- test[,i]
  qnt <- quantile(x, probs=c(.25, .75))
  caps <- quantile(x, probs=c(.05, .95))
  H <- 1.5 * IQR(x)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  test[,i] = x
}
n <- nrow(train) * 0.7
T <- sample(nrow(train), size = n)
train1 <- train[T,-c(1,8,13)]
test1 <- train[-T,-c(1,8,13)]
test1_full <- train[-T,]
train.Y = train$TARGET_deathRate
#fix(train1)
knn <- knn.reg(train1, test1, train.Y, k=1)
knntestmse =mean(((test1_full$TARGET_deathRate) - (knn$pred))^2)
error = c(0,0,0,0,0)
for(i in 1:5)
{
  knn <- knn.reg(train1, test1, train.Y, k=i)
  knntestmse =mean(((test1_full$TARGET_deathRate) - (knn$pred))^2) 
  error[i] = knntestmse
}
error


train2 <- train[T,-c(1,4,5,7,8,9,10,12,13,15,17,22)]
test2 <- train[-T,-c(1,4,5,7,8,9,10,12,13,15,17,22)]
test2_full<-train[-T,]
train.Y1 = train$TARGET_deathRate
#fix(train2)
knn3 <- knn.reg(train2, test2, train.Y1, k=1)
knntestmse3 =mean(((test2_full$TARGET_deathRate) - (knn3$pred))^2)
error2 = c(0,0,0,0,0)
for(i in 1:5)
{
  knn3 <- knn.reg(train2, test2, train.Y1,k=i)
  knntestmse3 =mean(((test2_full$TARGET_deathRate) - (knn3$pred))^2) 
  error2[i] = knntestmse3
}
error2


#question-6 
set.seed(1)
train = read.csv("C:\\Users\\KIRAN KONDISETTI\\Desktop\\CancerData.csv ")
test = read.csv('C:\\Users\\KIRAN KONDISETTI\\Desktop\\CancerHoldoutData.csv ')
for (i in y)
{
  x <- train[,i]
  qnt <- quantile(x, probs=c(.25, .75))
  caps <- quantile(x, probs=c(.05, .95))
  H <- 1.5 * IQR(x)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  train[,i] = x
}
for (i in y)
{
  x <- test[,i]
  qnt <- quantile(x, probs=c(.25, .75))
  caps <- quantile(x, probs=c(.05, .95))
  H <- 1.5 * IQR(x)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  test[,i] = x
}
trainn <- train[,-c(1,8,13)]
testn <- test[,-c(1,8,13)]
y = test$TARGET_deathRate
error1 = c(0,0,0,0,0)
for(i in 1:5)
{
  knn1<- knn.reg(trainn, testn, train$TARGET_deathRate, k=i)
  knntestmse1 =mean(((test$TARGET_deathRate) - (knn1$pred))^2) 
  error1[i] = knntestmse1
}
error1
?knn.reg

