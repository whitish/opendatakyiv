library(caret)
data(faithful)
faithful[1:10, ]
dim(faithful)

$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Creating data sets$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
set.seed(333)
inTrain<-createDataPartition(y=faithful$waiting, p=0.5, list=FALSE)
trainFaith<-faithful[inTrain, ]
testFaith<-faithful[-inTrain, ]
head(trainFaith)
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="Waiting",ylab="Eruptions")

$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Simple linear model$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

lm1<-lm(eruptions~waiting, data=trainFaith)
summary(lm1)
plot(trainFaith$waiting,trainFaith$eruptions, pch=19, col="blue", xlab="Waiting",ylab="Eruptions")
lines(trainFaith$waiting, lm1$fitted)
lm2<-lm(eruptions~waiting, data=testFaith)
summary(lm2)

$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Predictions and Results$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

Trainresults<-data.frame("Y"=trainFaith$eruptions[1:10], "X"=trainFaith$waiting[1:10], "Y_predicted"=fitted(lm1)[1:10], "Residuals"=residuals(lm1)[1:10])
Trainresults
predict(lm1)[1:10]
trainRes<-residuals(lm1)
testRes<-residuals(lm2)
par(mfrow=c(2,2))
plot(trainFaith$waiting,trainFaith$eruptions, pch=19, col="blue", xlab="Waiting",ylab="Eruptions", main="Train Scaterplot")
lines(trainFaith$waiting, predict(lm1))
plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue", xlab="Waiting",ylab="Eruptions", main="Test Scaterplot")
lines(testFaith$waiting,predict(lm1, newdata=testFaith))
hist(trainRes,freq=FALSE, col="red", xlab="Train Residuals")
lines(density(trainRes), col="blue")
hist(testRes,freq=FALSE,  col="red", xlab="Test Residuals")
lines(density(testRes), col="blue")


$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Prediction intervals$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

pred1<-predict(lm1, newdata=testFaith, interval="prediction")  
pred1
ord<-order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue")
matlines(testFaith$waiting[ord], pred1[ord, ], type="l", col=c(1,2,2), lty=c(1,1,1), lwd=3)


$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Caret package$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
modFit_1<-train(eruptions~waiting, data=trainFaith, method="lm")  
summary(modFit_1)

$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Polinomial regression$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
modFit_2<-lm(eruptions~waiting+I(waiting)^2, data=trainFaith) 
plot(trainFaith$waiting,trainFaith$eruptions, pch=19, col="blue", xlab="Waiting",ylab="Eruptions")
lines(trainFaith$waiting, fitted(modFit_2))
 

$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Multiply linear regression$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
library(ISLR)
library(ggplot2)
library(caret)
library(car)
data(Wage)
Wage<-subset(Wage,select=-c(logwage))
View(Wage)
data(state)
View(state.x77)
state<-as.data.frame(state.x77)
View(state)



$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Creating data set with covariates$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
inTrain<-createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training<-Wage[inTrain, ]
testing<-Wage[-inTrain, ]
dim(training)
dim(testing)

$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Examining bivariate relationships$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

featurePlot(x=training[ ,c("age", "education","jobclass")], y=training$wage, plot="pairs")
qplot(age,wage,colour=education, data=training)
qplot(age, wage, colour=jobclass, data=training)

$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Linear model$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

modFit<-train(wage~age+jobclass+education, method="lm", data=training)
finMod<-modFit$finalModel
summary(finMod)
print(modFit)

$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Results$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
plot(finMod, 1, pch=19, cex=0.5, col="#00000010")  
qplot(finMod$fitted, finMod$residuals, colour=race, data=training)
plot(finMod$residuals, pch=19)
pred<-predict(modFit, testing)
qplot(wage, pred, colour=year, data=testing)
 
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$PCA$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

  
inTrain_q<-createDataPartition(y=state$Murder, p=0.7, list=FALSE)
train<-state[inTrain_q, ]
test<-state[-inTrain_q, ]
scatterplotMatrix(train, spread=FALSE, smoother.arg=list(lty=2), main="Scatterplot Matrix")
round(cor(train),digits = 3)
lm_mult<-train(Murder~., method="lm", data=train)
summary(lm_mult)

eig_train<-eigen(cor(train))
eig_train$values
prcomp<-prcomp(train, scale=T)
summary(prcomp)
var<-eig_train$values/8
var
plot(var, type="b")
prcomp$rotation[ ,1]
PC1<-apply(prcomp$rotation[ ,1]*t(train),2,sum)
PC1


$$$$$$$$$$$$$$$$$$$$$$$$$Caret package$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

train_pca<-train(train$Murder~.,method="lm", preProcess="pca",data=train)
summary(train_pca)  
  
  