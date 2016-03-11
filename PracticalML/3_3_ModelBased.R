install.packages('gbm')
library(ISLR); data(Wage); library(ggplot2); library(caret); library(gbm);
Wage <- subset(Wage,select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
#Fit the model

modFit <- train(wage ~ ., method="gbm",data=training,verbose=FALSE)
print(modFit)
#Plot the results

qplot(predict(modFit,testing),wage,data=testing)


########
install.packages(c('e1071', 'klaR'))
data(iris); library(ggplot2)
names(iris)
table(iris$Species)
#Create training and test sets

inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)
#Build predictions

modlda = train(Species ~ .,data=training,method="lda")
modnb = train(Species ~ ., data=training,method="nb")
plda = predict(modlda,testing); pnb = predict(modnb,testing)
table(plda,pnb)
#Comparison of results

equalPredictions = (plda==pnb)
qplot(Petal.Width,Sepal.Width,colour=equalPredictions,data=testing)


