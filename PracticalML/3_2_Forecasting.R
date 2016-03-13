library(quantmod)
library(PerformanceAnalytics)
library(FinTS)
library(rpart)
library(rpart.plot)
library(rattle) 
library(TTR)
library(forecast)

$$$$$$$$$$$$$$$$$Importing time series from finance.yahoo$$$$$$$$$$$$$$$$$$$$$$$$

getSymbols(Symbols = "CHK", src = "yahoo",from = "2010-03-03", to = "2016-03-03")
CHK<-data.frame(CHK)
View(CHK)
str(CHK)

$$$$$$$$$$$$$$$$$$$$$$$$$Creating the matrix$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

CHK$Date<-rownames(CHK)
colnames(CHK)<-c("Open","High","Low","Close","Volume","Adjusted","Date")
CHK$Date<-as.Date(CHK$Date)
str(CHK)
CHK$Return<-c(NA,diff(CHK$Adjusted)/CHK$Adjusted[-1])
View(CHK)

$$$$$$$$$$$$$$$$$$$$Working with time series$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

data_1<-xts(CHK$Return, order.by = CHK$Date)  # xts is used to create an xts object from raw data inputs.
str(data_1)
data<-ts(CHK$Return[-1], frequency = 10) # frequency - number of observations per unit per time
plot(data)
plot(decompose(data),xlab="Time")

$$$$$$$$$$$$$$$$$$$$$$$Analyzing time series$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
charts.PerformanceSummary(data_1)
table.AnnualizedReturns(data_1)
summary(data)  
tstrain<-window(data,start = 1, end=20)
tstrain
tstest<-window(data, start=20, end=50)

####################Simple moving average###############

plot.ts(tstrain)
lines(ma(tstrain, order=11), col="red")

#####################Exponential smoothing#################################
library(forecast)
etstrain<-ses(tstrain,alpha = 0.6, initial="simple",h=5)
plot(etstrain)
etsfor<-forecast(etstrain)
accuracy(etsfor,tstest)

$$$$$$$$$$$$$$$$$$$$$$Choosing the strategy$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
CHK$ReturnSign<-ifelse(CHK$Return<0,"Down","Up")
View(CHK) 

$$$$$$$$$$$$$$$$$Creating a column to forecast tomorrow return$$$$$$$$$$

CHK$Forecast<-c(CHK$ReturnSign[-1],NA)


$$$$$$$$$$$$$$$$$Choosing dependent variables(day and sign)$$$$$$$$$$$$$

CHK$Day<-factor(weekdays(CHK$Date))
CHK$Day
CHK$DaySign<-factor(ifelse(CHK$Close-CHK$Open>0,1,-1))
CHK$DaySign
View(CHK)
CHK<-CHK[-c(1,nrow(CHK)),] 
str(CHK)

$$$$$$$$$$$$$$$$$Creating data sets$$$$$$$$$$$$$$$$$$$$$$$$$
  
train<-CHK[1:750, ]
test<-CHK[-(1:750),] 

  
$$$$$$$$$$$$$$$$$$Decision tree$$$$$$$$$$$$$$$$$$$$$$$$$$
  
tree<-rpart(data=train, Forecast ~ ReturnSign+Day+DaySign, method = "class")
tree
fancyRpartPlot(tree, sub = "Simple tree")

Prediction<-predict(tree, test, type="class")
Prediction

$$$$$$$$$$$$Caret package$$$$$$$$$$$$$$$$$$$
 
  
library(caret) 

modFit<-train(Forecast~., method="rpart", data=train)
print(modFit$finalModel)
plot(modFit$finalModel, uniform =TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE)




library(ggplot2)
data(iris)
names(iris)
table(iris$Species)
plot(iris$Petal.Width, iris$Sepal.Width, pch=19, col=as.numeric(iris$Species))
inTrain<-createDataPartition(y=iris$Species, p=.7, list=F)
training<-iris[inTrain,]
testing<-iris[-inTrain,]
kc <- kmeans(subset(training, select=-c(Species)), centers=3)
kc
training$clusters<-as.factor(kc$cluster)
qplot(Petal.Width, Petal.Length, colour=clusters, data=training)
table(kc$cluster, training$Species)



