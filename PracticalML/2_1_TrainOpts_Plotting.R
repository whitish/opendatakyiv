#####################################
# 29.01.2016
# Training options
#####################################
###SPAM Example

library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
modelFit <- train(type ~.,data=training, method="glm")


###Train options

args(train.default)

###trainControl

args(trainControl)

##seed example

set.seed(1235)
modelFit2 <- train(type ~.,data=training, method="glm")
modelFit2
##seed example

set.seed(1235)
modelFit3 <- train(type ~.,data=training, method="glm")
modelFit3


####Plotting predictors####

###Example: Wage data

library(ISLR); library(ggplot2); library(caret); library(gridExtra);
data(Wage)
summary(Wage)
###Get training/test sets

inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)
###Feature plot (caret package)

featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")
###Qplot (ggplot2 package)

qplot(age,wage,data=training)

###Qplot with color (ggplot2 package)

qplot(age,wage,colour=jobclass,data=training)

###Add regression smoothers (ggplot2 package)

qq <- qplot(age,wage,colour=education,data=training)
qq +  geom_smooth(method='lm',formula=y~x)


###cut2, making factors (Hmisc package)

cutWage <- cut2(training$wage,g=3)
table(cutWage)

####Boxplots with cut2

p1 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot"))
p1

###Boxplots with points overlayed

p2 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot","jitter"))
grid.arrange(p1,p2,ncol=2)


###Tables

t1 <- table(cutWage,training$jobclass)
t1
prop.table(t1,1)
###Density plots

qplot(wage,colour=education,data=training,geom="density")
