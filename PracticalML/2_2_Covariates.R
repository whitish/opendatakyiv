library(caret); library(kernlab); data(spam)

inTrain <- createDataPartition(
  y=spam$type,
  p=0.75,
  list=FALSE
)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

hist(training$capitalAve, main = "")
mean(training$capitalAve)
sd(training$capitalAve)

max(training$capitalAve)
min(training$capitalAve)

################################################################################
trainCapitalAve <- training$capitalAve
# Standard score / z-score
trainCapitalAveS <- (trainCapitalAve - mean(trainCapitalAve))/sd(trainCapitalAve)

mean(trainCapitalAve)
mean(trainCapitalAveS)

sd(trainCapitalAve)
sd(trainCapitalAveS)

max(trainCapitalAveS)
min(trainCapitalAveS)


################################################################################
testCapitalAve <- testing$capitalAve
# Standard score / z-score
testCapitalAveS <- (testCapitalAve - mean(trainCapitalAve))/sd(trainCapitalAve)

mean(testCapitalAveS)
sd(testCapitalAveS)


########## preProcess ###########################################################

preProc <- preProcess(training[,-58], method = c("center","scale"))
trainCapitalAveS == predict(preProc, training[,-58])$capitalAve

trainCapitalAveS <- predict(preProc, training[,-58])$capitalAve

mean(trainCapitalAveS)
sd(trainCapitalAveS)

testCapitalAveS <- predict(preProc, testing[,-58])$capitalAve
mean(trainCapitalAveS)
sd(trainCapitalAveS)

# browseURL('http://www.inside-r.org/node/86978')
################################################################################
set.seed(32343)

modelFit <- train(
  type~.,
  data=training,
  preProcess=c("center", "scale"),
  method="glm"
)
modelFit

testPred <- predict(modelFit, testing[,-58])
testPred == testing[,58]
confusionMatrix(testPred, testing[,58])


################################################################################
par(mfrow=c(1,2))
hist(trainCapitalAveS)
qqnorm(trainCapitalAveS)

preProc <- preProcess(training[,-58], method = c("BoxCox"))
trainCapitalAveS <- predict(preProc, training[,-58])$capitalAve

par(mfrow=c(1,2))
hist(trainCapitalAveS)
qqnorm(trainCapitalAveS)


################################################################################
set.seed(13343)


training$capitalAveWithNA <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size=1, prob=0.05)==1
training$capitalAveWithNA[selectNA] <- NA


preProc <- preProcess(training[,-58], method = "knnImpute")
capitalAveWithNA <- predict(preProc, training[,-58])$capitalAveWithNA


capitalAveWithoutNA <- training$capitalAve
capitalAveWithoutNA <- (capitalAveWithoutNA - mean(capitalAveWithoutNA))/sd(capitalAveWithoutNA)

quantile(capitalAveWithNA - capitalAveWithoutNA)
quantile((capitalAveWithNA - capitalAveWithoutNA)[selectNA])
quantile((capitalAveWithNA - capitalAveWithoutNA)[!selectNA])


################################################################################
################################################################################
################################################################################
# TAVRIIA V NOVOSELSKOGO ODESA          UA
# McDonald's 019         ODESA          UA
# AUCHAN FONTANKA        ODESA          UA
# WWW.MAFIA.UAKIEV2      KYIV         UAUA
# WWW.E-KVYTOK.UA        KYIV           UA
# WWW.ALIEXPRESS.COM     LONDON         GB
################################################################################
################################################################################
################################################################################

spam$capitalAveSq <- spam$capitalAve^2


################################################################################
library(ISLR); library(caret); data(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]


################################################################################
table(training$jobclass)

dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies, newdata=training))

################################################################################
nsv <- nearZeroVar(training, saveMetrics=T)
nsv

library(splines)
bsBasis <- bs(training$age, df=3)
bsBasis

linFit <- lm(wage ~ bsBasis, data=training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(linFit, newdata=training), col='green', pch=19, cex=0.5)


################################################################################
predict(bsBasis, age=testing$age)

