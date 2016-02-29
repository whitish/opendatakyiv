#####################################
# 29.01.2016
# Data Slicing
#####################################
data(spam)
# Split to 25% vs 75% test and sample sets
inTrain <- createDataPartition(spam$type, p=, list = FALSE) # fill the gaps
inTrain<-as.vector(inTrain)
inTest<-setdiff(1:nrow(spam), inTrain)

# Create train and test sets
training <- spam[inTrain,]
testing <- spam[inTest,]
dim(training)

# Fit a model
set.seed(32343)
library(caret)
library(e1071)

modelFit<-train(type~., data = , method = "glm") # fill the gaps
modelFit
modelFit$finalModel

# predict
predictions <- predict(modelFit, newdata = ) # fill the gaps
predictions

# Build a confusion matrix
confusionMatrix(predictions, testing$type)

#####################################
# Check different sampling practics
#####################################
set.seed(32323)

#k-Fold sampling
folds<-createFolds(y = , k=10, list = TRUE, returnTrain = TRUE) # fill the gaps
str(folds)
folds[[1]][1:10]

# Bootstrapping (random resampling with replacement)
folds<-createResample(y = , times = 10, list = TRUE) # fill the gaps
str(folds)
folds[[1]][1:10] # elements repeated since sampling with replacement

# Sampling with time series data
set.seed(123)
tme<-1:1000
folds<-createTimeSlices(y = tme, initialWindow = 20, horizon = 10)
names(folds)

# Verify the result
folds$train[[1]]
folds$test[[1]]

folds$train[[2]]
folds$test[[2]]
