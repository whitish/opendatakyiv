###########################################
# Trees & Random Forests
###########################################

data(iris)
names(iris)
table(iris$Species)

# explore how petal width correlats with the sepal width
plot(iris$Petal.Width, iris$Sepal.Width, pch=19, col=as.numeric(iris$Species))

# train tree model
library(tree)
tree1 <- tree(Species~Sepal.Width+Petal.Width, data=) #fill the gaps
summary(tree1)

#plot the tree
plot(tree1)
text(tree1)

#plot with partition tree
plot(iris$Petal.Width, iris$Sepal.Width, pch=19, col = as.numeric(iris$Species))
partition.tree(tree1, label='Species', add=T)

#predicting new values
max(iris$Petal.Width) # 2.5
max(iris$Sepal.Width) # 4.4

newdata<-data.frame(Petal.Width=runif(20,0,2.5), Sepal.Width=runif(20,2,4.5))
pred1<-predict(tree1, <fill the gaps>) # return probability
pred1<-predict(tree1, newdata, type = "class") # return classes
plot(newdata$Petal.Width, newdata$Sepal.Width, col=as.numeric(pred1), pch=19)
partition.tree(tree1, 'Species', add=TRUE)


# Explore overfitting
library(MASS)
data(Cars93)
head(Cars93)
dim(Cars93)

# predict the DriveTrain (4x4, Front, Rear)
treeCars<-tree(DriveTrain ~ MPG.city + MPG.highway + AirBags + EngineSize + Width + Length + Weight + Price + Cylinders + Horsepower + Wheelbase, data = <fill the gaps>)
plot(treeCars)
text(treeCars)

# plot errors
par(nfrow=c(1,2))
plot(cv.tree(treeCars, FUN=prune.tree, K = , method="misclass")) #fill the gaps
plot(cv.tree(treeCars, FUN=prune.tree, K = , method="deviance")) #fill the gaps

# prune the tree
pruneTree<-prune.tree(treeCars, best = <fill the gaps>) # try e.g. 4
plot(pruneTree)
text(pruneTree)

# compare accuracy of two models
table(Cars93$DriveTrain, predict(pruneTree, type='class'))
table(Cars93$DriveTrain, predict(treeCars, type='class'))


#########
# Random forests
#########
libratry(ggplot2)
library(caret)
inTrain<-createDataPartition(y=iris$Species, p=.7, list=F)
training<-iris[inTrain,]
testing<-iris[-inTrain,]
modFit<-train(Species~.,data=training, method="rf")
modFit

getTree(modFit$finalModel,k=2)

# predict with rf
pred<-predict(modFit, <fill the gaps>)
table(pred, testing$Species)

# compare tree with rf
treerf<-tree(Species~.,data=training)
table(predict(treerf, testing, type='class'), testing$Species)
