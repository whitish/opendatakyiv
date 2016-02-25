rm(list = ls(all = TRUE))

#################################### sample ####################################
a = c(1,2,3,4,5,6,7)
class(a)

sample(a)
sample(a, 5)



##################################### seed #####################################
set.seed(333)
sample(a)
set.seed(333)
sample(a, 3)



##################################### spam #####################################
library(kernlab)
data(spam)
dim(spam)
spam[1,]
View(spam[1,])
View(spam)



################################################################################
set.seed(333)
smallSpam <- spam[sample(dim(spam)[1], size=10),]
plot(smallSpam$capitalAve)



################################################################################
isSpam <- smallSpam$type=="spam"
spamLabel <- isSpam
spamLabel[isSpam] = 8
spamLabel[!isSpam] = 1
plot(smallSpam$capitalAve, pch=spamLabel)



################################# hypothesis_1 #################################
hypothesis_1 <- function(x){
    prediction <- rep(NA, length(x))
    prediction[x > 2.7] <- "spam"
    prediction[x < 2.4] <- "nonspam"
    prediction[x >= 2.4 & x <= 2.45] <- "spam"
    prediction[x >= 2.45 & x <= 2.70] <- "nonspam"
    
    return(prediction)
}

hypothesis_1(smallSpam$capitalAve)
smallSpam$type

table(hypothesis_1(smallSpam$capitalAve), smallSpam$type)



################################# hypothesis_2 #################################
hypothesis_2 <- function(x){
    prediction <- rep(NA, length(x))
    prediction[x > 2.9] <- "spam"
    prediction[x < 2.9] <- "nonspam"
    
    prediction
}


table(hypothesis_2(smallSpam$capitalAve), smallSpam$type)


table(hypothesis_1(spam$capitalAve), spam$type)
table(hypothesis_2(spam$capitalAve), spam$type)


sum(hypothesis_1(spam$capitalAve)==spam$type)
sum(hypothesis_2(spam$capitalAve)==spam$type, na.rm = T)



################################################################################
################################################################################
################################################################################
################################################################################
