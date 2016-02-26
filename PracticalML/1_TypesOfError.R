library(kernlab)
data(spam)
spam1 <- spam[spam$capitalAve < (mean(spam$capitalAve)+3*sd(spam$capitalAve)),]
spam_train <- spam1[sample(nrow(spam1),100),]
spamL <- (spam_train$type=='spam')*1 +1
plot(spam_train$capitalAve, col = spamL)
abline(h = 1.5)
abline(h = 2,col = 'blue')

(summary(spam_train$capitalAve[spam_train$type=='spam'])[4] + summary(spam_train$capitalAve[spam_train$type!='spam'])[4])/2
predictor <- function(x,threshold){
  prediction <- rep(NA, length(x))
  prediction[x<threshold] <- 'notspam'
  prediction[x>=threshold] <- 'spam'
  return (prediction)
}
tt <- table(predictor(spam_train$capitalAve,2),spam_train$type)
(tt[1,1] + tt[2,2])/sum(tt)
tt


