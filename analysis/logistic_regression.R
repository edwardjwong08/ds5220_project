# DS 5220 Project - Logistic Regression

c1 <- rgb(red=0,green=0.1,blue=0.9,alpha=0.4)
c2 <- rgb(red=0.9,green=0,blue=0.05,alpha=0.4)

headers = read.csv("default of credit card clients.csv", header = F, nrows = 1, as.is = T)
default = read.csv("default of credit card clients.csv", skip = 2, header = F)
headers[1] <- 'ID'
colnames(default)= headers


# Fit a general logistic model to predict Default using all variables
model_fit <- glm(Y ~ ., data=default,family=binomial)
print(summary(model_fit))


# 10-fold cross validation for the model
#randomly shuffle data
default.shuffled <- default[sample(nrow(default)),]

#define number of folds to use for k-fold cross-validation
K <- 10 

#define degree of polynomials to fit
degree <- 10

#create k equal-sized folds
folds <- cut(seq(1,nrow(default.shuffled)),breaks=K,labels=FALSE)


#create object to Store Model Prediction Error
error = c()

#Perform K-fold cross validation - Find Model Prediction Error
for(i in 1:K){
  
  #define training and testing data
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- default.shuffled[testIndexes, ]
  trainData <- default.shuffled[-testIndexes, ]
  

  fit.train = glm(Y ~ ., data=trainData,family=binomial)
  fit.pred = predict(fit.train, newdata=testData,family=binomial)
  fit.test <- rep(0,length(fit.pred))
  fit.test[fit.pred >= 0.5] <- 1 
  e <- sum(fit.test != testData$Y)/length(fit.test)
  error <- c(error,e)
}

#find MSE for each degree 
print('Mean of Cross Validation Error')
print(mean(error))
