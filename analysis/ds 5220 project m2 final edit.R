library(tidyverse)
library(corrplot)
library(glmnet)

#df <- read.csv("C:/Users/poibo/Downloads/default+of+credit+card+clients (1)/default of credit card clients.csv")
df <- read.csv("default of credit card clients.csv")

X <- as.matrix(df[, 2:24])
y <- as.numeric(df$default.payment.next.month) 

c1 <- rgb(red=0, green=0.1, blue=0.9, alpha=0.4)
c2 <- rgb(red=0.9, green=0, blue=0.05, alpha=0.4)

counts <- table(y)

#bar plot for outcome/traget variable
barplot(counts, 
        main = "Frequency of Credit Default Outcomes the Following Month", 
        xlab = "Credit Default the Following Month", 
        ylab = "Frequency", 
        col = c(c1, c2), 
        names.arg = c("No", "Yes"),
        ylim = c(0, 25000))

#correlation matrix for the features
cors <- cor(X)
par(pin = c(18, 9))
corrplot(cors, method = 'color', addCoef.col = "black", tl.cex = 0.7, number.cex = 0.25)

#subset first 1000 rows for visualization
default_plot <- df[1:1000,]

#scatter plots of selected variables
plot(default_plot$LIMIT_BAL, default_plot$BILL_AMT1, ylim = c(0, 25000), xlim = c(0, 5e5),
     col = ifelse(y[1:1000] == 1, c2, c1), pch = 19, cex = 2, 
     main = 'Credit Default for LIMIT_BAL and BILL_AMT1')

plot(default_plot$PAY_AMT1, default_plot$BILL_AMT1, ylim = c(0, 20000), xlim = c(0, 20000),
     col = ifelse(y[1:1000] == 1, c2, c1), pch = 19, cex = 2, 
     main = 'Credit Default for PAY_AMT1 and BILL_AMT1')

plot(default_plot$PAY_AMT6, default_plot$LIMIT_BAL, ylim = c(0, 5e5), xlim = c(0, 20000),
     col = ifelse(y[1:1000] == 1, c2, c1), pch = 19, cex = 2, 
     main = 'Credit Default for PAY_AMT6 and LIMIT_BAL')

#get logistic regression model
model_fit <- glm(y ~ ., data = df[, 2:24], family = binomial)
print(summary(model_fit))

#10-fold cross-validation for logistic regression
set.seed(42)
df_shuffled <- df[sample(nrow(df)), ]

K <- 10

folds <- cut(seq(1, nrow(df_shuffled)), breaks = K, labels = FALSE)

error <- c()

for (i in 1:K) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- df_shuffled[testIndexes, ]
  trainData <- df_shuffled[-testIndexes, ]
  
  fit_train <- glm(default.payment.next.month ~ ., data = trainData, family = binomial)
  fit_pred <- predict(fit_train, newdata = testData, type = "response")
  fit_test <- ifelse(fit_pred >= 0.5, 1, 0)
  
  e <- mean(fit_test != testData$default.payment.next.month)
  error <- c(error, e)
}

print('Mean of Cross Validation Error')
print(mean(error))

#optimize lambda value
cv.lasso <- cv.glmnet(X, y, alpha = 1, family = "binomial")
plot(cv.lasso)

best_lambda <- cv.lasso$lambda.min
print(best_lambda)

#run lasso on optimal penalty value
lasso.model <- glmnet(X, y, alpha = 1, lambda = best_lambda)
print(coef(lasso.model))
