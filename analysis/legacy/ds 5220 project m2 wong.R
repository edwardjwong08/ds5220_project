library(tidyverse)
library(corrplot)
library(glmnet)

df <- read.csv("C:/Users/poibo/Downloads/default+of+credit+card+clients (1)/default of credit card clients.csv")

#df <- within(df, rm(ID))

cors = cor(df[1:23])
par(pin = c(18, 9))
corrplot(cors, method = 'color', addCoef.col = "black", tl.cex = 0.7, number.cex = 0.25)

X <- as.matrix(df[, 2:24])
y <- as.numeric(df$default.payment.next.month) 

cv.lasso <- cv.glmnet(X, y, alpha=1, family="binomial")
plot(cv.lasso)

best.lambda <- cv.lasso$lambda.min
print(best.lambda)

lasso.model <- glmnet(X, y, alpha=1, lambda=best.lambda)

coef(lasso.model)
