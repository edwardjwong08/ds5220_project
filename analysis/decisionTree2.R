# Load data
#set the directory to where you have the datafile and change it accordingly
df <- read.csv("default of credit card clients.xlsx - Data.csv")
#df <- read.csv("C:/Users/poibo/Downloads/default of credit card clients.xlsx - Data.csv")
df <- subset(df, select = -c(ID))

#cross-entropy function (houseman)
cross_entropy <- function(x, y, split) {
  y_low <- y[which(x <= split)]
  y_high <- y[which(x > split)]
  
  if (length(y_low) == 0 || length(y_high) == 0) {
    return(0)
  }
  
  p_low <- sum(y_low) / length(y_low)
  p_high <- sum(y_high) / length(y_high)
  
  ce_low <- -p_low * log(p_low + 1e-9) - (1 - p_low) * log(1 - p_low + 1e-9)
  ce_high <- -p_high * log(p_high + 1e-9) - (1 - p_high) * log(1 - p_high + 1e-9)
  
  #weighted sum
  ce <- (length(y_low) * ce_low + length(y_high) * ce_high) / (length(y_low) + length(y_high))
  return(ce)
}

#calculate total cross-entropy for a feature split (houseman)
calculate_cross_entropy <- function(split_data) {
  target <- split_data[[ncol(split_data)]]
  p <- mean(target)
  ce <- -p * log(p + 1e-9) - (1 - p) * log(1 - p + 1e-9)
  return(ce)
}

#decision tree (wong)
dt <- function(df, depth, tolerance = 0.01, used_features = c()) {
  if (depth <= 0 || length(setdiff(colnames(df)[-ncol(df)], used_features)) == 0) {
    target <- df[[ncol(df)]]
    prediction <- names(sort(table(target), decreasing = TRUE))[1]
    return(list(prediction = prediction))
  }
  
  best_ce <- Inf
  best_split <- NULL
  best_feature <- NULL
  
  available_features <- setdiff(colnames(df)[-ncol(df)], used_features)
  
  for (col_name in available_features) {
    feature <- df[[col_name]]
    
    min_val <- min(feature, na.rm = TRUE)
    max_val <- max(feature, na.rm = TRUE)
    
    for (split_point in seq(min_val, max_val, length.out = 10)) {
      left_split <- df[feature <= split_point, ]
      right_split <- df[feature > split_point, ]
      
      if (nrow(left_split) == 0 || nrow(right_split) == 0) next
      
      ce_val <- cross_entropy(feature, df[[ncol(df)]], split_point)
      
      if (ce_val < best_ce) {
        best_ce <- ce_val
        best_split <- split_point
        best_feature <- col_name
      }
    }
  }
  
  if (!is.null(best_feature) && best_ce < tolerance) {
    left_split <- df[df[[best_feature]] <= best_split, ]
    right_split <- df[df[[best_feature]] > best_split, ]
    
    new_used <- c(used_features, best_feature)
    
    return(list(
      feature = best_feature,
      split = best_split,
      left = dt(left_split, depth - 1, tolerance, new_used),
      right = dt(right_split, depth - 1, tolerance, new_used)
    ))
  }
  
  target <- df[[ncol(df)]]
  prediction <- names(sort(table(target), decreasing = TRUE))[1]
  return(list(prediction = prediction))
}


#example
depth <- 3
tree <- dt(df, depth = depth, tolerance = 1.0)
print(tree)

set.seed(8)

sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

#predict and evaluate on test set
predict_dt <- function(tree, sample) {
  #pull prediction values if not null
  if (!is.null(tree$prediction)) {
    return(as.numeric(tree$prediction))
  }
  
  #get the feature value counts
  feature_value <- sample[[tree$feature]]
  #return left and right splits accordingly
  if (feature_value <= tree$split) {
    return(predict_dt(tree$left, sample))
  } else {
    return(predict_dt(tree$right, sample))
  }
}

tree_train <- dt(train, depth = 7, tolerance = 1.0)
print(tree_train)

tree_test <- dt(test, depth = 7, tolerance = 1.0)
print(tree_test)

#vectorized predictions
test_predictions <- apply(test, 1, function(row) predict_dt(tree_train, as.list(row)))
actual <- test[[ncol(test)]]

#accuracy
accuracy <- sum(test_predictions == actual) / length(actual)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

#confusion Matrix
conf_matrix <- table(Predicted = test_predictions, Actual = actual)
print("Confusion Matrix:")
print(conf_matrix)

#precision recall and f1 scores
precision <- conf_matrix[4]/(conf_matrix[4] + conf_matrix[3])
recall <- conf_matrix[4]/(conf_matrix[4] + conf_matrix[2])
f1 <- (2*precision*recall)/(precision+recall)

print("Precision:")
print(precision)

print("Recall:")
print(recall)

print("F1:")
print(f1)
