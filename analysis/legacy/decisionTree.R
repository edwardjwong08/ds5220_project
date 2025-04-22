df <- read.csv("C:/Users/poibo/Downloads/default of credit card clients.xlsx - Data.csv")
df <- subset(df, select = -c(ID))

depth <- 2


dt <- function(df, depth, tolerance = 5, used_features = c()) {
  #simple case if there is nothing
  if (depth <= 0 || length(setdiff(colnames(df)[-ncol(df)], used_features)) == 0) {
    target <- df[[ncol(df)]]
    prediction <- names(sort(table(target), decreasing = TRUE))[1]
    return(list(prediction = prediction))
  }
  
  best_ce <- 0
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
      
      ce_val <- calculate_cross_entropy(left_split) + calculate_cross_entropy(right_split)
      
      
      if (ce_val > best_ce) {
        best_ce <- ce_val
        best_split <- split_point
        best_feature <- col_name
      }
    }
  }
  
  if (!is.null(best_feature) && best_ce >= tolerance) {
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
  prediction <- names(sort(table(target), decreasing = T))[1]
  return(list(prediction=prediction))
}

dt(df, depth, tolerance = 5)



