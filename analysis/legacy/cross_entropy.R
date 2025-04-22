# Credit Default Project - Decision Tree

# Define Function to Calculate Cross Entropy Based on Split
c1 <- rgb(red=0,green=0.1,blue=0.9,alpha=0.4)
c2 <- rgb(red=0.9,green=0,blue=0.05,alpha=0.4)
colors <- c(c1,c2)

x1 <- runif(100,0,1)
x2 <- runif(100,0,1)
y <- rbinom(100,1,0.5)

data <- matrix(c(x1,x2),nrow=100,ncol=2)

plot(x1,x2,col=colors[y+1],pch=19,cex=1.5)

cross_entropy <- function(x,y,split) {
  y_low <- y[which(x<=split)]
  y_high <- y[which(x>split)]
  if (length(y_low) == 0) {
    n <- length(y_high) 
    ce <- -log(sum(y_high)/n)
  }
  else if (length(y_high) == 0) {
    n <- length(y_low) 
    ce <- -log(sum(y_low)/n)
  }
  else if ((sum(y_low)/length(y_low)) > (sum(y_high)/length(y_high))) { # The predicted egion for y=1 is below the split
    n <- length(y_low)
    ce <- -log(sum(y_low)/n)
    
  } else {
    n <- length(y_high) 
    ce <- -log(sum(y_high)/n)
  }
  return (ce)
}
  
min_cross_entropy <- function(data,y) {
  best_ce <- 0
  best_feat <- NA
  best_split <- NA
  for (i in 1:ncol(data)) {
    x <- data[,i]
    # Note it doesn't consider the entire range of data because it 
    # may select a break point right at the boundary or after one point
    mesh <- seq(0.2,0.8,by=0.01)
    for (k in 1:length(mesh)) {
      s <- mesh[k]
      ce <- cross_entropy(x,y,s)
      if (ce > best_ce) {
        best_ce <- ce
        best_feat <- i
        best_split <- mesh[k]
      }
    }
  }
  return(c(best_ce,best_feat,best_split))
}


branch <- min_cross_entropy(data,y)
if (branch[2] == 1) {
  abline(v=branch[3],col='gray',lty=2,lw=2)
} else {
  abline(h=branch[3],col='gray',lty=2,lw=2)
}

