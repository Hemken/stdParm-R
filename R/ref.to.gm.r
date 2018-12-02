ref.to.gm <- function(k) {
  C <- matrix(0, ncol=k, nrow=k)
  C[1,1] <- 1
  
  for (i in 1:k) {
    for (j in 2:k) {
      if (i < j) {
        C[i,j] <- 1/k
      } else if (i == j) {
        C[i,j] <- (k-1)/k
      } else {
        C[i,j] <- -1/k
      }
    }
  }
  return(C)
}

