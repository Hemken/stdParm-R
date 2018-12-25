#' ref.to.gm
#' 
#' @description Reference-to-grand-mean conversion matrix
#' 
#' @param k the number of levels in the factor
#'     to be converted
#'     
#' @return a coeficient recentering matrix (the inverse of
#'     the data recentering matrix)
#'     
#' @seealso \code{mean.to.matrix}, \code{sd.to.matrix}
#' 
#' @examples 
#'     ref.to.gm(3)        # coefficient recentering
#'     solve(ref.to.gm(3)) # data recentering/recoding
#'  

ref.to.gm <- function(k) {
  C <- matrix(0, ncol=k, nrow=k)
  C[1,1] <- 1
  for (j in 2:k) {
    C[1,j] <- 1/k
    }
  
  for (i in 2:k) {
    for (j in 2:k) {
      if (i < j) {
        C[i,j] <- -1/k
      } else if (i == j) {
        C[i,j] <- (k-1)/k
      } else {
        C[i,j] <- -1/k
      }
    }
  }
  return(C)
}

ref.to.gm(3)
