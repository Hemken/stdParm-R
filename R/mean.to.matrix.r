mean.to.matrix <- function(x){
  stopifnot(is.numeric(x), length(x)==1, !is.null(names(x)))
  centering.matrix <- matrix(c(1,0,x,1), ncol=2)
  colnames(centering.matrix) <- c("(Intercept)", names(x))
  rownames(centering.matrix) <- c("(Intercept)", names(x))
  return(centering.matrix)
}
