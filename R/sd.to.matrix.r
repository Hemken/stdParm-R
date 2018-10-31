sd.to.matrix <- function(x){
  stopifnot(is.numeric(x), length(x)==1, !is.null(names(x)))
  scaling.matrix <- matrix(c(1,0,0,x), ncol=2)
  colnames(scaling.matrix) <- c("(Intercept)", names(x))
  rownames(scaling.matrix) <- c("(Intercept)", names(x))
  return(scaling.matrix)
}