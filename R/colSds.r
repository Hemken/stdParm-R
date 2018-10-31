colSds <- function(x, na.rm=FALSE){
  stopifnot(is.data.frame(x) || is.matrix(x))
  col.sds <- apply(x,2,sd,na.rm=na.rm)
  names(col.sds) <- colnames(x)
  return(col.sds)
}