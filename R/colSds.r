#' colSds
#' 
#' @description Column standard deviations for matrices and data frames
#' 
#' @param x a matrix or data frame
#' @param na.rm logical Should missing values be removed?
#' 
#' @return a named vector of standard devations
#' 
#' @seealso 
#'     \code{colMeans}
#'     
#' @examples 
#'     colSds(mtcars[,c("wt", "disp")])
#'     

colSds <- function(x, na.rm=FALSE){
  stopifnot(is.data.frame(x) || is.matrix(x))
  col.sds <- apply(x,2,sd,na.rm=na.rm)
  names(col.sds) <- colnames(x)
  return(col.sds)
}