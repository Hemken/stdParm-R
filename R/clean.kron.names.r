#' clean.kron.names
#' 
#' Clean up rownames and colnames returned by \code{kronecker}
#' 
#' @param xnames a character vector of rownames or colnames
#' 
#' @return a character vector of cleaned up names
#' 
#' @examples 
#'     A <- matrix(1:4, ncol=2)
#'     B <- matrix(5:8, ncol=2)
#'     rownames(A) <- colnames(A) <- c("a1", "a2")
#'     rownames(B) <- colnames(B) <- c("b1", "b2")
#'     C <- kronecker(A,B, make.dimnames = TRUE)
#'     clean.kron.name(colnames(C))
#'     

clean.kron.names <- function (xnames) {
  stopifnot(is.character(xnames))
  cns <- strsplit(xnames, split=":", fixed=TRUE)
  cns <- lapply(cns, function(x) c(x[-1],x[1]))
  cns <- lapply(cns, function (x) paste(x, collapse=":"))
  
  cns[cns=="(Intercept):(Intercept)"] <- "(Intercept)"
  cns <- sub("(Intercept):", "", cns, fixed=TRUE)
  cns <- sub(":(Intercept)", "", cns, fixed=TRUE)
  
  return(cns)
}
