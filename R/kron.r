#' kron
#' 
#' @description Form Kronecker (direct) products with useful row 
#'     and column names
#' 
#' @param A,B matrices that generate a direct product
#' 
#' @return the matrix \code{kronecker(A,B, make.dimnames=TRUE)}, but
#'     with the names formed in reverse order, and \code{(Intercept)}
#'     terms cleaned up.
#'     
#' @seealso 
#'     \code{clean.kron.names}, \code{kronecker}
#'     
#' @examples 
#'     A <- matrix(1:4, ncol=2)
#'     B <- matrix(5:8, ncol=2)
#'     rownames(A) <- colnames(A) <- c("a1", "a2")
#'     rownames(B) <- colnames(B) <- c("b1", "b2")
#'     kronecker(A,B, make.dimnames = TRUE)
#'     kron(A,B)
#'     

kron <- function(A,B) {
  # forms A %x% B for coefficient transformation matrices
  # assumes A and B already have useful row and column names
  #stopifnot(is.matrix(A) & is.matrix(B))
  C <- kronecker(A, B, make.dimnames=TRUE)
  # print(colnames(C))
  colnames(C) <- clean.kron.names(colnames(C))
  rownames(C) <- clean.kron.names(rownames(C))

  return(C)
}

