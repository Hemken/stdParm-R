kron <- function(A,B) {
  # forms A %x% B for coefficient transformation matrices
  # assumes A and B already have useful row and column names
  #stopifnot(is.matrix(A) & is.matrix(B))
  C <- kronecker(A, B, make.dimnames=TRUE)
  colnames(C)[colnames(C)=="(Intercept):(Intercept)"] <- "(Intercept)"
  colnames(C) <- sub("(Intercept):", "", colnames(C), fixed=TRUE)
  colnames(C) <- sub(":(Intercept)", "", colnames(C), fixed=TRUE)
  rownames(C)[rownames(C)=="(Intercept):(Intercept)"] <- "(Intercept)"
  rownames(C) <- sub("(Intercept):", "", rownames(C), fixed=TRUE)
  rownames(C) <- sub(":(Intercept)", "", rownames(C), fixed=TRUE)
  return(C)
}
