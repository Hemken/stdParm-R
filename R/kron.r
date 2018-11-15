kron <- function(A,B) {
  # forms A %x% B for coefficient transformation matrices
  # assumes A and B already have useful row and column names
  #stopifnot(is.matrix(A) & is.matrix(B))
  C <- kronecker(A, B, make.dimnames=TRUE)
  
  colnames(C) <- clean.kron.names(colnames(C))
  rownames(C) <- clean.kron.names(rownames(C))

  return(C)
}

