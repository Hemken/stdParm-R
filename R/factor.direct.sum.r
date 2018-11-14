factor.direct.sum <- function(A, f.terms) {
  stopifnot(is.matrix(A), is.numeric(A), is.character(f.terms))
  # also check symmetry of A
  
  fln <- length(f.terms)
  
  nA <- nrow(A)
  B <- A
  
  for (i in 2:fln) {
    nB <- nrow(B)
    Bzeros <- matrix(0, nrow=nB, ncol=nA)
    colnames(Bzeros) <- paste(colnames(A),f.terms[i],sep=":")
    Azeros <- matrix(0, nrow=nA, ncol=nB)
    rownames(Azeros) <- paste(rownames(A),f.terms[i],sep=":")
    
    B <- rbind( cbind( B, Bzeros ),
                cbind( Azeros, A ) )
    rownames(B) <- colnames(B) <- sub("^\\(Intercept\\):", "", colnames(B))
  }
  return(B)
}