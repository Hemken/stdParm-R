polyterm <- function(mu, b.terms){
  stopifnot(is.numeric(mu), length(mu)==1, length(names(mu))==1,
            is.character(b.terms))
  # figure out the polynomial degree of name(mu)A <- mean.to.matrix(mu) # recentering x - 3
  A <- C <- mean.to.matrix(mu) # recentering x - 3
  if (polydegree > 1){
    for (i in 1:polydegree){
      C <- kron(A,C)
      }
  }
  C
  
  # drop/zero repeated column terms
  cnames <- colnames(C)
  keepcols <- match(b.terms, cnames)
  C <- C[, keepcols]
  colnames(C) <- cnames[keepcols]
  C
  
  # collect repeated row terms
  cnames <- colnames(C) # reduced columns
  rnames <- rownames(C)
  rowcombine <- matrix(NA, nrow=length(cnames), ncol=length(rnames))
  for (i in 1:length(cnames)) {
    rowcombine[i,] <- rnames %in% cnames[i]
  }
  C <- rowcombine %*% C
  rownames(C) <- cnames
  C
}