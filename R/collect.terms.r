collect.terms <- function (A) {
  stopifnot(is.matrix(A))
  cnames <- colnames(A)
  keepcols <- match(unique(cnames), cnames)
  A <- A[, keepcols]
  colnames(A) <- cnames[keepcols]
#  A
  cnames <- colnames(A)
  rnames <- rownames(A)
  rowcombine <- matrix(NA, nrow=length(cnames), ncol=length(rnames))
  for (i in 1:length(cnames)) {
    rowcombine[i,] <- rnames %in% cnames[i]
  }
  A <- rowcombine %*% A
  rownames(A) <- cnames
  return(A)
}
