clean.order <- function (A, terms.to.match) {
  termorder <- match.terms(colnames(A), terms.to.match)
  Z <- A[termorder$found,termorder$found]  # remove extra columns
  rownames(Z) <- colnames(Z) <- termorder$found.as # fix names
  colorder <- order(termorder$found.at)  # reorder
  Z <- Z[colorder, colorder]
  return(Z)
}
