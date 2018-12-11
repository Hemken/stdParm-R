polyterm <- function(mu, b.terms, type="center"){
  stopifnot(is.numeric(mu), length(mu)==1, length(names(mu))==1,
            is.character(b.terms))
  
  term.vars <- terms.vars.degrees(b.terms)
  polydegree <- max(term.vars[,names(mu)])
  
  if (type=="center") {
    A <- C <- mean.to.matrix(mu) # recentering x - 3
  } else if (type=="scale") {
    A <- C <- sd.to.matrix(mu)
  }
  if (polydegree > 1){
    for (i in 2:polydegree){
      C <- kron(A,C)
      }
  }
 # print(C)
  
  # drop/zero repeated column terms
  cnames <- colnames(C)
  # print(cnames)
  keepcols <- match(b.terms, cnames)
  # print(keepcols)
  C <- C[, keepcols[!is.na(keepcols)]]
#  colnames(C) <- cnames[keepcols] # redundant?
 # print(C)
  
  if (type=="center") {
    # collect repeated row terms
    cnames <- colnames(C) # reduced columns
    rnames <- rownames(C)
    rowcombine <- matrix(NA, nrow=length(cnames), ncol=length(rnames))
    for (i in 1:length(cnames)) {
      rowcombine[i,] <- rnames %in% cnames[i]
    }
    C <- rowcombine %*% C
    rownames(C) <- cnames
  } else if (type=="scale") {
    C <- C[keepcols[!is.na(keepcols)], ] # keep the same rows as columns
  }
  
  return(C)
}