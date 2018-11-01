vars.in.terms <- function(terms){
  # terms is a vector of coefficient names
  # returns a matrix of terms and variables
  stopifnot(is.character(terms))
  splitterms <- strsplit(terms, ":")
  vars <- unique(unlist(splitterms))
  
  term.vars <- matrix(FALSE, ncol=length(vars), nrow=length(terms))
  rownames(term.vars) <- terms
  colnames(term.vars) <- vars
  
  for (i in 1:length(splitterms)) {
    for (j in 1:length(splitterms[[i]])) {
      for (k in 1:length(vars)) {
        if (splitterms[[i]][j] == vars[k]){
          term.vars[i,k] <- TRUE
        }
      }
    }
  }
  return(term.vars)
}