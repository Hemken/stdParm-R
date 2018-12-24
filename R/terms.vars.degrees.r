#' terms.vars.degrees
#' 
#' Generate a terms-variables-degrees table/matrix
#' 
#' @param terms a character vector of model terms/coefficients
#' 
#' @return a matrix with the terms as \code{rownames},
#'     the constituent variables as \code{colnames}, and
#'     the polynomial degree of each variable in each term
#'     as the table cell. The matrix also has an \code{order}
#'     attribute, recording the interaction order of each
#'     term.
#'     
#' @examples 
#'     fit <- lm(mpg ~ wt*disp, data=mtcars)
#'     terms <- names(coef(fit))
#'     terms.vars.degrees(terms)
#'     
#' @seealso 
#'     \code{terms.object}, \code{terms}, \code{formula}
#'     

terms.vars.degrees <- function(terms){
  # terms is a vector of coefficient names
  # returns a matrix of terms and variables
  stopifnot(is.character(terms))
  splitterms <- strsplit(terms, ":")
  vars <- unique(unlist(splitterms))
  
  term.vars <- matrix(0, ncol=length(vars), nrow=length(terms))
  rownames(term.vars) <- terms
  colnames(term.vars) <- vars
  
  for (i in 1:length(splitterms)) {
    for (j in 1:length(splitterms[[i]])) {
      for (k in 1:length(vars)) {
        if (splitterms[[i]][j] == vars[k]){
          term.vars[i,k] <- term.vars[i,k] + 1
        }
      }
    }
  }
  attr(term.vars, "order") <- sapply(lapply(splitterms, unique), length)
  return(term.vars)
}
