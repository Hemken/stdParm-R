#' poly.fix
#' 
#' @description Converts polynomial terms like I(x^2)
#'     to the interaction form x:x
#'     
#' @param terms a character vector of terms, possibly including
#'     interaction terms
#' 
#' @return a character vector of converted terms
#' 
#' @examples 
#'     poly.fix(c("Intercept", "x", "I(x^2)"))
#'     

poly.fix <- function(terms) {
  stopifnot(is.character(terms))
  termlist <- strsplit(terms, ":")
  termconv <- lapply(termlist, poly.to.interaction)
  termcollapsed <- lapply(termconv, paste, collapse=":")
  
  return(unlist(termcollapsed))
}

