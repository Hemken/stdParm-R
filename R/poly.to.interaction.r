#' poly.to.interaction
#' 
#' @description Converts polynomial terms like I(x^2)
#'     to the interaction form x:x
#'     
#' @param terms a character vector of simple terms
#'     (no intercation terms)
#' 
#' @return a character vector of converted terms
#' 
#' @seealso \code{poly.fix}
#' 
#' @examples 
#'     poly.to.interaction(c("Intercept", "x", "I(x^2)"))
#'     

poly.to.interaction <- function (terms) {
  stopifnot(is.character(terms))

  # some regex patterns
  polyregex <- "^(I\\()(.*)(\\^)([[:digit:]])(\\))$"
  prevar   <- "^I\\("
  postvar  <- "\\^[[:digit:]]\\)$"
  preexpt  <- "^I\\(.*\\^"
  postexpt <- "\\)$"
  
  # which terms to convert
  change <- grepl(polyregex, terms) # polynomial terms, using I()
  pterms <- terms[change]
  # print(pterms)
  
  if (length(pterms)>0) {
    # pull out the variable
    pvars  <- sub(prevar, "", pterms)
    pvars  <- sub(postvar, "", pvars)
    
    # pull out the polynomial degree
    pdegree <- sub(preexpt, "", pterms)
    pdegree <- sub(postexpt, "", pdegree)
    
    # conversion
    new     <- sapply(1:length(pterms), function(x) paste(rep(pvars[x], pdegree[x]), collapse=":"))
    
    terms[change] <- new
  }
  return(terms)
}
