poly.fix <- function(terms) {
  # For compound terms, polynomials within interaction terms
  #
  # Converts polynomial terms like I(x^2), written
  #   with the I() inhibit function, to the
  #   interaction form, x:x
  
  # Returns a character vector of the converted terms
  # 
  stopifnot(is.character(terms))
  termlist <- strsplit(terms, ":")
  # print(termlist)
  termconv <- lapply(termlist, poly.to.interaction)
  # print(termconv)
  termcollapsed <- lapply(termconv, paste, collapse=":")
  return(unlist(termcollapsed))
}

