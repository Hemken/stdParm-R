poly.to.interaction <- function (terms) {
  # Converts polynomial terms like I(x^2), written
  #   with the I() inhibit function, to the
  #   interaction form, x:x
  
  # Returns a character vector of the converted terms
  
  # 
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
  
  # pull out the variable
  pvars  <- sub(prevar, "", pterms)
  pvars  <- sub(postvar, "", pvars)
  
  # pull out the polynomial degree
  pdegree <- sub(preexpt, "", pterms)
  pdegree <- sub(postexpt, "", pdegree)
  
  # conversion
  new     <- sapply(1:length(pterms), function(x) paste(rep(pvars[x], pdegree[x]), collapse=":"))
  
  terms[change] <- new
  return(terms)
}

terms <- c("I(x^2)","I(wt^2)","I(log(disp)","I(x^2+3)","I(x^5)", "x*y*z^2")

# polynomial terms, using I()
grepl("^I\\(.*\\^[[:digit:]]\\)$", terms) 

poly.to.interaction(terms)
