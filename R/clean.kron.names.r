clean.kron.names <- function (xnames) {
  stopifnot(is.character(xnames))
  cns <- strsplit(xnames, split=":", fixed=TRUE)
  cns <- lapply(cns, function(x) c(x[-1],x[1]))
  cns <- lapply(cns, function (x) paste(x, collapse=":"))
  
  cns[cns=="(Intercept):(Intercept)"] <- "(Intercept)"
  cns <- sub("(Intercept):", "", cns, fixed=TRUE)
  cns <- sub(":(Intercept)", "", cns, fixed=TRUE)
  
  return(cns)
}
