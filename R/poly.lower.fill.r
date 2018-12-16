poly.lower.fill <- function(degrees) {
  # degrees - named vector of polynomial degrees
  # polyterms  <- degrees[degrees>1]
  pterms <- vector("list", 0)
  pnames     <- names(degrees)
  for (i in 1:length(degrees)) {
    pterms[[i]] <- poly.to.interaction(paste0("I(",pnames[i], "^", 1:degrees[i],")"))
    # pterms <- c(pterms, polyint)
  }
  names(pterms) <- pnames
  return(pterms)
}

