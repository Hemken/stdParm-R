matrix.build.clean <- function(x, terms, type="center") {
  stopifnot(is.numeric(x), is.character(terms),
            type %in% c("center", "scale"))
  nx <- length(x)
  varnames <- names(x)
  
  C <- matrix(1, ncol=1)
  colnames(C) <-rownames(C) <- "(Intercept)"
  
  for (i in 1:nx){ # over means
    xc <- x[i]
    names(xc) <- varnames[i]
    if (type=="center") { # centering or scaling matrix
      C <- kron(mean.to.matrix(xc), C)
    } else {
      C <- kron(sd.to.matrix(xc), C)
    }                    # centering or scaling matrix
    
    found <- matching.terms(colnames(C), terms)
    C <- C[found, found]
  }         # over means
  return(C)
}
