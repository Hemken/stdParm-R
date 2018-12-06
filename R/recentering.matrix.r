recentering.matrix <- function(x, terms, type="center") {
  stopifnot(is.numeric(x), is.character(terms),
            type %in% c("center", "scale"))
  nx <- length(x)
  varnames <- names(x)
  
  C <- matrix(1, ncol=1)
  colnames(C) <-rownames(C) <- "(Intercept)"
  
  for (i in 1:nx){ # over means
    xc <- x[i]
    names(xc) <- varnames[i]
    # print(varnames[i])
    if (type=="center") { # centering or scaling matrix
      A <- polyterm(xc, terms) # integrate polynomial terms here
      C <- kron(A, C)
    } else if (type=="scale") {
      A <- polyterm(xc, terms, type="scale") # integrate polynomial terms here
      # print(A)
      # print(C)
      C <- kron(A, C)
      # C <- kron(sd.to.matrix(xc), C)
    }                    # centering or scaling matrix
    
    found <- matching.terms(colnames(C), terms)
    # check for missing lower order terms
    C <- C[,found]
    C <- C[rowSums(C)>0, ] 
  }         # over means
  return(C)
}
