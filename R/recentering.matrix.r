recentering.matrix <- function(x, terms, type="center") {
  stopifnot(is.numeric(x), is.character(terms),
            type %in% c("center", "scale"))
  nx <- length(x)
  varnames <- names(x)
  complete.terms <- generate.all.terms(terms.vars.degrees(terms))
  
  C <- matrix(1, ncol=1)
  colnames(C) <-rownames(C) <- "(Intercept)"
  
  for (i in 1:nx){ # over means
    xc <- x[i]
    # print(paste("varname", i, varnames[i]))
    names(xc) <- varnames[i]
    # print(xc)
    if (type=="center") { # centering or scaling matrix
      # print(polyterm(xc, terms))
      A <- polyterm(xc, complete.terms) # integrate polynomial terms here
      # print(A)
      # print(C)
      C <- kron(A, C)
    } else if (type=="scale") {
      A <- polyterm(xc, complete.terms, type="scale") # integrate polynomial terms here
      # print(A)
      # print(C)
      C <- kron(A, C)
      # C <- kron(sd.to.matrix(xc), C)
    }                    # centering or scaling matrix
    
    # found <- matching.terms(colnames(C), terms)
    # message(varnames[i], " colnames: ", colnames(C))
    # message("searching: ", terms)
    # matched <- match.terms(colnames(C), terms)
    matched <- match.terms(colnames(C), complete.terms)
    # message("match.terms return")
    # print(matched)
    found <- unique(c("(Intercept)", matched$found)) # must include (Intercept)
    # check for missing lower order terms
    # print(C)
    C <- C[,found]
    # print(C)
    # message(str(C))
    C <- C[rowSums(C)>0, ] 
  }         # over means
  C <- C[,terms]
  C <- C[rowSums(C)>0, ] 
  return(C)
}
