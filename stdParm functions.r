mean.to.matrix <- function(x){
  stopifnot(is.numeric(x), length(x)==1, !is.null(names(x)))
  centering.matrix <- matrix(c(1,0,x,1), ncol=2)
  colnames(centering.matrix) <- c("(Intercept)", names(x))
  rownames(centering.matrix) <- c("(Intercept)", names(x))
  return(centering.matrix)
}

sd.to.matrix <- function(x){
  stopifnot(is.numeric(x), length(x)==1, !is.null(names(x)))
  scaling.matrix <- matrix(c(1,0,0,x), ncol=2)
  colnames(scaling.matrix) <- c("(Intercept)", names(x))
  rownames(scaling.matrix) <- c("(Intercept)", names(x))
  return(scaling.matrix)
}

colSds <- function(x, na.rm=FALSE){
  stopifnot(is.data.frame(x) || is.matrix(x))
  col.sds <- apply(x,2,sd,na.rm=na.rm)
  names(col.sds) <- colnames(x)
  return(col.sds)
}

kron <- function(A,B) {
  # forms A %x% B for coefficient transformation matrices
  # assumes A and B already have useful row and column names
  #stopifnot(is.matrix(A) & is.matrix(B))
  C <- kronecker(A, B, make.dimnames=TRUE)
  colnames(C)[colnames(C)=="(Intercept):(Intercept)"] <- "(Intercept)"
  colnames(C) <- sub("(Intercept):", "", colnames(C), fixed=TRUE)
  colnames(C) <- sub(":(Intercept)", "", colnames(C), fixed=TRUE)
  rownames(C)[rownames(C)=="(Intercept):(Intercept)"] <- "(Intercept)"
  rownames(C) <- sub("(Intercept):", "", rownames(C), fixed=TRUE)
  rownames(C) <- sub(":(Intercept)", "", rownames(C), fixed=TRUE)
  return(C)
}

vars.in.terms <- function(terms){
  # terms is a vector of coefficient names
  # returns a matrix of terms and variables
  stopifnot(is.character(terms))
  splitterms <- strsplit(terms, ":")
  vars <- unique(unlist(splitterms))
  
  term.vars <- matrix(FALSE, ncol=length(vars), nrow=length(terms))
  rownames(term.vars) <- terms
  colnames(term.vars) <- vars
  
  for (i in 1:length(splitterms)) {
    for (j in 1:length(splitterms[[i]])) {
      for (k in 1:length(vars)) {
        if (splitterms[[i]][j] == vars[k]){
          term.vars[i,k] <- TRUE
        }
      }
    }
  }
  return(term.vars)
}
