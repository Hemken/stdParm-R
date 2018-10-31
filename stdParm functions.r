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
    
    found <- colnames(C)[colnames(C) %in% terms]
    tocheck <- colnames(C)[!(colnames(C) %in% terms)]
    if (length(tocheck)>0){           # if tocheck
      needles <- vars.in.terms(tocheck)
      notfound <- NULL
      haystack <- vars.in.terms(terms)[,colnames(needles)]
      for (j in 1:nrow(needles)) {    # over needle rows
        for (k in 1:nrow(haystack)) { # over haystack rows
          if (identical(needles[j,], haystack[k,])) { # if in model
            found <- unique(c(found, rownames(needles)[j]))
          } # if in model
        }   # over haystack rows
      }     # over needle rows
    }       # if tocheck
    C <- C[found, found]
  }         # over means
  return(C)
}

factor.direct.sum <- function(A, f) {
  stopifnot(is.matrix(A), is.numeric(A), is.factor(f))
  # also check symmetry of A
  
  fln <- nlevels(f)
  fnames    <- paste0(deparse(substitute(f)), levels(f))
  #  fnames[1] <- "(Intercept)"
  
  nA <- nrow(A)
  B <- A
  
  for (i in 2:fln) {
    nB <- nrow(B)
    Bzeros <- matrix(0, nrow=nB, ncol=nA)
    colnames(Bzeros) <- paste(fnames[i],colnames(A),sep=":")
    Azeros <- matrix(0, nrow=nA, ncol=nB)
    rownames(Azeros) <- paste(fnames[i],rownames(A),sep=":")
    
    B <- rbind( cbind( B, Bzeros ),
                cbind( Azeros, A ) )
    rownames(B) <- colnames(B) <- sub(":\\(Intercept\\)$", "", colnames(B))
  }
  return(B)
}