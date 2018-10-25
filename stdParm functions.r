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

# Example problem to standardize
n <- 100
x1 <- runif(n, 2, 7)
x2 <- runif(n, 2, 7)
y <- 1 + 2*x1 + 3*x2 + 4*x1*x2 + rnorm(n)

df <- data.frame(y, x1, x2)

b <- coef(lm(y~ x1*x2, df))
b

z <- mean(x1)
names(z) <- "x1"
c1 <- mean.to.matrix(z)
z <- mean(x2)
names(z) <- "x2"
c2 <- mean.to.matrix(z)
s1 <- sd.to.matrix(sd(x1))
s2 <- sd.to.matrix(sd(x2))

Z <- (s2 %x% s1) %*% (c2 %x% c1) # the order is crucial

# x-standardized
b.xstd <- Z %*% b
b.xstd

b.xystd <- b.xstd # init
b.xystd[1] <- b.xystd[1] - mean(y)
b.xystd <- b.xystd/sd(y)
b.xystd

# Compare
x1.std <- scale(x1)
x2.std <- scale(x2)
y.std  <- scale(y)

coef(lm(y.std ~ x1.std*x2.std, df))

# Useful row and column names for our Kronecker products
c2 %x% c1 # no dim names
kronecker(c2, c1, make.dimnames=TRUE)
kron(c2,c1)

names(b) %in% colnames(C)
