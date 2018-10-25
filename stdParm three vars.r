library(MASS)

nobs <- 100
Sigma <- matrix(c(1, .5, .25, .5, 1, .3, .25, .3, 1), ncol=3)
X <- as.data.frame(mvrnorm(nobs, c(0,1,2), Sigma=Sigma))
names(X) <- paste0("x", 1:3)

x.means <- colMeans(X)
x.sds   <- colSds(X)
y <- with(X,
  1 + (-1)*x1 + 2*x2 + (-2)*x1*x2 + 3*x3 + rnorm(nobs)
)

df <- data.frame(y, X)

b <- coef(lm(y~ x1*x2 + x3, df))
b.terms <- names(b)
b

nmeans <- length(x.means)
varnames <- names(x.means)
S <- C <- matrix(1, ncol=1)
colnames(S) <-rownames(S) <- colnames(C) <-rownames(C) <- "(Intercept)"
for (i in 1:nmeans){
  x <- x.means[i]
  names(x) <- varnames[i]
  C <- kron(mean.to.matrix(x), C)
  inmodel <- colnames(C) %in% b.terms
  print(inmodel)
  print(colnames(C)[!inmodel])
  C <- C[, inmodel]
}
C

for (i in 1:nmeans){
  x <- x.sds[i]
  names(x) <- varnames[i]
  S <- kron(sd.to.matrix(x), S)
}

Z <- S %*% C # the order is crucial
Z
