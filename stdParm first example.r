source("StdParm functions.r")
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
