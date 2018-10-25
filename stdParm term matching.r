source("stdParm functions.r")
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

vars.in.terms(b.terms)

