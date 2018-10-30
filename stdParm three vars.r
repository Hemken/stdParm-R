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

C <- matrix.build.clean(x.means, b.terms)
S <- matrix.build.clean(x.sds, b.terms, type="scale")

Z <- S %*% C # the order is crucial
Z

Z%*%b # x-standardized

# Compare
df$x1.std <- scale(df$x1)
df$x2.std <- scale(df$x2)
df$x3.std <- scale(df$x3)
coef(lm(y~ x1.std*x2.std + x3.std, df))

library(stdBeta)
stdBeta(lm(y~ x1*x2 + x3, df)) # fully standardized

C%*%b # x-centered
