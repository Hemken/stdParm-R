source("stdParm functions.r")
library(MASS)

nobs <- 100
Sigma <- matrix(c(1, .75, .75, 1), ncol=2)
X <- as.data.frame(mvrnorm(nobs, c(0,1), Sigma=Sigma))
names(X) <- paste0("x", 1:2)

x.means <- colMeans(X)
x.sds   <- colSds(X)

f <- as.factor(sample(letters[1:3], nobs, replace=TRUE))
y <- with(X,
          1 + (-1)*x1 + 2*x2 + (-2)*x1*x2 + 0.5*(f=="b") - 0.5*(f=="c") + rnorm(nobs)
)

df <- data.frame(y, X, f)

b <- coef(fit <- lm(y~ x1*x2*f, df))
b.terms <- names(b)
b

summary(fit)
