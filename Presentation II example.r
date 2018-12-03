example <- gen_2x_poly(means=c(5,10), 
                       sigma=matrix(c(3,.5,.5,.75), ncol=2),
                       coefs=runif(9)*4)
example <- example[order(example$x1),]

orig <- lm(y ~  x1*x2, data=example)
library(QuantPsyc)
# c(0,lm.beta(orig))

library(stdBeta)
std <- stdBeta(orig)

tbl <- cbind(coef(orig), c(0,lm.beta(orig)),coef(std))
colnames(tbl) <- c("Original", "Classic Formula", "Recalculated")
zapsmall(tbl, digits=6)

example$yhat <- predict(orig)

exstd <- model.frame(std)
exstd$yhat <- predict(std)

plot(y ~ x1, data=example, main="Original")
points(yhat ~ x1, data=example, type="l", col="red")

plot(y ~ x1, data=exstd, main="Standardized",
     xlab=expression(x1[sigma]),
     ylab=expression(y[sigma]))
points(yhat ~ x1, data=exstd, type="l", col="red")
