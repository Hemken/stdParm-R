source("stdParm functions.r")
source("gen_2x_f.r")

df <- gen_3x(100L, sigma=matrix(c(1,.5,.25,.5,1,.3,.25,.3,1), ncol=3))

b <- coef(lm(y~ x1*x2 + x3, df))
b.terms <- names(b)
b

x.means <- colMeans(df[,2:4])
x.sds   <- colSds(df[,2:4])

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
