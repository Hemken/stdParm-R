source("stdParm functions.r")
source("gen_ex_models.r")

df <- gen_2x_f(100L, sigma=0.3)

b <- coef(fit <- lm(y~ x1*x2+f*x1, df))
b.terms <- names(b)
b

summary(fit)

fln <- nlevels(df$f)
fnames    <- paste0("f", levels(df$f))
fnames[1] <- "(Intercept)"

x.means <- colMeans(df[,c("x1","x2")])
x.sds <- colSds(df[,c("x1","x2")])

C <- matrix.build.clean(x.means, b.terms)
S <- matrix.build.clean(x.sds, b.terms, type="scale")

Z <- S %*% C # the order is crucial
Z
 
Z.plus <- factor.direct.sum(Z,fnames)
found <- matching.terms(colnames(Z.plus), b.terms)
Z <- Z.plus[found,found]
Z

# x-standardized
b.x <- Z%*%b
b.x

# fully standardized
b.x[1] <- b.x[1]-mean(df$y)
b.z <- b.x/sd(df$y)
b.z

library(stdBeta)
stdBeta(fit)
