source("stdParm functions.r")
source("gen_2x_f.r")

df <- gen_2x_f(100L, sigma=0.3)

b <- coef(fit <- lm(y~ x1*x2*f, df))
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
