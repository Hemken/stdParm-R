source("stdParm functions.r")
source("gen_ex_models.r")

df <- gen_2x_f(100L, sigma=0.3)
# two x variables, x1 and x2, and one factor
#   variable, f, with levels a and b

# Original model, to be centered and scaled
summary(fit <- lm(y~ x1*x2+f*x1, df))

# extract coefficients, term names
b <- coef(fit)
b.terms <- names(b)
b

# means and sds of continuous variables
x.means <- colMeans(df[,c("x1","x2")])
x.sds <- colSds(df[,c("x1","x2")])

# recentering and rescaling matrices
C <- recentering.matrix(x.means, b.terms)
S <- recentering.matrix(x.sds, b.terms, type="scale")

# combine to standardize
Z <- S %*% C # the order is crucial
Z

###################################
# Now combine with factor variables
###################################

# number of levels, term names
fln <- nlevels(df$f)
fnames    <- paste0("f", levels(df$f))
fnames[1] <- "(Intercept)"

# given the factor term names, build Z
Z.plus <- factor.direct.sum(Z,fnames)
found <- matching.terms(colnames(Z.plus), b.terms)
Z <- Z.plus[found,found]
Z
Z[b.terms,b.terms]

# Now use the result, and check it
# x-standardized
b.x <- Z%*%b
b.x

# fully standardized
b.x[1] <- b.x[1]-mean(df$y)
b.z <- b.x/sd(df$y)
b.z

library(stdBeta)
stdBeta(fit)

cylf <- as.factor(mtcars$cyl)
excat <- lm(mpg ~ cylf, data=mtcars)
summary(excat)

#contrasts(cylf) <- contr.treatment #ref.to.gm(3)[,-1]
contrasts(cylf) <- contr.sum #ref.to.gm(3)[,-1]
excat2 <- lm(mpg ~ cylf, data=mtcars)
#excat2 <- lm(mpg ~ wt*cylf, data=mtcars)
summary(excat2) # dropped level is now 3, not 1

#excat <- lm(mpg ~ wt*cylf, data=mtcars)
C <- ref.to.gm(3)
C %*% coef(excat) # note the dropped level is 1, not 3

