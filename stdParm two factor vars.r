source("stdParm functions.r")
source("gen_ex_models.r")

df <- gen_2x_2f(100L, sigma=0.3)
# two x variables, x1 and x2, and two factor
#   variables, f1 and f2, with levels {a,b} and {A,B}

# Original model, to be centered and scaled
summary(fit <- lm(y~ x1*x2+f1*x1+f2*x2, df))

# extract coefficients, term names
#   b.terms is the target list of terms to represent
#   in the transformation matrices we seek to construct
b <- coef(fit)
b.terms <- names(b)
b

# To begin thinking about constructing transformation
#   matrices, we need to know all the variables in
#   the model, whether they are continuous or factor
#   variables, and the polynomial degree 
#   for continuous variables.
modelvars <- all.vars(formula(fit))
numvars <- sapply(df[, modelvars], is.numeric)

# means and sds of continuous variables
x.means <- colMeans(df[,numvars][-1])
x.sds <- colSds(df[,numvars][-1])

# recentering and rescaling matrices
C <- matrix.build.clean(x.means, b.terms)
S <- matrix.build.clean(x.sds, b.terms, type="scale")

# combine to standardize
Z <- S %*% C # the order is crucial
Z

###################################
# Now combine with factor variables
###################################

# number of levels, term names
fln <- nlevels(df$f1)
fnames    <- paste0("f1", levels(df$f1))
fnames[1] <- "(Intercept)"

# given the factor term names, build Z
Z.plus <- factor.direct.sum(Z,fnames)
found <- matching.terms(colnames(Z.plus), b.terms)
Z <- Z.plus[found,found]
Z

fln <- nlevels(df$f2)
fnames    <- paste0("f2", levels(df$f2))
fnames[1] <- "(Intercept)"

# given the factor term names, build Z
Z.plus <- factor.direct.sum(Z,fnames)
found <- matching.terms(colnames(Z.plus), b.terms)
Z <- Z.plus[found,found]
Z

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
