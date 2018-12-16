setwd("R")
source("stdParm functions.r")
setwd("..")

# Disjoint first order terms
tvd <- terms.vars.degrees(c("w:z", "x:y:z"))
generate.all.terms(tvd) # 10 terms

# Additive
m1 <- lm(mpg ~ wt + disp, data=mtcars)
b.terms <- names(coef(m1))
b.terms
tvd <- terms.vars.degrees(b.terms)
generate.all.terms(tvd) # 3 terms

# No (Intercept)
m0 <- lm(mpg ~ wt + 0, data=mtcars)
b.terms <- names(coef(m0))
b.terms
tvd <- terms.vars.degrees(b.terms)
generate.all.terms(tvd) # 2 terms

# Nested term
mnest <- lm(mpg ~ wt + wt:disp, data=mtcars)
b.terms <- names(coef(mnest))
b.terms
tvd <- terms.vars.degrees(b.terms)
generate.all.terms(tvd) # 4 terms

# Polynomial - nested in polynomial
mpoly <- lm(mpg ~ wt + I(wt^2) + (wt + I(wt^2)):disp, data=mtcars)
p.terms <- names(coef(mpoly))
p.terms <- poly.fix(p.terms)
p.terms
tvd <- terms.vars.degrees(p.terms)
generate.all.terms(tvd)  # 6 terms

tvd <- terms.vars.degrees(c("w:z:z", "x:y:y:y:z"))
generate.all.terms(tvd) # 10 terms
