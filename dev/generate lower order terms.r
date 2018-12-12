setwd("R")
source("stdParm functions.r")
setwd("..")

tvd <- terms.vars.degrees(c("w:z", "x:y:z"))

generate.all.terms(tvd)


m0 <- lm(mpg ~ wt + wt:disp, data=mtcars)
b.terms <- names(coef(m0))
b.terms
tvd <- terms.vars.degrees(b.terms)

generate.all.terms(tvd)

mpoly <- lm(mpg ~ wt + I(wt^2) + (wt + I(wt^2)):disp, data=mtcars)
p.terms <- names(coef(mpoly))
p.terms <- poly.fix(p.terms)
p.terms

tvd <- terms.vars.degrees(p.terms)
generate.all.terms(tvd)  # does not generate polynomial terms
p.terms
