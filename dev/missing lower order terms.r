setwd("R")
source("stdParm functions.r")
setwd("..")

m0 <- lm(mpg ~ wt + wt:disp, data=mtcars)

terms(m0)

b.terms <- attr(terms(m0), "term.labels")
vars.in.terms(b.terms)
terms.vars.degrees(b.terms)

mpoly <- lm(mpg ~ wt + I(wt^2) + (wt + I(wt^2)):disp, data=mtcars)
p.terms <- attr(terms(mpoly), "term.labels")
p.terms <- poly.fix(p.terms)

# splitterms <- strsplit(p.terms, ":")
# sapply(lapply(splitterms, unique), length)

tvd <- terms.vars.degrees(p.terms)
tvd

tvd[,"wt"]
wt.order <- attr(tvd, "order")[tvd[,"wt"]!=0]
all(1:max(wt.order) %in% wt.order)

tvd[,"disp"]
disp.order <- attr(tvd, "order")[tvd[,"disp"]!=0]
all(1:max(disp.order) %in% disp.order)
