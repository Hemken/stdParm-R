setwd("R")
source("stdParm functions.r")
setwd("..")

m0 <- lm(mpg ~ wt + wt:disp, data=mtcars)

# terms(m0)

b.terms <- attr(terms(m0), "term.labels")
# vars.in.terms(b.terms)
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
anymissing <- !all(1:max(wt.order) %in% wt.order)
if (anymissing) (1:max(wt.order))[!(1:max(wt.order) %in% wt.order)]

# tvd[,"disp"] # terms, degree of "disp"
disp.order <- attr(tvd, "order")[tvd[,"disp"]!=0] # order of terms w/ "disp"
anymissing <- !all(1:max(disp.order) %in% disp.order) # missing term indicator
if (anymissing) (1:max(disp.order))[!(1:max(disp.order) %in% disp.order)]

find.missing.terms <- function (tvd) {
  # tvd - terms.vars.degrees return object
  nvar <- ncol(tvd)
  varnames <- colnames(tvd)
  missed <- vector("list", length=nvar)
  names(missed) <- varnames
  for (i in 1:nvar) {
    print(varnames[i])
    v.order <- attr(tvd, "order")[tvd[,i]!=0]
    all.orders <- 1:max(v.order)
    anymissing <- !all(all.orders %in% v.order) # missing term indicator
    if (anymissing) {
      v.miss <- all.orders[!(all.orders %in% v.order)]
      missed[[i]] <- v.miss
    }
  }
  print(missed)
}

find.missing.terms(tvd)

find.missing.terms(terms.vars.degrees(c("w:z", "x:y:z")))

tvd <- terms.vars.degrees(c("w:z", "x:y:z"))
order0 <- "(Intercept)"
order1 <- colnames(tvd)

pastex <- function (mat) {
  apply(mat, 2, paste, collapse=":")
}
order2 <- pastex(combn(c("w","z"),2))
order2 <- c(order2, pastex(combn(c("x","y","z"),2)))
order3 <- pastex(combn(c("x","y","z"),3))

unique(c(order0, order1, order2, order3))
