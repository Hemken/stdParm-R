setwd("R")
source("stdParm functions.r")
setwd("..")
setwd("dev")
source("gen_ex_models.r")
setwd("..")

df <- gen_3x(means=c(0,1,2), sigma=matrix(c(1, .5, .25, .5, 1, .3, .25, .3, 1),ncol=3),
       coefs=c(1,-1,2,3,-2,0,0,0))

b <- coef(lm(y~ x1*x2 + x3, df))
b.terms <- names(b)
b

terms.vars.degrees(b.terms)

