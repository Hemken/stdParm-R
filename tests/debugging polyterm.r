wd <- setwd("Z:/R/stdParm-R/R")
source("stdParm functions.r", local=TRUE)
setwd("Z:/R/stdParm-R")

addm   <- lm(mpg ~ 0 + wt + disp, data=mtcars)
mu    <- colMeans(mtcars[,c("wt", "disp")])
terms <- names(coef(addm))
terms

recentering.matrix(mu, terms)
traceback()

# polyterm returns an empty matrix

for (i in 1:length(mu)) {
  xc <- mu[i]
  # print(varnames[i])
  names(xc) <- names(mu)[i]
  # print(xc)
  
  print(polyterm(xc, terms))
  
}
    
