wd <- setwd("./R")
source("stdParm functions.r", local=TRUE)
setwd(wd)

addm   <- lm(mpg ~ 0 + wt + disp, data=mtcars)
mu    <- colMeans(mtcars[,c("wt", "disp")])
sigma <- colSds(mtcars[,c("wt", "disp")])
terms <- names(coef(addm))
terms

recentering.matrix(mu, terms)
recentering.matrix(sigma, terms, type="scale") # rescale without recentering

C <-recentering.matrix(mu, terms)
recentering.matrix(sigma, row.names(C), type="scale") # rescale after recentering

intatzero   <- lm(mpg ~ 0 + wt*disp, data=mtcars)
mu    <- colMeans(mtcars[,c("wt", "disp")])
sigma <- colSds(mtcars[,c("wt", "disp")])
terms <- names(coef(intatzero))
terms

recentering.matrix(mu, terms)
recentering.matrix(sigma, terms, type="scale") # rescale without recentering

C <-recentering.matrix(mu, terms)
recentering.matrix(sigma, row.names(C), type="scale") # rescale after recentering

nested   <- lm(mpg ~ wt + wt:disp, data=mtcars)
mu    <- colMeans(mtcars[,c("wt", "disp")])
sigma <- colSds(mtcars[,c("wt", "disp")])
terms <- names(coef(nested))
terms

recentering.matrix(mu, terms)
recentering.matrix(sigma, terms, type="scale") # rescale without recentering

C <-recentering.matrix(mu, terms)
recentering.matrix(sigma, row.names(C), type="scale") # rescale after recentering

polymod   <- lm(mpg ~ I(wt^2), data=mtcars)
mu    <- mean(mtcars$wt)
names(mu) <- "wt"
sigma <- sd(mtcars$wt)
names(sigma) <- "wt"
terms <- poly.fix(names(coef(polymod)))
terms

recentering.matrix(mu, terms)
recentering.matrix(sigma, terms, type="scale") # rescale without recentering

C <-recentering.matrix(mu, terms)
recentering.matrix(sigma, row.names(C), type="scale") # rescale after recentering
