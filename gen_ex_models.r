# Two correlated x vars and a factor

gen_2x_f <- function(nobs, means=c(0,0), sigma=0, nlevels=2L,
                     coefs=c(rep(1L, times=4*nlevels)),
                     sd.resid=1) {
  # factor has full interaction with the x variables
  # coefs order=first order terms, second order terms, third order terms
  stopifnot(length(nobs)==1, is.integer(nobs))
  stopifnot(length(means)==2, is.numeric(means))
  stopifnot(length(sigma)==1, is.numeric(sigma))
  stopifnot(length(nlevels)==1, is.integer(nlevels), nlevels>=2)
  stopifnot(length(coefs)==4*nlevels, is.numeric(coefs), !all(coefs==0))
  
  require(MASS)
  
  Sigma <- matrix(c(1, sigma, sigma, 1), ncol=2)
  X <- mvrnorm(nobs, means, Sigma=Sigma) # this is what MASS if for
  x1 <- X[,1]; x2 <- X[,2]

  f <- as.factor(sample(letters[1:nlevels], nobs, replace=TRUE))
  mmat <- model.matrix(~x1*x2*f)

  y <- mmat%*%(coefs) + rnorm(nobs, sd=sd.resid)
  
  return(data.frame(y, x1, x2, f))
}


gen_3x <- function(nobs=100L, means=c(0,0,0), sigma=diag(nrow=3,ncol=3), 
                   coefs=c(rep(1L, times=8)),
                   sd.resid=1) {
  stopifnot(length(nobs)==1, is.integer(nobs))
  stopifnot(length(means)==3, is.numeric(means))
  stopifnot(is.matrix(sigma), is.numeric(sigma))
  stopifnot(length(coefs)==8, is.numeric(coefs), !all(coefs==0))
  stopifnot(length(sd.resid)==1, is.numeric(sd.resid), sd.resid>0)
  
  require(MASS)
  
  X <- as.data.frame(mvrnorm(nobs, means, Sigma=sigma))
  x1 <- X[,1]; x2 <- X[,2]; x3 <- X[,3]
  
  mmat <- model.matrix(~x1*x2*x3)
  
  y <- mmat%*%(coefs) + rnorm(nobs, sd=sd.resid)
  
  return(data.frame(y, x1, x2, x3))
}
