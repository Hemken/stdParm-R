sim.poly.center <- function (i,x, nvals) {
  y <- x^3 - 2*x^2 +0.5 + rnorm(nvals)
  model1 <- lm(y ~ x + I(x^2) + I(x^3))
  
  px <- poly(x,3)
  colnames(px) <- c("x", "x:x", "x:x:x")
  model2 <- lm(y ~ px)
  
  d <- x - mean(x)
  model3 <- lm(y ~ d + I(d^2) + I(d^3))
  
  mu <- mean(x)
  names(mu) <- "x"
  A <- mean.to.matrix(mu) # recentering x - 3
  C <- kron(A,A)
  C <- kron(A,C)
  
  # clean extra columns, collapse extra rows
  C <- collect.terms(C)
  
  b <- coef(model1)
  recentered <- C%*%b
  y4 <- as.matrix(data.frame(1,d,d^2,d^3))%*%(C%*%b)
  
  y1 <- predict(model1)
  y2 <- predict(model2)
  y3 <- predict(model3)
  
  return(c(sqrt(sum((y1-y2)^2)),  # poly
           sqrt(sum((y1-y3)^2)),  # recenter data
           sqrt(sum((y1-y4)^2)))) # recenter coefs
}
