sim.3var.center <- function (i, nvals) {
  df <- gen_3x(nvals, 
               means=c(-0.5,0,0.5),
               sigma=matrix(c(1,.5,.25,.5,1,.3,.25,.3,1), ncol=3),
               coefs=c(1,2,0,-2,.75,.5,.25,0))
  
  model1 <- lm(y~x1*x2*x3, data=df)
  
  b <- coef(model1)
  b.terms <- names(b)
  
  df2 <- df
  df2[,2:4] <- apply(df[,2:4],2,scale, scale=FALSE)
  model2 <- lm(y~x1*x2*x3, data=df2)
  
  x.means <- colMeans(df[,2:4])
  C <- matrix.build.clean(x.means, b.terms)
  
  y1 <- predict(model1) # original
  y2 <- predict(model2) # recentered data
  y3 <- cbind(rep(1,nvals), as.matrix(df2[2:4]),
              df2[["x1"]]*df2[["x2"]],df2[["x1"]]*df2[["x3"]],df2[["x2"]]*df2[["x3"]],
              df2[["x1"]]*df2[["x2"]]*df2[["x3"]])%*%(C%*%b)
  
  return(c(sqrt(sum((y1-y2)^2)),  # recenter data
           sqrt(sum((y1-y3)^2))))        # recenter coefs
}
