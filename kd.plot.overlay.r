kd.plot.overlay <- function (X, nvals) {
  # X : a matrix
  stopifnot(is.matrix(X))
  ncols <- dim(X)[2]
  xlim <- c(min(X),max(X))
  # d1 <- density(X[,1])
  # d2 <- density(X[,2])
  # d3 <- density(X[,3])
  d <- apply(X,2,density)
  dy <- unlist(lapply(d,getElement,"y"))
  ylim <- c(min(dy,0),
            max(dy))
  
  plot(d[[1]], xlim=xlim, ylim=ylim, 
       main=paste("Norms (N=",nvals,")"))
  for (i in 2:ncols) {
    lines(d[[i]], col=i)
  }
}
