# set.seed(20181111)
source("stdParm functions.r")

source("sim.poly.center.r")
source("kd.plot.overlay.r")

library(parallel)

cl <- makeCluster(8)

nvals <- 100
x <- seq(-1,2,length.out=nvals)
clusterExport(cl, c("x", "nvals", "sim.poly.center", "mean.to.matrix", "kron", "collect.terms"))
devnorms <- parSapply(cl, 1:100000, sim.poly.center, x, nvals)
rowMeans(devnorms)
kd.plot.overlay(t(devnorms), nvals)


nvals <- 1000
x <- seq(-1,2,length.out=nvals)
clusterExport(cl, c("x", "nvals", "sim.poly.center", "mean.to.matrix", "kron", "collect.terms"))
devnorms <- parSapply(cl, 1:100000, sim.poly.center, x, nvals)
rowMeans(devnorms)
kd.plot.overlay(t(devnorms), nvals)


nvals <- 10000
x <- seq(-1,2,length.out=nvals)
clusterExport(cl, c("x", "nvals", "sim.poly.center", "mean.to.matrix", "kron", "collect.terms"))
devnorms <- parSapply(cl, 1:100000, sim.poly.center, x, nvals)
rowMeans(devnorms)
kd.plot.overlay(t(devnorms), nvals)

stopCluster(cl)
