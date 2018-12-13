source("stdParm functions.r")

source("gen_ex_models.r")
source("errors/sim.3var.center.r")
source("errors/kd.plot.overlay.r")

library(parallel)

cl <- makeCluster(8)
fcns <- c("nvals", "sim.3var.center", "gen_3x", 
          "mean.to.matrix", "matching.terms", "terms.vars.degrees", 
          "kron", "clean.kron.names", "recentering.matrix")
nvals <- 100L
clusterExport(cl, fcns)
devnorms <- parSapply(cl, 1:100000, sim.3var.center, nvals)
rowMeans(devnorms)
kd.plot.overlay(t(devnorms), nvals)

nvals <- 1000L
clusterExport(cl, fcns)
devnorms <- parSapply(cl, 1:100000, sim.3var.center, nvals)
rowMeans(devnorms)
kd.plot.overlay(t(devnorms), nvals)

nvals <- 10000L
clusterExport(cl, fcns)
devnorms <- parSapply(cl, 1:100000, sim.3var.center, nvals)
rowMeans(devnorms)
kd.plot.overlay(t(devnorms), nvals)

stopCluster(cl)

