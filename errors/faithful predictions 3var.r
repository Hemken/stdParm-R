source("stdParm functions.r")
source("gen_ex_models.r")

source("sim.3var.center.r")
source("kd.plot.overlay.r")

library(parallel)

cl <- makeCluster(8)

nvals <- 100L
clusterExport(cl, c("nvals", "sim.3var.center", "gen_3x", 
                    "mean.to.matrix", "matching.terms", "vars.in.terms", "kron", "matrix.build.clean"))
devnorms <- parSapply(cl, 1:100000, sim.3var.center, nvals)
rowMeans(devnorms)
# kd.plot.2overlay(t(devnorms))
kd.plot.overlay(t(devnorms), nvals)

nvals <- 1000L
clusterExport(cl, c("nvals", "sim.3var.center", "gen_3x", 
                    "mean.to.matrix", "matching.terms", "vars.in.terms", "kron", "matrix.build.clean"))
devnorms <- parSapply(cl, 1:100000, sim.3var.center, nvals)
rowMeans(devnorms)
# kd.plot.2overlay(t(devnorms), nvals)
kd.plot.overlay(t(devnorms), nvals)

nvals <- 10000L
clusterExport(cl, c("nvals", "sim.3var.center", "gen_3x", 
                    "mean.to.matrix", "matching.terms", "vars.in.terms", "kron", "matrix.build.clean"))
devnorms <- parSapply(cl, 1:100000, sim.3var.center, nvals)
rowMeans(devnorms)
# kd.plot.2overlay(t(devnorms), nvals)
kd.plot.overlay(t(devnorms), nvals)

stopCluster(cl)

