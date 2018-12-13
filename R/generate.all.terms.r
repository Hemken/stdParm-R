generate.all.terms <- function (tvd) {
  # tvd - terms.vars.degrees return object
  
  maxdegrees <- apply(tvd, 2, max)
  all.terms <- poly.lower.fill(maxdegrees)
  
  generators <- row.names(tvd)[attr(tvd, "order") >=2]
  
  for (i in 1:length(generators)){
    print(generators[i])
    tvdrow <- tvd[generators[i],]
    tvdrow <- tvdrow[tvdrow>0]
    print(tvdrow)
    vars <- names(tvdrow)
    for (j in 2:length(vars)){
      all.terms <- unique(c(all.terms, pastex(combn(vars, j))))
    }
  }
  return(all.terms)
}
