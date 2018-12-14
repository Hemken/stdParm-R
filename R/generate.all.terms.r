generate.all.terms <- function (tvd) {
  # tvd - terms.vars.degrees return object
  
  all.terms <- "(Intercept)"
  
  maxdegrees <- apply(tvd, 2, max)
  all.terms <- unique(c(all.terms, poly.lower.fill(maxdegrees)))
  # print(all.terms)
  generators <- row.names(tvd)[attr(tvd, "order") >=2]
  nhigher <- length(generators)
  if (nhigher > 0) {
    for (i in 1:nhigher){
      print(generators[i])
      tvdrow <- tvd[generators[i],]
      tvdrow <- tvdrow[tvdrow>0]
      print(tvdrow)
      vars <- names(tvdrow)
      for (j in 2:length(vars)){
        all.terms <- unique(c(all.terms, pastex(combn(vars, j))))
      } # for vars in generator
    } # for generators
    
  } # if nhigher
  return(all.terms)
}
