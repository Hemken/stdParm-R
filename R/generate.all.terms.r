generate.all.terms <- function (tvd) {
  # tvd - terms.vars.degrees return object/table
  # returns a (completed) vector of terms
  
  all.terms <- "(Intercept)"
  # print(all.terms)
  maxdegrees <- apply(tvd, 2, max)
  # print(maxdegrees)
  all.terms <- unique(c(all.terms, unlist(poly.lower.fill(maxdegrees))))
  # print(all.terms)
  generators <- row.names(tvd)[attr(tvd, "order") >=2]
  nhigher <- length(generators)
  if (nhigher > 0) {
    for (i in 1:nhigher){
      # print(generators[i])
      tvdrow <- tvd[generators[i],]
      tvdrow <- tvdrow[tvdrow>0]
      # print(tvdrow)
      # print(pastex(apply(expand.grid(poly.lower.fill(tvdrow)),1,paste, sep=":")))
      all.terms <- unique(c(all.terms,
                            pastex(apply(
                              expand.grid(
                                poly.lower.fill(tvdrow)),1,paste, sep=":"))))
      # vars <- names(tvdrow)
      # # print(vars)
      # for (j in 2:length(vars)){
      #   all.terms <- unique(c(all.terms, pastex(combn(vars, j))))
      # } # for vars in generator
    } # for generators
    
  } # if nhigher
  return(all.terms)
}
