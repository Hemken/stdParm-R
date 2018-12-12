generate.all.terms <- function (tvd) {
  # tvd - terms.vars.degrees return object
  
  all.terms <- "(Intercept)"
  all.terms <- c(all.terms, colnames(tvd))
  
  generators <- row.names(tvd)[attr(tvd, "order") >=2]
  
  for (i in 1:length(generators)){
    # print(generators[i])
    tvdrow <- tvd[generators[i],]
    tvdrow <- tvdrow[tvdrow>0]
    vars <- names(tvdrow)
    for (j in 2:length(vars)){
      all.terms <- unique(c(all.terms, pastex(combn(vars, j))))
    }
  }
  return(all.terms)
}
