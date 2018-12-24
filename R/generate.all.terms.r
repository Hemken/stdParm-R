#' generate.all.terms
#' 
#' Generate a complete list of terms where some lower order terms
#'     may have been dropped
#' 
#' @param tvd a terms/variables/degrees table, see \code{terms.vars.degrees}
#' 
#' @return a character vector of model terms
#' 
#' @examples
#'     fit <- lm(mpg ~ 0 + wt*disp, data=mtcars)
#'     generate.all.terms(terms.vars.degrees(names(coef(fit))))
#'
generate.all.terms <- function (tvd) {

  all.terms <- "(Intercept)"
  maxdegrees <- apply(tvd, 2, max)
  all.terms <- unique(c(all.terms, unlist(poly.lower.fill(maxdegrees))))
  generators <- row.names(tvd)[attr(tvd, "order") >=2]
  nhigher <- length(generators)
  if (nhigher > 0) {
    for (i in 1:nhigher){
      tvdrow <- tvd[generators[i],]
      tvdrow <- tvdrow[tvdrow>0]
      all.terms <- unique(c(all.terms,
                            pastex(apply(
                            expand.grid(
                            poly.lower.fill(tvdrow)),1,paste, sep=":"))))
    } # for generators
  } # if nhigher
  return(all.terms)
}
