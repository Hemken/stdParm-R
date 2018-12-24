#' recentering.matrix
#' 
#' Recentering and rescaling matrix generation
#' 
#' @param x named vector of recentering or rescaling constants, one
#'     per variable in \code{terms}
#' @param terms character vector of terms (coefficients) in a model
#' @param type the type of matrix to return, either \code{center} 
#'     (the default) or \code{scale}
#'     
#' @return a recentering or rescaling matrix
#' 
#' @examples 
#'     fit <- lm(mpg ~ wt*disp, data=mtcars)
#'     mu    <- colMeans(mtcars[,c("wt", "disp")])
#'     sigma <- colSds(mtcars[,c("wt", "disp")])
#'     terms <- names(coef(fit))
#'     
#'     recentering.matrix(mu, terms)
#'     recentering.matrix(mu, terms, type="scale")
#'     
#' @export
#' 
recentering.matrix <- function(x, terms, type="center") {
  stopifnot(is.numeric(x), is.character(terms),
            type %in% c("center", "scale"))
  nx <- length(x)
  varnames <- names(x)
  if (is.null(varnames)) stop("recentering/rescaling constants must have names")
  complete.terms <- generate.all.terms(terms.vars.degrees(terms))
  
  C <- matrix(1, ncol=1)
  colnames(C) <-rownames(C) <- "(Intercept)"
  
  for (i in 1:nx){ # over means
    xc <- x[i]
    names(xc) <- varnames[i]
    if (type=="center") { # centering or scaling matrix
      A <- polyterm(xc, complete.terms)
      C <- kron(A, C)
    } else if (type=="scale") {
      A <- polyterm(xc, complete.terms, type="scale")
      C <- kron(A, C)
    }                    # centering or scaling matrix
    
    matched <- match.terms(colnames(C), complete.terms)
    found <- matched$found
    C <- C[,found]
    C <- C[rowSums(C)>0, ] 
  }         # over means
  C <- C[,terms]
  C <- C[rowSums(C)>0, ] 
  return(C)
}
