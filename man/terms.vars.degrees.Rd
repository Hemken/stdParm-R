% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/terms.vars.degrees.r
\name{terms.vars.degrees}
\alias{terms.vars.degrees}
\title{terms.vars.degrees}
\usage{
\method{terms}{vars.degrees}(terms)
}
\arguments{
\item{terms}{a character vector of model terms/coefficients}
}
\value{
a matrix with the terms as \code{rownames},
    the constituent variables as \code{colnames}, and
    the polynomial degree of each variable in each term
    as the table cell. The matrix also has an \code{order}
    attribute, recording the interaction order of each
    term.
}
\description{
Generate a terms-variables-degrees table/matrix
}
\examples{
    fit <- lm(mpg ~ wt*disp, data=mtcars)
    terms <- names(coef(fit))
    terms.vars.degrees(terms)
    
}
\seealso{
\code{terms.object}, \code{terms}, \code{formula}
}
