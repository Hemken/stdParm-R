% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poly.fix.r
\name{poly.fix}
\alias{poly.fix}
\title{poly.fix}
\usage{
poly.fix(terms)
}
\arguments{
\item{terms}{a character vector of terms, possibly including
interaction terms}
}
\value{
a character vector of converted terms
}
\description{
Converts polynomial terms like I(x^2)
    to the interaction form x:x
}
\examples{
    poly.fix(c("Intercept", "x", "I(x^2)"))
    
}
