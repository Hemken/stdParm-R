source("R/poly.to.interaction.r")

terms <- c("I(x^2)","I(wt^2)","I(log(disp)","I(x^2+3)","I(x^5)", "x*y*z^2")

# polynomial terms, using I()
grepl("^I\\(.*\\^[[:digit:]]\\)$", terms) 


poly.to.interaction(terms)
