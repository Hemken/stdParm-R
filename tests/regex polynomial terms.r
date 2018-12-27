source("R/poly.to.interaction.r")

terms <- c("I(x^2)","I(wt^2)","I(wt^2):disp","I(x^5)","mpg:I(wt^2)",
           "z:z","I(log(disp))","I(x^2+3)", "x*y*z^2")

# polynomial terms, using I()
grepl("^I\\(.*\\^[[:digit:]]\\)$", terms) 


# poly.to.interaction(terms)

poly.fix(terms)
