terms <- c("I(x^2)","I(wt^2)","I(log(disp)","I(x^2+3)","I(x^5)", "x*y*z^2")

grepl("I", terms)

grepl("\\(", terms)

grepl("\\^", terms)

grepl("[[:digit:]]", terms)

grepl("\\^[[:digit:]]", terms)

# polynomial terms, using I()
grepl("^I\\(.*\\^[[:digit:]]\\)$", terms) 

# as three sub-expressions, I(, varname, ^:digit)
grepl("^(I\\()(.*)(\\^[[:digit:]]\\))$", terms) # polynomial terms, using I()
