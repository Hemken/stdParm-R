terms <- c("I(x^2)","I(wt^2)","I(log(disp)","I(x^2+3)","I(x^5)", "x*y*z^2")

grepl("I", terms)

grepl("\\(", terms)

grepl("\\^", terms)

grepl("[[:digit:]]", terms)

grepl("\\^[[:digit:]]", terms)

# polynomial terms, using I()
grepl("^I\\(.*\\^[[:digit:]]\\)$", terms) 

# as five sub-expressions, I(, varname, ^:digit)
polyregex   <- "^(I\\()(.*)(\\^)([[:digit:]])(\\))$"
prevar <- "^I\\("
postvar <- "\\^[[:digit:]]\\)$"
expt <- "(\\))$"
grepl(polyregex, terms) # polynomial terms, using I()

pterms <- terms[grepl(polyregex, terms)]
pvars <- sub(prevar, "", pterms)
pvars <- sub(postvar, "", pvars)

#pterms <- terms[grepl(polyregex, terms)]
pdegree <- sub("^I\\(.*\\^", "", pterms)
pdegree <- sub("\\)$", "", pdegree)

sapply(1:length(pterms), function(x) paste(rep(pvars[x], pdegree[x]), collapse=":"))

