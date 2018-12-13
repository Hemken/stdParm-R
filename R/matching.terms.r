matching.terms <- function (findthese, inthose) {
  found <- findthese[findthese %in% inthose]
  tocheck <- findthese[!(findthese %in% inthose)]
  if (length(tocheck)>0){           # if tocheck
    needles <- terms.vars.degrees(tocheck)
    haystack <- terms.vars.degrees(inthose)[,colnames(needles)]
    for (j in 1:nrow(needles)) {    # over needle rows
      for (k in 1:nrow(haystack)) { # over haystack rows
        if (identical(needles[j,], haystack[k,])) { # if in model
          found <- unique(c(found, rownames(needles)[j]))
        } # if in model
      }   # over haystack rows
    }     # over needle rows
  }       # if tocheck
  return(found)
}


