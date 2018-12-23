match.terms <- function (findthese, inthose) {
  # A function to find proposed terms in a target
  #    list of terms
  #
  # Arguments:
  #   findthese: proposed terms to find
  #   inthose:   target terms to search
  #
  # Value:
  #   found:    proposed terms found in target
  #   found.as: form of found term
  #   found.at: position found in target
  stopifnot(is.character(findthese),is.character(inthose))

  needles <- terms.vars.degrees(findthese)
  haystack <- terms.vars.degrees(inthose)

  matchcols <- match(colnames(haystack), colnames(needles))
  # print(matchcols) # which haystack variables?
  dropped <- is.na(match(colnames(needles), colnames(haystack)))
  # print(dropped) # needle variables not in the haystack
  extra <- as.matrix(haystack[,is.na(matchcols)])
  # print(extra) # part of the haystack not searched
  haystack <- haystack[, !is.na(matchcols)]
  # print(haystack) # part of the haystack to search
  
  haystack <- haystack[, colnames(needles)[!dropped]]
    # print(haystack)
    found <- NULL
    found.as <- NULL
    found.at <- NULL
    for (j in 1:nrow(needles)) {    # over needle rows
      for (k in 1:nrow(haystack)) { # over haystack rows
        if (identical(needles[j,], haystack[k,])) { # if in model
          if (all(extra[k,]==FALSE)) { # if first order
            found <- unique(c(found, rownames(needles)[j]))
            found.as <- unique(c(found.as,rownames(haystack)[k]))
            found.at <- unique(c(found.at,k))
          } # if first order
      }   # over haystack rows
    }     # over needle rows
    
  }

  return(list(found=found, found.as=found.as, found.at=found.at))
}
