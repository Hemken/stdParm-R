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
  needles <- vars.in.terms(findthese)
  haystack <- vars.in.terms(inthose)
  matchcols <- match(colnames(haystack), colnames(needles))
  extra <- as.matrix(haystack[,is.na(matchcols)])
  haystack <- haystack[, !is.na(matchcols)]
  haystack <- haystack[, colnames(needles)]
  found <- NULL
  found.as <- NULL
  found.at <- NULL
  for (j in 1:nrow(needles)) {    # over needle rows
    # print(paste("checking",j))
    # print(needles[j,])
    for (k in 1:nrow(haystack)) { # over haystack rows
      if (identical(needles[j,], haystack[k,])) { # if in model
        # print(paste("found!", k))
        # print(haystack[k,])
        # print(extra[k,])
        if (all(extra[k,]==FALSE)) { # if first order
          found <- unique(c(found, rownames(needles)[j]))
          found.as <- unique(c(found.as,rownames(haystack)[k]))
          found.at <- unique(c(found.at,k))
        } # if first order
      } # if in model
    }   # over haystack rows
    # print("")
  }     # over needle rows
  return(list(found=found, found.as=found.as, found.at=found.at))
}
