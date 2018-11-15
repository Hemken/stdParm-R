order.terms <- function (findthese, inthose) {
  needles <- vars.in.terms(findthese)
  haystack <- vars.in.terms(inthose)
  matchcols <- match(colnames(haystack), colnames(needles))
  extra <- as.matrix(haystack[,is.na(matchcols)])
  haystack <- haystack[, !is.na(matchcols)]
  haystack <- haystack[, colnames(needles)]
  found <- NULL
  found.as <- NULL
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
        } # if first order
      } # if in model
    }   # over haystack rows
    # print("")
  }     # over needle rows
  return(list(found=found, found.as=found.as))
}
