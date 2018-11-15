matching.terms <- function (findthese, inthose) {
  found <- findthese[findthese %in% inthose]
#  found.index <- (1:length(inthose))[findthese %in% inthose]
  tocheck <- findthese[!(findthese %in% inthose)]
  if (length(tocheck)>0){           # if tocheck
    needles <- vars.in.terms(tocheck)
#    notfound <- NULL
    haystack <- vars.in.terms(inthose)[,colnames(needles)]
    for (j in 1:nrow(needles)) {    # over needle rows
      for (k in 1:nrow(haystack)) { # over haystack rows
        if (identical(needles[j,], haystack[k,])) { # if in model
          found <- unique(c(found, rownames(needles)[j]))
#          found.index <- c(found.index,k)
        } # if in model
      }   # over haystack rows
    }     # over needle rows
  }       # if tocheck
  return(found)
}


# findthese <- colnames(Z.plus)
# inthose <- b.terms
# found <- findthese[findthese %in% inthose]
# 
# wherefound <- match(findthese, inthose)
# names(wherefound) <- 1:length(inthose)
# wherefound <- wherefound[!is.na(wherefound)]
# whichfound <- match(inthose, findthese)
# names(whichfound) <- 1:length(findthese)
# whichfound <- whichfound[!is.na(whichfound)]
# found.index <- (1:length(inthose))[findthese %in% inthose]
# 
# tocheck <- findthese[!(findthese %in% inthose)]
# 
# needles <- vars.in.terms(tocheck)
# haystack <- vars.in.terms(inthose)[,colnames(needles)]
# 
# for (j in 1:nrow(needles)) {    # over needle rows
#   print("checking")
#   print(needles[j,])
#   for (k in 1:nrow(haystack)) { # over haystack rows
#     if (identical(needles[j,], haystack[k,])) { # if in model
#       print("found!")
#       print(haystack[k,])
#       found <- unique(c(found, rownames(needles)[j]))
#       found.index <- c(found.index,k)
#       print(rbind(found,found.index))
#     } # if in model
#   }   # over haystack rows
# }     # over needle rows
