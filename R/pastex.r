#' pastex
#' 
#' @description Paste together the rows of a matrix, separated by a colon
#' 
#' @param mat a matrix
#' 
#' @return a character vector
#' 
#' @examples 
#'     A <- matrix(letters[1:6], ncol=3)
#'     pastex(A)
#'     

pastex <- function (mat) {
  apply(mat, 2, paste, collapse=":")
}
