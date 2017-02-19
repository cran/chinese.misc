#' An Enhanced Version of as.character
#'
#' This function manages to coerce one or more objects into a character vector. Unlike 
#' \code{as.character}, this function can handle data frames, lists and recursive lists 
#' (lists of lists), even when there are factor objects inside data frames and lists. If there is any 
#' \code{NULL} object in a list, \code{as.character2} will coerce that element into 
#' \code{character(0)} rather than the character "NULL", which is what 
#' \code{as.character} does. When the object is of class matrix or data frame, the function 
#' will open it by column.
#'
#' @param ... one or more objects to be coerced.
#'
#' @return a character vector
#'
#' @export
#' @examples
#' as.character2(NULL, NULL)
#' # Try a list of NULLs
#' null_list <- list(a = NULL, b = NULL, c = NULL)
#' # Compare the different results of as.character 
#' # and as.character2. In fact, we usually 
#' # want the latter one.
#' as.character(null_list)
#' as.character2(null_list)
#' # Try a list with a data frame in it
#' df <- data.frame(matrix(c(66,77,NA,99), nr = 2))
#' l <- list(a = 1:4, b = factor(c(10,20,NA, 30)), c = c('x', 'y', NA, 'z'), d = df)
#' as.character2(l)
#' # Try a list of lists
#' l2 <- list(l, l, cha = c('a', 'b', 'c'))
#' as.character2(l2)
as.character2 <- function(...) {
  x <- list(...)
  if (is.null(x)) {
    final <- character(0)
  }
  else if (length(x) == 0) {
    final <- character(0)
  }
  else if (class(x)[1] == "data.frame") {
    final <- inner_from_df(x)
  }
  else if (class(x)[1] == "list") {
    final <- list.as.character(x)
  }
  else {
    final <- as.character(x)
  }
  if (is.null(final)){
    final <- character(0)
  }
  if (!is.vector(final)) 
    stop("Coersion failed.")
  names(final) <- NULL
  return(final)
}
