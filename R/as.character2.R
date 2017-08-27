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
#' df <- data.frame(matrix(c(66,77,NA,99), nrow = 2))
#' l <- list(a = 1:4, b = factor(c(10,20,NA, 30)), c = c('x', 'y', NA, 'z'), d = df)
#' as.character2(l)
#' # Try a list of lists
#' l2 <- list(l, l, cha = c('a', 'b', 'c'))
#' as.character2(l2)
as.character2 <- function(...) {
  X <- list(...)
  lengthX <- length(X)  
  if (lengthX == 0){
	FINAL <- character(0)
  } else {
    FINAL <- unlist(lapply(1: lengthX, EAch_chA_fInAl, object = X))
  }	
  if (is.null(FINAL)){
	FINAL <- character(0)
  }
  if (!is.vector(FINAL)) 
    stop("Coersion failed.")
  names(FINAL) <- NULL
  return(FINAL)
}

as.character2_2 <- function(...) {
  X <- list(...)
  if (length(X) == 0){
	FINAL <- character(0)
  } else {
    FINAL <- c()
    for (i in 1:length(X)){
      x <- X[[i]]
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
      else if (class(x)[1] == "SimpleCorpus") {
	    x$meta$language <- NULL
    	final <- x$content
      }	  
      else {
    	final <- as.character(x)
      }
      FINAL <- append(FINAL, final)
    }
  }  
  if (is.null(FINAL)){
	FINAL <- character(0)
  }
  if (!is.vector(FINAL)) 
    stop("Coersion failed.")
  names(FINAL) <- NULL
  return(FINAL)
}

EAch_chA_fInAl <- function(i, object){
  obj <- object[[i]]
  if (is.null(obj)) {
	return(character(0))
  }
  else if (length(obj) == 0) {
	return(character(0))
  }
  else if (class(obj)[1] == "data.frame") {
	return(inner_from_df(obj))
  }
  else if (class(obj)[1] == "list") {
	return(list.as.character(obj))
  }
  else if (class(obj)[1] == "SimpleCorpus") {
    obj$meta$language <- NULL
	return(obj$content)
  }	  
  else {
	return(as.character(obj))
  }
}
