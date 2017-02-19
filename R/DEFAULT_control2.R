#' A Default Value for corp_or_dtm 2
#'
#' This list object is by default called by \code{\link{corp_or_dtm}}.
#'
#' The object specifies word length from 2 to 
#' 25, and takes a regular expression as tokenizer. Also, \code{DEFAULT_control1}
#' sets lengh from 1 to 25.
#'
#' @export
#' @examples
#' require(tm)
#' x <- c(
#'   "Hello, what do you want to drink?", 
#'   "drink a bottle of milk", 
#'   "drink a cup of coffee", 
#'   "drink some water")
#' dtm <- corp_or_dtm(x, from = "v", type = "dtm", control = DEFAULT_control2)
DEFAULT_control2<- list(wordLengths = c(2, 25), tokenizer = NLP::as.Token_Tokenizer(NLP::Regexp_Tokenizer("\\s", invert = TRUE)))
