#' Input a Filename and Return a Vector of Stop Words
#'
#' When a filename is provided, the function will return a vector of terms. If nothing is provided, 
#' it will return the stop words used in package \code{jiebaR}. See Details.
#'
#' In a valid text file that saves stop words, each word should occupy a single line. However, 
#' if any line that contains more than one word and these words are separated by blanks, 
#' punctuations, numbers, it is also accepted, for the function will try to split them.
#' Duplicated words will also be automatically removed.
#' The encoding of a stop words file is auto-detected by the function.
#'
#' For stop-word list from \code{jiebaR}, see \code{\link[jiebaR]{STOPPATH}}.  It contains 
#' many words that are often removed in analyzing Chinese text and some English
#' stop words. However, the result returned by \code{make_stoplist} is slightly different.
#'
#' @param x a length 1 character specifying a valid stop word file. If it is not provided or 
#' is "jiebar" (default), it will return part of the stop words used by package \code{jiebaR}.
#' See Details.
#'
#' @return a character vector of words. If no word is obtained, it will return \code{NULL}. 
#' After reading a file, the first six words will be automatically printed on the screen 
#' to help you make sure the words are obtained successfully.
#'
#' @export
make_stoplist <-
function(x = "jiebar") {
  if (length(x) > 1) {
    x <- x[1]
    message("x has length > 1, only the 1st is used.")
  }
  if (!identical(x, "jiebar")){
    if (!(file.exists(x) & ! dir.exists(x)))
      stop('x must be a valid filename.')
  } 
  if (x == "jiebar") {
    ST <- readLines(jiebaR::STOPPATH, encoding = Ruchardet::detectFileEncoding(jiebaR::STOPPATH))
    ST <- ST[-c(1:127, 137, 148:155, 878:882, 1180:1206, 1359, 1526:1534)]
  }
  else {
    the_enc <- Ruchardet::detectFileEncoding(x, n = 10)
    if (the_enc %in% c("windows-1252", "TIS-620")) 
      the_enc <- "GB18030"
    ST <- scan(x, what = "character", sep = "\n", quiet = TRUE, fileEncoding = the_enc)
    if (the_enc == "UTF-8" & identical(ST, "?")) {
      ST <- mUltIEncOdE(x)
    }
	ST <- gsub("[[:cntrl:]]\\d", "", ST)
  }
  ST <- ST[!is.na(ST) & ST != ""]
  ST <- unlist(strsplit(ST, "\\\\n|\\\\t|\\\\s|\\\\r"))
  ST <- unlist(strsplit(ST, "\\W+|\\d+"))
  ST <- ST[ST != "" & ST != "NA"]
  ST=unique(ST)
  if (length(ST) == 0) 
    ST <- NULL
  print(utils::head(ST))
  return(ST)
}
