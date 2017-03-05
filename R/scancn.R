#' Read a Text File by Auto-Detecting Encoding
#'
#' The function reads a text file and tries to detect file encoding. If you have Chinese files from 
#' different sources and cannot give them a single encoding, just let this function detect and 
#' read them. The function can save you much time on dealing with unrecognizable characters. 
#'
#' The function calls \code{scan(x, what= "character", ...)} and auto-detects file 
#' encoding. Sometimes 
#' a Chinese file is encoded in "UTF-8", but what is actually read is a "?". When this happens, 
#' the function reads it twice and uses \code{\link[stringi]{stri_encode}} to convert it.
#' If invalid inputs are found in the content, the file will also be read twice.
#'
#' The function always returns a length 1 character. If the return of \code{scan} is a vector 
#' with length larger than 1, 
#' elements will be pasted together with three spaces. 
#'
#' It will return 
#'  a " " (one space) when all the elements of the vector are \code{NA}.
#' If not all elements 
#' are \code{NA}, those equal to \code{NA} will be changed to "" (a size 0 string) before being 
#' pasted together.
#'
#' @param x a length 1 character specifying filename.
#' @param enc a length 1 character of file encoding specified by user. The default is "auto", which 
#' means let the function detect encoding.
#' @param read_2nd should be \code{TRUE} (default) or \code{FALSE}. When it is \code{TRUE}, 
#' some files will be read twice, see Details.
#'
#' @return a length 1 character of text.
#'
#' @export
#' @examples
#' # No Chinese is allowed, so try an English file
#' x <- file.path(find.package("base"), "CITATION")
#' scancn(x)
scancn <-
function(x, enc = "auto", read_2nd = TRUE) {
  if (length(x) > 1) {
    x <- x[1]
    message("x has length > 1, only the 1st is used.")
  }
  if (!read_2nd %in% c(TRUE, FALSE)) 
    stop("read_2nd must be TRUE or FALSE.")
  the_enc <- gEtthEEnc(x1 = x, x2 = enc)
  text <- tryscAn(x = x, the_enc_in = the_enc, read_2nd_in = read_2nd)
  text <- gsub("[[:cntrl:]]\\d", "", text)
  if (all(is.na(text)) | identical(text, "?")) {
    text <- " "
  }
  text[is.na(text)] <- ""
  text <- paste(text, collapse = "   ")
  text <- gsub("\\n", " ", text)
  text <- gsub("\\\\(t|r|n|)", "", text)
  if (!grepl("\\w", text)){
    text <- " "
    message(x, " is blank.")	
  }
  return(text)
}
