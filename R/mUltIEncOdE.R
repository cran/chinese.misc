#' @import stringi
mUltIEncOdE <-
function(x) {
  y <- scan(x, what = "character", fileEncoding = "", quiet = TRUE, sep = "\n")
  return(stringi::stri_encode(y, "UTF-8", "GB18030"))
}
