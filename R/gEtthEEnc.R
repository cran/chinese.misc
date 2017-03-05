#' @import Ruchardet
gEtthEEnc <- function(x1, x2) {
  if (x2 == "auto") {
    y <- tryCatch(expr = {
      inner_enc <- Ruchardet::detectFileEncoding(x1, 40)
    }, warning = function(w) {
      return("native.enc")
    })
  }
  else {
    y <- x2
  }
  return(y)
}
