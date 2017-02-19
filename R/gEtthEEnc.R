#' @import Ruchardet
gEtthEEnc <- function(x1, x2) {
  if (x2 == "auto") {
    y <- tryCatch(expr = {
      inner_enc <- Ruchardet::detectFileEncoding(x1, 15)
    }, warning = function(w) {
      return("native.enc")
    })
    if (y %in% c("windows-1252", "TIS-620")) {
      y <- "GB18030"
    }
  }
  else {
    y <- x2
  }
  return(y)
}
