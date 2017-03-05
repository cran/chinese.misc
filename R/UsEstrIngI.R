UsEstrIngI <- function(x){
  fsize <- file.info(x)$size
  y <- readBin(x, what = "raw", size = 1, n = fsize)
  encoding <- stringi::stri_enc_detect2(y , NA)[[1]]$Encoding[1]
  if (!is.na(encoding)){
    y <- stringi::stri_encode(y, encoding, "UTF-8")
  } else {
    y <- stringi::stri_encode(y, stringi::stri_enc_get(), "UTF-8")
  }
  y <- unlist(stringi::stri_split_lines(y, omit_empty = TRUE))
  if (length(y) == 0)
    y <- ""
  y[is.na(y)] <- ""
  y[y == "NA"] <- ""
  return(y)
}
