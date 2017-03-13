tryscAn <-
function(x, the_enc_in, read_2nd_in){
  trytext <- tryCatch(
    expr = {
      text_in <- scan(x, what = "character", quiet = TRUE, sep = "\n", fileEncoding = the_enc_in)
      if (the_enc_in %in% c("utf-8", "UTF-8", "utf8") & identical(text_in, "?")){
        text_in <- "rEAd_2nd"
      } else if (grepl("^gb|^GB", the_enc_in) & identical(text_in, "?")){
        text_in <- "rEAd_2nd"
	  }
	  text_in <- whetherencode(text_in)
    }, 
    error = function(e){
      echar <- as.character(e)
	  if (grepl("unsupported conversion", echar)){
        return("rEAd_2nd")
      } else {
        stop(echar)
      }
    }, 
    warning = function(w){
    	return("rEAd_2nd")
	}
  )
  if (identical(trytext, "rEAd_2nd")){
	if (read_2nd_in){
		if (stringistatus == 1){
			trytext <- UsEstrIngI (x)
		} else {
		    fsize <- file.info(x)$size
			BIN <- readBin(x, what = "raw", size = 1, n = fsize)
			trytext <- stringi::stri_encode(BIN, "GB18030", "UTF-8")
        }
	} else {
		trytext <- "?"
	}
  }
  return(trytext )
}
