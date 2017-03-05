tryscAn <- function(x, the_enc_in, read_2nd_in){
  trytext <- tryCatch(
    expr = {
      text_in <- scan(x, what = "character", quiet = TRUE, sep = "\n", fileEncoding = the_enc_in)
      if (the_enc_in %in% c("utf-8", "UTF-8", "utf8") & identical(text_in, "?")){
        stop("rEAd_2nd")
      }
	  text_in
    }, 
    error = function(e){
      echar <- as.character(e)
      if (grepl("rEAd_2nd", echar)){
        return("rEAd_2nd")
      } else if (grepl("unsupported conversion", echar)){
        return("uns_UsEstrIngI")
      } else {
        stop(echar)
      }
    }, 
    warning = function(w){
      wchar <- as.character(w)
      if (grepl("invalid input found", wchar)){
        return("inv_UsEstrIngI")
      } else {
        return(text_in)
      }
    }
  )
  if (read_2nd_in){
    if (identical(trytext , "rEAd_2nd")){
      trytext  <- mUltIEncOdE(x)
	}
    if (identical(trytext , "uns_UsEstrIngI")|identical(trytext , "inv_UsEstrIngI")){
	  if (stringistatus == 1){
        trytext  <- UsEstrIngI(x)
	  }
    }
  } else {
    if (identical(trytext , "rEAd_2nd")|identical(trytext, "uns_UsEstrIngI")){
      trytext  <- " "
	} else {
	  trytext <- scan(x, what = "character", quiet = TRUE, sep = "\n", fileEncoding = the_enc_in)
    }
  }	
  return(trytext )
}
