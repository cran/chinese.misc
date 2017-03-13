UsEstrIngI <-
function(x) {
    fsize <- file.info(x)$size
    BIN <- readBin(x, what = "raw", size = 1, n = fsize)
	y <- tryCatch(
		expr = {
			try_vec <- c("zh", "", NA)
			try_time <- 1
			still_wrong <- 1
			while (still_wrong == 1 & try_time < 4) {
				txt <- tryCatch(expr = {
					encoding <- stringi::stri_enc_detect2(BIN, locale = try_vec[try_time])[[1]]$Encoding[1]
					if (is.na(encoding)) {
						warning()
					} else {
						txt <- stringi::stri_encode(BIN, encoding, "UTF-8")
					}
				}, warning = function(w) {
					return(-1)
				})
				if (identical(txt, -1)) {
					try_time <- try_time + 1
				} else {
					still_wrong <- 0
				}
			}
			txt
		}, 
		error=function(e){
			return(-1)
		}
	)
    if (identical(y, -1)) {
       y <- stringi::stri_encode(BIN, "GB18030", "UTF-8")
    }
    return(y)
}
