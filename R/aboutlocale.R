localestart2 <- function(){
	y <- tryCatch(
		expr = {
			s1 <- Sys.getlocale("LC_COLLATE")
			s2 <- Sys.getlocale("LC_CTYPE")
			if (s1 == "Chinese (Simplified)_China.936" & s2 == "Chinese (Simplified)_China.936"){
				yy <- "n"
			} else {
				Sys.setlocale(category = "LC_COLLATE", "Chinese (Simplified)_China.936")
				Sys.setlocale(category = "LC_CTYPE", "Chinese (Simplified)_China.936")
				yy <- c("y", s1, s2)
			}
			yy
		},
		error = function(e){
			return("n")
		}
	)
	return(y)
}

localeend2 <- function(x){
	tryCatch(
		expr = {
			if (x[1] == "y"){
				Sys.setlocale(category = "LC_COLLATE", x[2])
				Sys.setlocale(category = "LC_CTYPE", x[3])
			}
		},
		error = function(e){
			message(" ")
		}
	)
}

whetherencode <- function(x){
	y <- tryCatch(
		expr = {
			xx <- x
			de <- suppressWarnings(Ruchardet::detectEncoding(x))
			if (!de %in% c("UTF-8", "utf-8")){
				xx <- stringi::stri_encode(xx, to = "UTF-8")
			}
			xx
		},
		error = function(e){
			return(-999999)
		},
		warning = function(w){
			return(-999999)
		}
	)
	if (identical(y, -999999))
		y <- x
	return(y)
}
