#' Paste Words or Numbers with Quatation Marks
#' 
#' This function simply pastes things together and 
#' message them on the screen. Suppose you use 
#' \code{x=readClipboard()} and get "a", "b", "c", 
#' then, \code{CQUOTE(x)} will message 
#' \code{c("a", "b", "c")} on the screen.
#' 
#' @param x a character, numeric or factor vector.
#' @param quote whether to add quotation marks. 
#' Default is NULL, the function adds quotation 
#' marks to characters, not numbers. However, 
#' you can set it to TRUE or FALSE.
#' @param blank if \code{x} are characters, whether 
#' blank (i. e., "") is to be kept. Default is TRUE.
#'
#' @export
#' @examples
#' \donttest{
#' a=letters[1: 5]
#' CQUOTE(a) # c("a", "b", "c", "d", "e")
#' b=1: 5
#' CQUOTE(b) # c(1, 2, 3, 4, 5)#' CQUOTE(b, quote=TRUE) # c("1", "2", "3", "4", "5")
#' }
CQUOTE=function(x, quote=NULL, blank=TRUE){
	if (blank==FALSE) x=x[x != ""]
	if (is.null(quote)){
		res=if (is.character(x) | is.factor(x)) paste('c("', paste(x, collapse='", "'), '")', sep='') else paste('c(', paste(x, collapse=', '), ')', sep='')
	} else if (quote==TRUE){
		res=paste('c("', paste(x, collapse='", "'), '")', sep='')
	} else if (quote==FALSE){
		res=paste('c(', paste(x, collapse=', '), ')', sep='')
	}
	message(res)
}
