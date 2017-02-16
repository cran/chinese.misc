As.chArActEr_in <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }
  else if (class(x)[1] == "list") {
    return(x)
  }
  else if (class(x)[1] == "data.frame") {
    return(inner_from_df(x))
  }
  else {
    return(as.character(x))
  }
}
