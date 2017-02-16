inner_from_df <-
function(xx) {
  yy <- c()
  for (i in 1:ncol(xx)) {
    yy <- append(yy, as.character(xx[, i]))
  }
  return(yy)
}
