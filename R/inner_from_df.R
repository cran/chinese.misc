inner_from_df <-
function(xx) {
  return(as.vector(apply(xx, 2, as.character)))
}
