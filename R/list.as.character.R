list.as.character <- function(x) {
  nolist <- FALSE
  Y <- c()
  while (nolist == FALSE) {
    N <- c()
    for (i in 1:length(x)) {
      if (class(x[[i]])[1] == "list") {
        xi <- rapply(x[[i]], f = As.chArActEr_in, how = "replace")
        if (length(xi) != 0) {
          for (j in 1:length(xi)) {
          if (class(xi[[j]])[1] == "list") {
            N <- append(N, xi[[j]])
          }
          else {
            Y <- append(Y, xi[[j]])
          }
          }
        }
      }
      else {
        Y <- append(Y, As.chArActEr_in(x[[i]]))
      }
    }
    if (length(N) == 0) {
      nolist <- TRUE
    }
    else {
      x <- N
    }
  }
  rm(x)
  return(unlist(Y))
}
