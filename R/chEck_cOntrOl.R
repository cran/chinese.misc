chEck_cOntrOl <- function(x) {
  inner_token <- NLP::as.Token_Tokenizer(NLP::Regexp_Tokenizer("\\s", invert = TRUE))
  if (is.null(x)) {
    x <- list(wordLengths = c(1, 25), tokenizer = inner_token)
  }
  else if (class(x) != "list") {
    stop("control must be a list or NULL.")
  }
  else {
    namescontrol <- names(x)
    if (is.null(namescontrol)) 
      stop("Elements of control must have names.")
    if (!all(namescontrol %in% c("bounds", "wordLengths", "dictionary", "tokenizer", "weighting", "stopwords"))) 
      stop("List names must be among bounds, wordLengths, dictionary, tokenizer, weighting, stopwords.")
    x$tokenizer <- inner_token
    if ("dictionary" %in% namescontrol) {
      max_nchar <- max(nchar(x$dictionary))
      x$wordLengths <- c(1, max_nchar)
    }
    else {
      if (!"wordLengths" %in% namescontrol) 
        x$wordLengths <- c(1, 25)
    }
  }
  return(x)
}