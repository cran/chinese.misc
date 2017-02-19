#' Generate Corpus or Document Term Matrix with 1 Line
#'
#' This function allows you to input a vectoer of characters, or a mixture of files and folders, it 
#' will automatically detect file encodings, segment Chinese texts, 
#' do specified modification, 
#' remove stop words,  and then generate corpus or dtm (tdm). Since \pkg{tm} 
#' does not support Chinese well, this function manages to solve some problems. See Details.
#'
#' Package \pkg{tm} has two problems in creating Chinese document term matrix. First, 
#' it tries to segment an already segmented Chinese Corpus and put together terms that should 
#' not be put together. Second, if a term appears in the middle of a text and the end of the text, 
#' very occassionally it is taken as two different terms. The function is to deal with the problems.
#' It calls \code{\link{scancn}} to read files and auto-detect file encoding, 
#' and calls \code{\link[jiebaR]{segment}} to segment Chinese text, and finally 
#' calls \code{\link[tm]{Corpus}} to generate corpus, 
#' or \code{\link[tm]{DocumentTermMatrix}}, 
#' or \code{\link[tm]{TermDocumentMatrix}} to create dtm or tdm.
#'
#' Users should provide their jiebar cutter by \code{mycutter}. Otherwise, the function 
#' uses \code{DEFAULT_cutter} which is created when the package is loaded. 
#' The \code{DEFAULT_cutter} is simply \code{worker(write = FALSE)}.
#' See \code{\link[jiebaR]{worker}}.
#'
#' As long as 
#' you have not manually created another variable called "DEFAULT_cutter", 
#' you can directly use \code{jiebaR::new_user_word(DEFAULT_cutter...)} 
#' to add new words. By the way, whether you manually create an object 
#' called "DEFAULT_cutter", the original loaded DEFAULT_cutter which is 
#' used by default by functions in this package will not be removed by you.
#' So, whenever you want to use this default value, either you do not set 
#' \code{mycutter}, or set it to \code{mycutter = chinese.misc::DEFAULT_cutter}.
#'
#' By default, the argument \code{control} is set 
#' to \code{DEFAULT_control1}, which is created 
#' when the package is loaded. It allows words with length 1 to 25 to be placed in dtm or tdm.
#' Alternatively, \code{DEFAULT_control2} is also created 
#' when loading package, which sets 
#' word length to 2 to 25. When \code{control} is \code{NULL}, the function still points 
#' to the original value of \code{DEFAULT_control1}.
#'
#' You can create your own \code{DEFAULT_control1} or modify the originally 
#' loaded one, and you can remove them. However, in fact, the original one can 
#' neither be removed nor modified. 
#' So, whenever you want to use the original value, just do not set
#' \code{control}, or set it to {control = chinese.misc::DEFAULT_control1}.
#' The same is to \code{DEFAULT_control2}.
#' 
#'Whatever the control list is assigned to \code{control}, the function makes sure
#' that it never re-segments a segmented Chinese text.
#'
#' @param ... names of folders, files, or the mixture of the two kinds. It can also be a character 
#' vector of texts to be processed when setting \code{from} to "v", see below.
#' @param from should be "dir" or "v". If your inputs are filenames, it should be "dir" (default), 
#' If the input is a character vector of texts, it should be "v". However, if it is set to "v", 
#' make sure each element is not identical to filename in your working
#' directory; and, if they are identical, the function will raise an error. To do this check is 
#' because if they are identical, \code{segment} will take the input as a file to read! 
#' @param type what do you want for result. It is case insensitive, thus those can be transformed 
#' to "c", "cor", "corp", "corpus" represent a corpus 
#' result; and "dtm" for document term matrix, 
#' and "tdm" for term document matrix. See Details. Input other than the above represents 
#' a corpus result. The default value is "corpus".
#' @param enc a length 1 character specifying encoding when reading files. If your files 
#' may have different encodings, or you do not know their encodings, 
#' set it to "auto" (default) 
#' to let the function auto-detect encoding for each file.
#' @param mycutter the jiebar cutter to segment text. A default cutter is used. See Details.
#' @param stop_word a character vector to specify stop words that should be removed. 
#' If it is \code{NULL}, nothing is removed. If it is "jiebar", the stop words used by 
#' \pkg{jiebaR} are used, see \code{\link{make_stoplist}}.
#' Please note the default value is \code{NULL}. Texts are transformed to lower case before 
#' removing stop words, so your stop words only need to contain lower case characters.
#' @param stop_pattern vector of regular expressions. These patterns are similar to stop words. 
#' Terms that match the patterns will be removed.
#' @param control a named list to be passed to \code{DocumentTermMatrix} 
#' or \code{TermDocumentMatrix} to create dtm or tdm. Most of the time you do not need to 
#' set this value because a default value is used. When you set the argument to \code{NULL}, 
#' it still points to this default value. See Details.
#' @param myfun1 a function used to modify each text after being read by \code{scancn} 
#' and before being segmented. 
#' @param myfun2 a function used to modify each text after they are segmented.
#' @param special a length 1 character or regular expression to be passed to \code{dir_or_file} 
#' to specify what pattern should be met by filenames. The default is to read all files.
#' See \code{\link{dir_or_file}}.
#'
#' @return a corpus, or document term matrix, or term document matrix. 
#'
#' @export
#' @import tm
#' @import NLP
#' @examples
#' require(tm)
#' x <- c(
#'   "Hello, what do you want to drink?", 
#'   "drink a bottle of milk", 
#'   "drink a cup of coffee", 
#'   "drink some water")
#' # The simplest argument setting
#' dtm <- corp_or_dtm(x, from = "v", type = "dtm")
#' # Modify argument control to see what happens
#' dtm <- corp_or_dtm(x, from = "v", type="dtm", control = list(wordLengths = c(3, 20)))
#' dtm <- corp_or_dtm(x, from = "v", type = "dtm", stop_word = c("you", "to", "a", "of"))
corp_or_dtm <-
function(..., from = "dir", type = "corpus", enc = "auto", mycutter = DEFAULT_cutter, stop_word = NULL, stop_pattern = NULL, control = DEFAULT_control1,
  myfun1 = NULL, myfun2 = NULL, special = "") {
  message("CHECKING ARGUMENTS")
  if (!is.null(stop_word)) {
    stop_word <- as.character2(stop_word)
    stop_word <- stop_word[!is.na(stop_word)]
    if (length(stop_word) == 0) 
      stop_word <- NULL
  }
  if (!is.null(myfun1)) 
    stopifnot(is.function(myfun1))
  if (!is.null(myfun2)) 
    stopifnot(is.function(myfun2))
  if (!is.null(stop_pattern)) 
    stopifnot(all(!is.na(stop_pattern)))
  if (is.null(type)) 
    stop("Argument type should not be NULL.")
  type <- tolower(as.character(type[1]))
  if (type %in% c("dtm", "tdm")){
    control <- chEck_cOntrOl(control)
  }
  input <- c(...)
  if (!is_character_vector(input))
    stop("Your input should be characters.")
  input[is.na(input)] <- ""
  if (from == "dir") {
    message("PROCESSING FILE NAMES")
    fullname <- dir_or_file(input, special = special)
    partname <- gsub("^.*/", "", fullname)
    seged_vec <- rep(NA, length(fullname))
    message("READING AND PROCESSING FILES")
    for (i in 1:length(fullname)) {
      fi <- fullname[i]
      the_enc <- gEtthEEnc(x1 = fi, x2 = enc)
      conved <- scancn(fi, enc = the_enc)
      if (!is.null(myfun1) && grepl("[^[:space:]]", conved) == TRUE) {
        FUN1 <- match.fun(myfun1)
        conved <- FUN1(conved)
		conved <- AftEr_myfUn(conved)
      }	  
      if (!is.null(mycutter)) {
        conved <- paste(jiebaR::segment(conved, jiebar = mycutter), collapse = " ")
        conved <- gsub("\\s+", " ", conved)
      }
      if (!is.null(myfun2) && grepl("[^[:space:]]", conved) == TRUE) {
        FUN2 <- match.fun(myfun2)
        conved <- FUN2(conved)
		conved <- AftEr_myfUn(conved, pa = TRUE)
      }
      seged_vec[i] <- conved
    }
    message("GENERATING CORPUS")
    corp <- tm::Corpus(tm::VectorSource(seged_vec))
    names(corp) <- partname
    rm(seged_vec, fullname, partname)
  }
  if (from == "v") {
    input <- gsub("\\\\(t|r|s|n|)", " ", input)
	input <- gsub("\\s+$", "", input)
    if (any(file.exists(input))) {
      stop("Some strings are identical to filenames in working directory, please make some changes.")
    }
    message("PROCESSING CHARACTER VECTOR")
    seged_vec <- rep(NA, length(input))
    for (i in 1:length(input)) {
      ii <- input[i]
      if (ii %in% c(NA, "NA", "?") | grepl("[^[:space:]]", ii) == FALSE) {
        message("Element ", i, " may be blank or missing, set it to a space.")
        ii <- " "
      }
      if (is.function(myfun1) && grepl("[^[:space:]]", ii) == TRUE) {
        FUN1 <- match.fun(myfun1)
        ii <- FUN1(ii)
        ii <- AftEr_myfUn(ii)
	  }
      if (!is.null(mycutter)) {
        ii <- paste(jiebaR::segment(ii, mycutter), collapse = " ")
        ii <- gsub("\\s+", " ", ii)
      }
      if (is.function(myfun2) && grepl("[^[:space:]]", ii) == TRUE) {
        FUN2 <- match.fun(myfun2)
        ii <- FUN2(ii)
        ii <- AftEr_myfUn(ii, pa = TRUE)
      }
      seged_vec[i] <- ii
    }
    message("GENERATING CORPUS")
    corp <- tm::Corpus(tm::VectorSource(seged_vec))
    rm(seged_vec)
  }
  message("PROCESSING CORPUS")
  corp <- tm::tm_map(corp, tm::removePunctuation)
  corp <- tm::tm_map(corp, tm::removeNumbers)
  corp <- tm::tm_map(corp, tm::content_transformer(tolower))
  if (!is.null(stop_word)) {
    if (stop_word[1] == "jiebar") {
      corp <- tm::tm_map(corp, tm::removeWords, c(make_stoplist("jiebar")))
    }
    else {
      corp <- tm::tm_map(corp, tm::removeWords, c(stop_word))
    }
  }
  if (!is.null(stop_pattern)) {
    stop_pattern <- as.character2(stop_pattern)
    stop_pattern <- paste(stop_pattern, collapse = "|")
    corp <- tm::tm_map(corp, tm::removeWords, c(stop_pattern))
  }
  corp <- tm::tm_map(corp, tm::stripWhitespace)
  if (type %in% c("c", "cor", "corp", "corpus")) {
    return(corp)
  }
  else if (type %in% c("dtm", "tdm")) {
    message("MAKING ", type)
    corp <- tm::tm_map(corp, tm::content_transformer(zXzXz))
    if (type == "dtm") {
      DTMname <- tm::DocumentTermMatrix(corp, control = control)
    }
    else {
      DTMname <- tm::TermDocumentMatrix(corp, control = control)
    }
    rm(corp)
    last_pos <- which(DTMname$dimnames$Terms %in% c("zxvz", "zxvzxqzxj", "zxvzxqzxjzxv"))
    if (length(last_pos) != 0) 
      DTMname <- DTMname[, -last_pos]
    return(DTMname)
  }
  else {
    message("You do not specify a valid type, so return corpus.")
    return(corp)
  }
}
