#' Write Texts in CSV into Many TXT/RTF Files
#'
#' The function writes texts in a given .csv file into seperated .txt/.rtf files with file names added.
#'
#' Arguments \code{which} and \code{name_col} should be set in accordance 
#' with \code{row.names}. Suppose your .csv file has 3 columns, the 1st column is for row names, 
#' the 2nd for filenames, the 3rd for fulltexts. In so far as the 1st column is for row names 
#' and \code{row.names} should be 1, your data frame only has two columns, and the 
#' fulltexts are in the 2nd column, so set \code{which} to 2 (not 3), and \code{name_col}
#' should be 1. If the .csv file do not have row names and its 1st column is for filenames, 2nd 
#' column for fulltexts, then \code{which} should be 2, and \code{name_col} is 1.
#' 
#' In writing .txt/.rtf files, the function gives each file a unique number as part of its filename. The 
#' mechanism is as follows: suppose you have 1234 files, as this number has four digits, a 
#' series of numbers 0001, 0002,...0012,...0300,...1234 are assigned rather 
#' than 1, 2,...12,...300,...1234. There are several reasons to do this: first, if \code{name_col} 
#' is \code{NULL}, this procedure automatically assigns names. Second, the column you 
#' specify may have duplicate names. Third, even the column does not have duplicate names, 
#' the process the function modifies the names to make them valid may 
#' also produce duplicate names. Fourth, numbers with full digits make it easy to sort them 
#' in any software.
#'
#' @param csv a .csv file. One of its columns contains texts to be written.
#' @param folder a name of a folder that stores the .txt/.rtf files 
#' created by the function. The folder may 
#' already exist. If it does not exist, the function will try to create it recursively. If it cannot 
#' be created, an error will be raised. See \code{\link{dir.create}}. Note: a name that contains 
#' no punctuation is prefered.
#' @param which a number: which column contains texts. See Details
#' @param header should the .csv file be read with its first row as header? This argument is 
#' passed to \code{\link{read.csv}}. Default is \code{TRUE}. 
#' @param row.names should only be \code{NULL} or 1. This means whether your .csv file 
#' has no column for row names, or row names are in the 1st column.
#' it will be passed to \code{\link{read.csv}}. See Details.
#' @param na_in_csv character vector indicating what content in the .csv file's cells should be 
#' taken as \code{NA}. The default values are "", " ", "?", "NA", "999"; and you can 
#' specify other values. But whatever you specify, the default values will always be taken 
#' as \code{NA}. If you do not provide a character vector, the default values are used.
#' @param na_in_txt a length 1 character specifying what to write into a .txt file if 
#' a csv cell is \code{NA}. The default is " " (a space).
#' @param name_col a length 1 number to indicate which column of your data should be taken 
#' as filenames. If it is \code{NULL} (default), a unique number will be given to each file, 
#' See Detail. If a cell is taken to be \code{NA}, it will be converted to ""; if it is too long, 
#' only the first 90 characters are used; one or more blanks and punctuations 
#' will be replaced by " " (a space).
#' @param ext the extension of files to be written. Should be "txt", "rtf" or "". 
#' If it is not one of the three, it is set to "".
#'
#' @return nothing is returned and .txt/rtf files are written into the folder.
#'
#' @export
#' @examples
#' \dontrun{
#' # First, we create a csv file
#' x1 <- file.path(find.package("base"), "CITATION")
#' x2 <- file.path(find.package("base"), "DESCRIPTION")
#' txt2csv(x1, x2, must_txt = FALSE, csv = "x1x2csv.csv")
#' # Now try to write files
#' wd <- getwd()
#' wd <- gsub("/$|\\\\$", "", wd)
#' f <- paste(wd, "x1x2csv", sep="/")
#' csv2txt(csv = "x1x2csv.csv", folder = f, which = 2, row.names = 1, ext = "")
#' }
csv2txt <-
function(csv, folder, which, header = TRUE, row.names = NULL, na_in_csv = c(NA, "", " ", "?", "NA", "999"), na_in_txt = " ", name_col = NULL, ext = "txt") {
  if (!grepl("\\.csv$|\\.CSV", csv)) 
    stop("csv must be a csv file.")
  if (!is_character_vector(na_in_txt, len = 1)) 
    stop("na_in_txt must be a length 1 character.")
  folder <- make_valid_folder(folder, return_null = FALSE)
  if (!is_positive_integer(which, len = 1)) 
    stop("Argument which must be a length 1 positive integer.")
  if (!is.null(row.names) & !identical(row.names, 1)) {
    stop("Argument row.names should only be NULL or 1.")
  }
  df <- utils::read.csv(csv, header = header, row.names = ifelse(is.null(row.names), NULL, 1), stringsAsFactors = FALSE)
  all_text <- as.character(df[, which])
  if (!is.null(name_col)) {
    if (!is_positive_integer(name_col, len = 1)) 
      stop("name_col must be a NULL or a positive integer.")
	if (name_col>ncol(df))
	  stop("name_col should not be larger than number of columns.")
	true_name <- as.character(df[, name_col])
    true_name[is.na(true_name)] <- ""	
	true_name <- substr(true_name, start = 1, stop = 90)
    true_name <- base::basename(true_name)
    true_name <- gsub("\\.txt$|\\.rtf$|\\.TXT|\\.RTF", "", true_name)
    true_name <- gsub("\\W+", " ", true_name)
  }
  rm(df)
  if (!is_character_vector(na_in_csv)) {
    na_in_csv <- c(NA, "", " ", "?", "NA", "999")
    message("No valid character vector is provided, argument na_in_csv is set to default.")
  }
  na_in_csv <- unique(append(na_in_csv, c(NA, "", " ", "?", "NA", "999")))
  all_text[all_text %in% na_in_csv] <- na_in_txt
  all_len <- length(all_text)
  file_name <- zero_paste(1:all_len)
  if (identical(ext, "txt")){
    ext2 <- ".txt"
  } else if (identical(ext, "rtf")){
	ext2 <- ".rtf"
  } else if (identical(ext, "")){
	ext2 <- ""
  } else {
    ext2 <- ""
	message ("ext invalid, it has been set to size 0 string.")
  } 
  if (is.null(name_col)) {
    for (i in 1:all_len) {
      utils::write.table(all_text[i], paste(folder, "/", file_name[i], ext2, sep = ""), quote = FALSE, row.names = FALSE, col.names = FALSE, fileEncoding = "UTF-8")
    }
  }
  else {
    for (i in 1:all_len) {
      utils::write.table(all_text[i], paste(folder, "/", file_name[i], " ", true_name[i], ext2, sep = ""), quote = FALSE, row.names = FALSE, col.names = FALSE, fileEncoding = "UTF-8")
    }
  }
}