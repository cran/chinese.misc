% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corp_or_dtm.R
\name{corp_or_dtm}
\alias{corp_or_dtm}
\title{Create Corpus or Document Term Matrix with 1 Line}
\usage{
corp_or_dtm(
  ...,
  from = "dir",
  type = "corpus",
  enc = "auto",
  mycutter = DEFAULT_cutter,
  stop_word = NULL,
  stop_pattern = NULL,
  control = "auto",
  myfun1 = NULL,
  myfun2 = NULL,
  special = "",
  use_stri_replace_all = FALSE
)
}
\arguments{
\item{...}{names of folders, files, or the mixture of the two kinds. It can also be a character 
vector of texts to be processed when setting \code{from} to "v", see below.}

\item{from}{should be "dir" or "v". If your inputs are filenames, it should be "dir" (default), 
If the input is a character vector of texts, it should be "v". However, if it is set to "v", 
make sure each element is not identical to filename in your working
directory; and, if they are identical, the function will raise an error. To do this check is 
because if they are identical, \code{jiebaR::segment} 
will take the input as a file to read!}

\item{type}{what do you want for result. It is case insensitive, thus those start with 
"c" or "C" represent a corpus 
result; and those start with "d" or "D" for document term matrix, 
and those start with "t" or "T" for term document matrix. 
Input other than the above represents 
a corpus result. The default value is "corpus".}

\item{enc}{a length 1 character specifying encoding when reading files. If your files 
may have different encodings, or you do not know their encodings, 
set it to "auto" (default) 
to let the function auto-detect encoding for each file.}

\item{mycutter}{the jiebar cutter to segment text. A default cutter is used. See Details.}

\item{stop_word}{a character vector to specify stop words that should be removed. 
If it is \code{NULL}, nothing is removed. If it is "jiebar", "jiebaR" or "auto", the stop words used by 
\pkg{jiebaR} are used, see \code{\link{make_stoplist}}.
Please note the default value is \code{NULL}. Texts are transformed to lower case before 
removing stop words, so your stop words only need to contain lower case characters.}

\item{stop_pattern}{vector of regular expressions. These patterns are similar to stop words. 
Terms that match the patterns will be removed.
Note: the function will automatically adds "^" and "$" to the pattern, which means 
first, the pattern you provide should not contain these two; second, the matching
is complete matching. That is  to say, if a word is to be removed, it not just
contains the pattern (which is to be checked by \code{grepl}, but the whole
word match the pattern.}

\item{control}{a named list similar to that 
which is used by \code{DocumentTermMatrix} 
or \code{TermDocumentMatrix} to create dtm or tdm. But 
there are some significant differences. 
Most of the time you do not need to 
set this value because a default value is used. When you set the argument to \code{NULL}, 
it still points to this default value. See Details.}

\item{myfun1}{a function used to modify each text after being read by \code{scancn} 
and before being segmented.}

\item{myfun2}{a function used to modify each text after they are segmented.}

\item{special}{a length 1 character or regular expression to be passed to \code{dir_or_file} 
to specify what pattern should be met by filenames. The default is to read all files.
See \code{\link{dir_or_file}}.}

\item{use_stri_replace_all}{default is FALSE. If it is TRUE, 
\code{stringi::stri_replace_all} is used to delete stop words, which has 
a slightly higher speed. This is still experimental.}
}
\value{
a corpus, or document term matrix, or term document matrix.
}
\description{
This function allows you to input a vector of characters, or a mixture of files and folders, it 
will automatically detect file encodings, segment Chinese texts, 
do specified modification, 
remove stop words,  and then generate corpus or dtm (tdm). Since \pkg{tm} 
does not support Chinese well, this function manages to solve some problems. See Details.
}
\details{
Package \pkg{tm} sometimes
tries to segment an already segmented Chinese Corpus and put together terms that 
should not be put together. The function is to deal with the problem.
It calls \code{\link{scancn}} to read files and 
auto-detect file encodings, 
and calls \code{jiebaR::segment} to segment Chinese text, and finally 
calls \code{tm::Corpus} to generate corpus.
When creating DTM/TDM, it 
partially depends on \code{tm::DocumentTermMatrix} 
and \code{tm::TermDocumentMatrix}, but also has some significant
differences in setting control argument. 

Users should provide their jiebar cutter by \code{mycutter}. Otherwise, the function 
uses \code{DEFAULT_cutter} which is created when the package is loaded. 
The \code{DEFAULT_cutter} is simply \code{worker(write = FALSE)}.
See \code{jiebaR::worker}.

As long as 
you have not manually created another variable called "DEFAULT_cutter", 
you can directly use \code{jiebaR::new_user_word(DEFAULT_cutter...)} 
to add new words. By the way, whether you manually create an object 
called "DEFAULT_cutter", the original loaded DEFAULT_cutter which is 
used by default by functions in this package will not be removed by you.
So, whenever you want to use this default value, you do not need to set 
\code{mycutter} and keep it as default.

The argument \code{control} is very similar to the argument used by 
\code{tm::DocumentTermMatrix}, but is quite different and will not be passed
to it! The permitted elements are below:

\itemize{
  \item (1) wordLengths: length 2 positive integer vector. 0 and \code{inf}
is not allowed. If you only want words of 4 to 10, then set it to c(4, 10).
If you do not want to limit the ceiling value, just choose a large value, 
e.g., c(4, 100).
In package tm (>= 0.7), 1 Chinese character is roughly
of length 2 (but not always computed by multiplying 2), 
so if a Chinese words is of 4 characters, the min value 
of wordLengths is 8. But here in \code{corp_or_dtm}, word length is exactly
the same as what you see on the screen. So, a Chinese word with 4 characters is
of length 4 rather than 8.
  \item (2) dictionary: a character vetcor of the words which will appear in DTM/TDM 
when you do not want a full one. If none of the words in the dictionary appears in 
corpus, a blank DTM/TDM will be created. The vector should not contain 
\code{NA}, if it does, only non-NA elements will be kept. Make sure at least 1
element is not \code{NA}. Note: if both dictionary and wordLengths appear in 
your control list, wordLengths will be ignored.
  \item (3) bounds: an integer vector of length 2 which limits the term frequency
of words. Only words whose total frequencies are in this range will appear in 
the DTM/TDM. 0 and \code{inf} is not allowed. Let a large enough value to 
indicate the unlimited ceiling.
  \item (4) have: an integer vector of length 2 which limits the time a word 
appears in the corpus. Suppose a word appears 3 times in the 1st article and 2 
times in the 2nd article, and 0 in the 3rd, 
then its bounds value = 3 + 2 + 0 = 5; but its have 
value = 1 + 1 + 0 = 2.
  \item (5) weighting: a function to compute word weights. The default is to 
compute term frequency. But you can use other weighting functions, typically
\code{tm::weightBin} or \code{tm::weightTfIdf}.
  \item (6) tokenizer: this value is temporarily deprecated and  
it cannot be modified by users. 
}

By default, the argument \code{control} is set 
to "auto", "auto1", or \code{DEFAULT_control1}, 
which are the same. This control list is created 
when the package is loaded. It is simply \code{list(wordLengths = c(1, 25))}, 
Alternatively, \code{DEFAULT_control2} (or "auto2") is also created 
when loading package, which sets 
word length to 2 to 25.
}
\examples{
x <- c(
  "Hello, what do you want to drink?", 
  "drink a bottle of milk", 
  "drink a cup of coffee", 
  "drink some water")
# The simplest argument setting
dtm <- corp_or_dtm(x, from = "v", type = "dtm")
# Modify argument control to see what happens
dtm <- corp_or_dtm(x, from = "v", type="d", control = list(wordLengths = c(3, 20)))
tdm <- corp_or_dtm(x, from = "v", type = "T", stop_word = c("you", "to", "a", "of"))
}
