% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DEFAULT_control2.R
\docType{data}
\name{DEFAULT_control2}
\alias{DEFAULT_control2}
\title{A Default Value for corp_or_dtm 2}
\format{
An object of class \code{list} of length 2.
}
\usage{
DEFAULT_control2
}
\description{
The object specifies word length from 2 to 
25. The second element, a tokenizer, is temporally deprecated.
Also, \code{DEFAULT_control1}
sets length from 1 to 25.
}
\examples{
require(tm)
x <- c(
  "Hello, what do you want to drink?", 
  "drink a bottle of milk", 
  "drink a cup of coffee", 
  "drink some water")
dtm <- corp_or_dtm(x, from = "v", type = "dtm", control = DEFAULT_control2)
}
\keyword{datasets}
