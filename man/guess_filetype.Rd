% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{guess_filetype}
\alias{guess_filetype}
\title{Guess the Filetype of a Data File}
\usage{
guess_filetype(path)
}
\arguments{
\item{path}{The path to the data file}
}
\value{
One of \code{"excel"}, \code{"delimited"}, \code{"unknown"}
}
\description{
\code{guess_filetype} attempts to guess the encoding of a data file from the
file's extension. If the extension is not recognized, it returns
\code{"unknown"}.
}
\keyword{internal}
