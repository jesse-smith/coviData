% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fst.R
\name{fst_convert-common}
\alias{fst_convert-common}
\alias{fst_inv}
\alias{fst_pcr}
\alias{fst_vac}
\title{Save Commonly Used Files in \code{fst} Format}
\usage{
fst_inv(date = NULL, dir = NULL, ext = c("csv", "xlsx"), save_as = NULL, ...)

fst_pcr(date = NULL, save_as = NULL, ...)

fst_vac(path = path_vac(ext = "csv"), save_as = NULL, ...)
}
\arguments{
\item{date}{A \code{Date} or string in the format "YYYY-MM-DD"}

\item{dir}{The directory holding the files of interest}

\item{ext}{The file type to return. Files may be saved as "fst", "csv", or
"xlsx".}

\item{save_as}{Character. The path to save the result \code{fst} file. This must
have an \code{fst} extension; if it does not, the current extension (if any)
will be removed and replaced with \code{fst}. If left \code{NULL}, the \code{path}
argument is used (with extension replaced by \code{fst}).}

\item{...}{Additional arguments to pass to \code{read_file_delim()} or
\code{read_file_excel()}. The \code{col_types} argument is particularly useful to
control the type in which the data is saved (default is all \code{character}).}

\item{path}{Character. File path to read from. it will be passed to
\code{\link[coviData:read_file_excel]{read_file_excel()}} if the file
extension is "xlsx" or "xls", otherwise it will be passed to
\code{\link[coviData:read_file_delim]{read_file_delim()}}}
}
\value{
The dataset being converted, invisibly
}
\description{
Each \verb{fst_*} function is designed to easily convert a specific file, though
they will technically work with any tabular data file.

\code{fst_inv()} converts an investigations file

\code{fst_pcr()} converts a PCR file

\code{fst_vac()} converts a vaccination file
}
\details{
For general conversion to \code{fst} format, see
\code{\link[coviData:fst_convert]{fst_convert()}}.
}
