% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read-nbs}
\alias{read-nbs}
\alias{read_inv}
\alias{read_pcr}
\title{Read NBS Files}
\usage{
read_inv(
  date = NULL,
  col_select = NULL,
  col_types = vroom::cols(.default = vroom::col_character()),
  ...
)

read_pcr(
  date = NULL,
  col_select = NULL,
  col_types = vroom::cols(.default = vroom::col_character()),
  ...
)
}
\arguments{
\item{date}{The download date of the data file to read}

\item{col_select}{One or more selection expressions, like in
\code{dplyr::select()}. Use \code{c()} or \code{list()} to use more than one expression.
See \code{?dplyr::select} for details on available selection options.}

\item{col_types}{One of \code{NULL}, a \code{\link[readr:cols]{cols()}} specification, or
a string. See \code{vignette("readr")} for more details.

If \code{NULL}, all column types will be imputed from the first 1000 rows
on the input. This is convenient (and fast), but not robust. If the
imputation fails, you'll need to increase the \code{guess_max} or supply the
correct types yourself.

Column specifications created by \code{\link[=list]{list()}} or \code{\link[readr:cols]{cols()}} must contain
one column specification for each column. If you only want to read a
subset of the columns, use \code{\link[readr:cols]{cols_only()}}.

Alternatively, you can use a compact string representation where each
character represents one column:
\itemize{
\item c = character
\item i = integer
\item n = number
\item d = double
\item l = logical
\item f = factor
\item D = date
\item T = date time
\item t = time
\item ? = guess
\item _ or - = skip

By default, reading a file without a column specification will print a
message showing what \code{readr} guessed they were. To remove this message,
set \code{show_col_types = FALSE} or set `options(readr.show_col_types = FALSE).
}}

\item{...}{Additional arguments to pass to
\code{\link[coviData:read_file_delim]{read_file_delim()}}}
}
\value{
A \code{tibble}
}
\description{
\code{read_inv()} reads investigation data files

\code{read_pct()} reads PCR data files
}
