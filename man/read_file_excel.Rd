% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read_file_excel}
\alias{read_file_excel}
\title{Read Excel Files}
\usage{
read_file_excel(
  file,
  range = NULL,
  col_types = "text",
  na = c("", ".", "NA", "na", "Na", "N/A", "n/a", "N/a", "NULL", "null", "Null"),
  guess_max = .Machine$integer.max\%/\%100L,
  ...
)
}
\arguments{
\item{file}{Path to the xls/xlsx file}

\item{range}{A cell range to read from, as described in \link[readxl]{cell-specification}.
Includes typical Excel ranges like "B3:D87", possibly including the sheet
name like "Budget!B2:G14", and more. Interpreted strictly, even if the
range forces the inclusion of leading or trailing empty rows or columns.
Takes precedence over \code{skip}, \code{n_max} and \code{sheet}.}

\item{col_types}{Either \code{NULL} to guess all from the spreadsheet or a
character vector containing one entry per column from these options:
"skip", "guess", "logical", "numeric", "date", "text" or "list". If exactly
one \code{col_type} is specified, it will be recycled. The content of a cell in
a skipped column is never read and that column will not appear in the data
frame output. A list cell loads a column as a list of length 1 vectors,
which are typed using the type guessing logic from \code{col_types = NULL}, but
on a cell-by-cell basis.}

\item{na}{Character vector of strings to interpret as missing values. By
default, readxl treats blank cells as missing data.}

\item{guess_max}{Maximum number of data rows to use for guessing column
types.}

\item{...}{Additional arguments to pass to
\code{\link[readxl:read_excel]{read_excel()}}}
}
\value{
A \code{tibble}
}
\description{
\code{read_file_excel()} reads Excel files using
\code{\link[readxl:read_excel]{read_excel()}}.
}
\details{
By default, \code{read_file_excel()} does not attempt to guess column types and
reads all columns as character. This can be changed by setting
\code{col_types = "guess"}. If columns are
guessed, the default is to use all rows; this can be changed by setting
\code{guess_max} to a different value.

Note that when reading Excel files as character, dates will be read as the
Excel numeric representation in character format
(i.e. the date 2020-01-01 will be read as \code{"43831"}). These dates can be
parsed into \code{Date} format using \code{\link[coviData:std_dates]{std_dates()}}
or any of the janitor package's date conversion functions (the most basic
being \code{\link[janitor:excel_numeric_to_date]{excel_numeric_to_date()}}).

\code{read_file_excel()} will eventually be paired with
\code{read_file_delim()} to replace the internals of
\code{\link[coviData:read_file]{read_file()}}.
}
