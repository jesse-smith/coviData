% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.R
\name{load_nbs}
\alias{load_nbs}
\title{Load NBS Data from Disk}
\usage{
load_nbs(
  date = Sys.Date(),
  directory = paste0("V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "Sandbox data pull Final/"),
  ext = c("csv", "xlsx")
)
}
\arguments{
\item{date}{The \code{Date} to look for in the file name}

\item{directory}{The directory to search in using \code{\link{find_file}}}

\item{ext}{The extension of the file to load}
}
\value{
A \code{\link[tibble]{tibble}} containing the loaded data
}
\description{
\code{load_nbs()} loads NBS data from a file into a \code{\link[tibble]{tibble}}.
It is essentially a wrapper around \code{\link{load_data}} with defaults
specific to NBS data.
}
