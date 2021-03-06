% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_date.R
\name{load_report_date}
\alias{load_report_date}
\title{Load Report Dates from \code{fst} File}
\usage{
load_report_date(
  path = path_create("V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "Report Date Linelist/report_date_linelist", ext = "fst")
)
}
\arguments{
\item{path}{The path to a file created by \code{save_report_date()}}
}
\value{
An \code{fst_tbl} object
}
\description{
\code{load_report_date()} is a simple wrapper around
\code{\link[fst:fst]{fst()}} that reads reduced report date data
from an \code{fst} file. It exists purely for convenience and semantic clarity; it
should only be used with files created by \code{save_report_date()}.
}
