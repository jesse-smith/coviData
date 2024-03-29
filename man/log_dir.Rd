% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/log.R
\name{log_dir}
\alias{log_dir}
\title{Set/Get Logging Directory}
\usage{
log_dir(dir, persistent = FALSE)
}
\arguments{
\item{dir}{The path to the log directory. If missing, \code{log_dir} returns the
current value of \code{.log_dir}}

\item{persistent}{Should the \code{coviData_log_dir} environment variable persist
after ending the current R session?}
}
\value{
The current logging directory if \code{dir} is missing, or a named list
of the old logging directory if used to change the logging directory
}
\description{
\code{log_dir()} gets and sets the \code{.log_dir} option and the \code{coviData_log_dir}
environment variable. The latter can be set persistently on Windows using
\code{persistent = TRUE}.
}
