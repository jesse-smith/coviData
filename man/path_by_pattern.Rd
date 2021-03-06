% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/path_by_pattern.R
\name{path_by_pattern}
\alias{path_by_pattern}
\alias{path_teams}
\alias{path_nights_weekends}
\title{Get Path(s) to Recent Files Matching a Pattern}
\usage{
path_by_pattern(dir, pattern, by = c("modified", "created"))
}
\arguments{
\item{dir}{The path to the directory of interest}

\item{pattern}{The regular expression to use when filtering paths}

\item{by}{Should recency be determined by modification or creation date?}
}
\value{
An \code{fs_path} vector
}
\description{
\code{path_by_pattern()} finds the most recent file in a given directory that
matches a regular expression.
}
