% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_generic.R
\name{download_servu}
\alias{download_servu}
\title{Download Lab File from Serv-U}
\usage{
download_servu(
  date = Sys.Date(),
  usr = Sys.getenv("sftp_usr"),
  pwd = Sys.getenv("sftp_pwd"),
  remote_dir,
  local_dir = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
  new_file = NULL
)
}
\arguments{
\item{date}{A \code{date} indicating the date of the file to download}

\item{usr}{The username to pass to the SFTP site. Ideally, this should be
stored in an Renviron file and not in a script.}

\item{pwd}{The password to pass to the SFTP site. Ideally, this should be
stored in an Renviron file and not in a script.}

\item{remote_dir}{A string indicating the directory on Serv-U that should
contain the file}

\item{local_dir}{A string indicating the directory on the local system in
which to save the file}

\item{new_file}{A string indicating the new file name to save the data under.
Currently not used.}
}
\description{
Download Lab File from Serv-U
}
