# NOTE ON AUTHORSHIP -----------------------------------------------------------
# These functions originate from the sftp package by Theodor Stenevang Klemming;
# see https://github.com/stenevang/sftp/ for the original implementation.

# FUNCTIONS --------------------------------------------------------------------

#' Create a List Object with SFTP Connection Details
#'
#' \code{sftp_connect} outputs a list which is used as an argument to all other
#' functions of the sftp package. Recommended practice is to store the output as
#' \code{sftp_con} since that is the object name all other sftp functions look
#' for by default. \strong{IMPORTANT! Your password MUST NOT contain the colon
#' character ("\code{}") or the at-sign ("\code{@@}") since these characters
#' have special use as delimiters in the connection string!}
#'
#' @param server The SFTP server URL, hostname, or IP address
#' @param folder The path to the folder where you want to operate; can be a
#'   single folder name in the SFTP root or a path with subfolders, e.g.
#'   \code{dir1/dir2/dir3}. Note that directory names are case sensitive.
#'   Default is empty string, which takes you to the root folder.
#' @param username The SFTP account username. It is recommended to not store
#'   your username in any script, but instead create an snvironment variable and
#'   call it using \code{\link[base]{Sys.getenv}}.
#' @param password The SFTP account password. It is recommended to not store
#'   your password in any script, but instead create an environment variable and
#'   call it using \code{\link[base]{Sys.getenv}}. \strong{IMPORTANT! Your
#'   password MUST NOT contain the colon character ("\code{:}") or the at-sign
#'   ("\code{@@}") since these characters have special use as delimiters in the
#'   connection string!}
#' @param protocol Default is \code{"sftp://"}. You will probably never change
#'   this value.
#' @param port Default is \code{"22"}. You will probably never change this
#'   value. Theoretically, an SFTP server could be set up to respond on a port
#'   other than the default port 22, but this is rarely the case.
#'
#' @return A list of strings which are used by all other \code{sftp_*} functions
#'   when connecting to an SFTP server.
#'
#' @examples
#'
#' \dontrun{
#' # Minimal - rely on defaults
#' sftp_con <- sftp_connect(
#'   username = Sys.getenv("MY_SFTP_USER"),
#'   password = Sys.getenv("MY_SFTP_PASS")
#' )
#'
#' # Explicit
#' sftp_con <- sftp_connect(
#'   server = "mysftp.com",
#'   folder = "myfolder",
#'   username = Sys.getenv("MY_SFTP_USER"),
#'   password = Sys.getenv("MY_SFTP_PASS"),
#'   protocol = "sftp://",
#'   port = 22
#' )
#' }
#'
#' @keywords internal
#'
#' @family sftp
#'
#' @export
#'
#' @author \href{https://github.com/stenevang/sftp}{Theodor Stenevang Klemming}
sftp_connect <- function(server   = "",
                         folder   = "",
                         username = "",
                         password = "",
                         protocol = "sftp://",
                         port     = 22) {
  server <- tolower(server)
  server <- gsub("^.*://", "", server) # remove any sftp:// or http:// etc
  server <- trim_slashes(server)

  # Important not to do tolower() on this value
  # since folder names are case sensitive on Unix/Linux
  folder <- trim_slashes(folder)

  userpass <- paste0(username, ":", password)

  protocol <- paste0(protocol, "://") # add colon-slash-slash incase missing
  protocol <- gsub("://://$", "://", protocol) # remove double ://

  port <- as.integer(port)

  url       <- paste0(protocol, server, "/", folder, "/")
  url_port  <- paste0(protocol, server, ":", port, "/", folder, "/")
  login_url <- paste0(protocol, userpass, "@", server, "/", folder, "/")
  login_url_port <- paste0(
    protocol, userpass, "@", server,  ":", port, "/", folder, "/"
  )

  sftp_connection <- list(protocol       = protocol,
                          server         = server,
                          port           = port,
                          folder         = folder,
                          username       = username,
                          password       = password,
                          userpass       = userpass,
                          url            = url,
                          url_port       = url_port,
                          login_url      = login_url,
                          login_url_port = login_url_port)
  return(sftp_connection)
}

#' List Files and/or Folders in an SFTP Directory
#'
#' \code{sftp_list} lists files, folders, or both in the SFTP location
#' specified by a list object created using \link{sftp_connect}.
#' Convenience wrapper functions are available in \code{\link{sftp_listfiles}}
#' and \code{\link{sftp_listdirs}}
#'
#' @param sftp_connection A list created by calling \code{\link{sftp_connect}};
#'   default value is \code{sftp_con}.
#' @param verbose Logical. Turn on messages to console. Default is \code{TRUE}.
#' @param curlPerformVerbose Logical. Turn on messages to console from
#'   \code{\link[RCurl:curlPerform]{RCurl::curlPerform}}.
#'   Default is \code{FALSE}.
#' @param encoding Explicitly identifies the encoding of the content that is
#'   returned by the server in its response. Possible values are \code{"UTF-8"}
#'   or \code{"ISO-8859-1"}. Default is \code{"UTF-8"}.
#' @param type One of \code{"f"}, code{"file"} for listing files; \code{"d"},
#'   \code{"dir"}, \code{"directory"} for listing directories; or \code{"all"}
#'   for both files and directories, the default.
#' @param recurse Logical. Recurse directories? Default is \code{FALSE}.
#'
#' @return A \code{\link[tibble]{tibble}}
#'
#' @examples
#'
#' \dontrun{
#' # minimal - rely on defaults
#' files_and_folders <- sftp_list(type = "all")
#'
#' # explicit
#' files_and_folders <- sftp_list(sftp_connection = sftp_con,
#'                                verbose = TRUE,
#'                                curlPerformVerbose = FALSE,
#'                                type = "all")
#' }
#'
#' @seealso \code{\link{sftp_listfiles}}, \code{\link{sftp_listdirs}}
#'
#' @keywords internal
#'
#' @family sftp
#'
#' @export
#'
#' @author \href{https://github.com/stenevang/sftp}{Theodor Stenevang Klemming}
sftp_list <- function(sftp_connection = get("sftp_con"),
                      verbose = TRUE,
                      curlPerformVerbose = FALSE,
                      encoding = "UTF-8",
                      type = "all",
                      recurse = F) {

  type <- tolower(type)

  allowed_type_values_file <- c("f", "file")
  allowed_type_values_dir  <- c("d", "dir", "directory")
  allowed_type_values <- c(
    allowed_type_values_file,
    allowed_type_values_dir,
    "all"
  )

  if (!(type %in% allowed_type_values)) {
    message(paste("The value of the 'type' argument is invalid:", type))
    message(
      paste(
        "Argument 'type' must be one of",
        paste(allowed_type_values, collapse = " ")
      )
    )
    return(F)
  }

  # Depending on the value of type, files OR folders will be listed
  cond_message <- function(mess, v = verbose) {
    if (v) message(mess)
  }

  cond_message(paste("SFTP url:", sftp_connection$url))
  curlOptionsValue <- "ftplistonly = T"
  rawstring <- RCurl::getURL(url = sftp_connection$url,
                             port = sftp_connection$port,
                             userpwd = sftp_connection$userpass,
                             verbose = curlPerformVerbose,
                             ftp.use.epsv = FALSE,
                             .encoding = encoding,
                             .opts = RCurl::curlOptions(curlOptionsValue))
  separated <- strsplit(rawstring, "\n", fixed = T)
  vector <- separated[[1]]
  vector2 <- gsub(" ", ";", vector)
  vector3 <- gsub(";+", ";", vector2)
  df <- data.frame("files" = vector3, stringsAsFactors = F)
  df2 <- df %>% tidyr::separate(
    "files",
    c(
      "rights",
      "links",
      "ownername",
      "ownergroup",
      "filesize",
      "t1",
      "t2",
      "t3",
      "name"
    ),
    sep = ";",
    extra = "merge"
  )
  df2$name <- gsub(";", " ", df2$name)
  df2$type <- ifelse(grepl("^d.*", df2$rights), "dir", "file")

  if (recurse) {
    dirs_found <- df2 %>%
      dplyr::filter(type == "dir", !.data[["name"]] %in% c(".", "..")) %>%
      dplyr::pull("name")
    for (d in dirs_found) {
      d_con <- sftp_connect(server = sftp_connection$server,
                            folder = paste0(sftp_connection$folder, "/", d),
                            username = sftp_connection$username,
                            password = sftp_connection$password,
                            protocol = sftp_connection$protocol,
                            port     = sftp_connection$port)
      d_list <- sftp_list(sftp_connection = d_con,
                          recurse = T)
      d_list <- d_list %>%
        dplyr::filter(!.data[["name"]] %in% c(".", "..")) %>%
        dplyr::mutate(name = paste0(d, "/", .data[["name"]]))

      df2 <- dplyr::bind_rows(df2, d_list) %>%
        dplyr::arrange(.data[["name"]])

    }
  }

  if (type %in% allowed_type_values_file) {
    final <- df2[df2$type == "file", ]
    cond_message(paste(nrow(final), "file(s) in SFTP folder."))
  } else if (type %in% allowed_type_values_dir) {
    final <- df2[df2$type == "dir", ]
    cond_message(paste(nrow(final), "dir(s) in SFTP folder."))
  } else {
    final <- df2
    cond_message(paste(nrow(final), "file(s) and/or dir(s) in SFTP folder."))
  }

  dplyr::as_tibble(final)
}

#' List files in an SFTP directory
#'
#' Convenience function wrapping \link{sftp_list} with type hard-set to "file".
#'
#' @param sftp_connection A list created by calling \link{sftp_connect}
#' Default value is "sftp_con".
#' @param verbose Logical. Turn on messages to console. Default is TRUE.
#' @param curlPerformVerbose Logical. Turn on messages to console from
#'   \code{\link[RCurl]{curlPerform}}. Default is FALSE.
#' @param recurse Logical. Recurse directories? Default is FALSE.
#'
#' @return A \code{\link[tibble]{tibble}}
#'
#' @examples
#'
#' \dontrun{
#' # minimal - rely on defaults
#' files <- sftp_listfiles()
#'
#' # explicit
#' files <- sftp_listfiles(sftp_connection = sftp_con,
#'                         verbose = TRUE,
#'                         curlPerformVerbose = FALSE)
#' }
#'
#' @seealso \link{sftp_listdirs}, \link{sftp_list}
#'
#' @keywords internal
#'
#' @family sftp
#'
#' @export
#'
#' @author \href{https://github.com/stenevang/sftp}{Theodor Stenevang Klemming}
sftp_listfiles <- function(sftp_connection = get("sftp_con"),
                           verbose = TRUE,
                           curlPerformVerbose = FALSE,
                           recurse = FALSE) {

  final <- sftp_list(sftp_connection = sftp_connection,
                     verbose = verbose,
                     curlPerformVerbose = curlPerformVerbose,
                     type = "file",
                     recurse = recurse)

  dplyr::as_tibble(final)
}



#' List folders in an SFTP directory
#'
#' Convenience function wrapping \link{sftp_list} with type hard set to "dir".
#'
#' @param sftp_connection A list created by calling \link{sftp_connect}
#' Default value is \code{sftp_con}.
#' @param verbose Logical. Turn on messages to console. Default is TRUE.
#' @param curlPerformVerbose Logical. Turn on messages to console from
#'   \code{\link[RCurl]{curlPerform}}. Default is FALSE.
#' @param recurse Logical. Recurse directories? Default is FALSE.
#'
#' @return A \code{\link[tibble]{tibble}}
#'
#' @examples
#'
#' \dontrun{
#' # minimal - rely on defaults
#' files <- sftp_listdirs()
#'
#' # explicit
#' files <- sftp_listdirs(sftp_connection = sftp_con,
#'                        verbose = TRUE,
#'                        curlPerformVerbose = FALSE)
#' }
#'
#' @seealso \link{sftp_listfiles}, \link{sftp_list}
#'
#' @keywords internal
#'
#' @family sftp
#'
#' @export
#'
#' @author \href{https://github.com/stenevang/sftp}{Theodor Stenevang Klemming}
sftp_listdirs <- function(sftp_connection = get("sftp_con"),
                          verbose = TRUE,
                          curlPerformVerbose = FALSE,
                          recurse = FALSE) {

  final <- sftp_list(sftp_connection = sftp_connection,
                     verbose = verbose,
                     curlPerformVerbose = curlPerformVerbose,
                     type = "dir",
                     recurse = recurse)

  dplyr::as_tibble(final)
}



#' Download files from an SFTP account
#'
#' This function is used for downloading files from an SFTP account.
#' The function uses connection credentials from a list object
#' created by calling \link{sftp_connect}. The files will be downloaded from
#' the SFTP account folder where you are currently "standing" as specified in
#' the connection list object.
#'
#' @param file A character vector of file names. If the wildcard "*" is used,
#'   then all files on the SFTP url will be downloaded. NOTE! Folders
#'   (directories) will not be downloaded.
#' @param tofolder A string containing a valid path - relative or absolute - to
#' the folder where the files will be saved. Default is the working directory.
#' If the folder does not exist it will be created.
#' @param sftp_connection A list object created with \code{\link{sftp_connect}}.
#'   Default is 'sftp_con'.
#' @param verbose Logical. Turn on messages to console. Default is TRUE.
#' @return The function returns the number of files downloaded, following
#'   successful download.
#'
#' @examples
#'
#' \dontrun{
#' # minimal - take all files on the SFTP url and save in the working directory
#' sftp_download("*")
#'
#' # take a vector of file names from a dataframe
#' sftp_download(file = files$name, tofolder = "my/relative/path")
#'
#' # take one specific file name from a vector from a dataframe
#' sftp_download(file = files$name[1], tofolder = getwd() )
#' }
#'
#' @seealso \link{sftp_upload}, \link{sftp_delete}, \link{sftp_rename}
#'
#' @keywords internal
#'
#' @family sftp
#'
#' @export
#'
#' @author \href{https://github.com/stenevang/sftp}{Theodor Stenevang Klemming}
sftp_download <- function(file,
                          tofolder = getwd(),
                          sftp_connection = get("sftp_con"),
                          verbose = TRUE) {

  tofolder <- trim_slashes(tofolder)

  using_wildcard <- FALSE
  if (length(file) == 1) {
    if (file == "*") {
      filelist <- sftp_listfiles(sftp_connection, verbose = FALSE)
      file <- filelist$name
      using_wildcard <- TRUE
    }
  }

  cond_message <- function(mess, v = verbose) {
    if (v) message(mess)
  }

  cond_message(paste("SFTP source:", sftp_connection$url))
  cond_message(paste("Save to folder:", tofolder))
  if (using_wildcard) cond_message("Downloading all files.")
  cond_message(paste(length(file), "file(s) to download."))
  filecounter <- 0
  for (f in file) {
    fileurl <- paste0(sftp_connection$url, f)
    port    <- as.integer(sftp_connection$port)
    userpwd <- sftp_connection$userpass
    filedestination <- file.path(tofolder, f)
    if (!dir.exists(dirname(filedestination))) {
      dir.create(dirname(filedestination), recursive = T)
      message("Creating folder ", dirname(filedestination))
    }
    writeBin(
      object = RCurl::getBinaryURL(
        url = fileurl,
        port = port,
        userpwd = userpwd,
        dirlistonly = FALSE
      ),
      con = filedestination
    )
    filecounter <- filecounter + 1
    cond_message(paste(f, "downloaded"))
  }
  cond_message(paste(filecounter, "file(s) downloaded."))
  return(filecounter)
}

#' Upload files to an SFTP account
#'
#' This function is used for uploading files to an SFTP account.
#' The function uses connection credentials from a list object
#' created by calling \link{sftp_connect}. The files will be uploaded to
#' the SFTP account folder where you are currently "standing" as specified in
#' the connection list object.
#'
#' @param file A character vector of file names. If the wildcard "*" is used,
#'  then all files in \code{fromfolder} will be uploaded. NOTE! Names of folders
#'  (directories) cannot be part of \code{file}. To create folders on the SFTP
#'  server, please see \code{\link{sftp_makedir}}.
#' @param fromfolder A string containing a valid path - relative or absolute -
#'   to the folder where the files in \code{file} can be found. Default is the
#'   working directory.
#' @param sftp_connection A list object created with \code{link{sftp_connect}}.
#'   Default is \code{"sftp_con"}.
#' @param log_file Valid path to a text file (.txt, .csv, .log etc) including
#'   filename and file extension.
#' @param verbose Logical. Turn on messages to console. Default is TRUE.
#' @return The function returns the number of files uploaded, following
#'   successful upload.
#'
#' @examples
#'
#' \dontrun{
#' # minimal - upload all files in the current working directory
#' sftp_upload("*")
#'
#' # take a vector of file names from a dataframe
#' sftp_upload(file = files$name, fromfolder = "my/relative/path/")
#'
#' # take one specific file name from a vector from a dataframe
#' sftp_upload(file = files$name[1], fromfolder = getwd())
#' }
#'
#'
#' @seealso \link{sftp_download}, \link{sftp_delete}, \link{sftp_rename}
#'
#' @keywords internal
#'
#' @family sftp
#'
#' @export
#'
#' @author \href{https://github.com/stenevang/sftp}{Theodor Stenevang Klemming}
sftp_upload <- function(file,
                        fromfolder = getwd(),
                        sftp_connection = get("sftp_con"),
                        log_file = NA,
                        verbose = TRUE) {

  fromfolder <- trim_slashes(fromfolder)

  using_wildcard <- FALSE
  if (length(file) == 1) {
    if (file == "*") {
      file <- list.files(path = fromfolder)
      #removing directories from the listing
      file <- file[!file.info(file)$isdir]
      using_wildcard <- TRUE
    }
  }

  cond_message <- function(mess, v = verbose) {
    if (v) message(mess)
  }

  cond_message(paste("Upload from folder:", fromfolder))
  cond_message(paste("SFTP destination:", sftp_connection$url))
  if (using_wildcard) cond_message("Uploading all files.")
  cond_message(paste(length(file), "file(s) to upload."))
  filecounter <- 0
  for (f in file) {
    filesource <- file.path(fromfolder, f)
    fileurl    <- paste0(sftp_connection$url, f)
    port       <- sftp_connection$port
    userpwd    <- sftp_connection$userpass
    RCurl::ftpUpload(what = filesource,
                     asText = FALSE,
                     to = fileurl,
                     port = port,
                     userpwd = userpwd)

    filecounter <- filecounter + 1
    cond_message(paste(f, "uploaded"))
  }
  cond_message(paste(filecounter, "file(s) uploaded."))

  if (!is.na(log_file)) {
    if (!file.exists(log_file)) {
      if (dirname(log_file) != ".") dir.create(dirname(log_file))
      file.create(log_file)
    }
    sftp_log(paste(file, "uploaded."), log_file)
    files_in_sftp <- sftp_listfiles(
      sftp_connection = sftp_connection,
      verbose = FALSE
    )
    files_in_sftp <- files_in_sftp[order(files_in_sftp$name), ]
    sftp_log("Files confirmed in the SFTP account:", log_file)
    for (r in seq_len(files_in_sftp)) {
      sftp_log(
        paste(
          files_in_sftp$name[r],
          "(", files_in_sftp$filesize[r], "bytes)"
        ),
        log_file
      )
    }
    sftp_log("- - -", log_file)
  }

  return(filecounter)
}


#' Delete files in an SFTP account
#'
#' Specify a file name or a vector of file names to delete in an SFTP account.
#'
#' @param file A vector of one or more file names that exist on the SFTP url.
#' Wildcard is not supported in order to minimize the risk of accidental
#' deletion. When a large portion of the files currently on the SFTP url needs
#' to be deleted, you can create an input value for \code{file} by using
#' \code{\link{sftp_listfiles}}. Note! Folders (directories) cannot be deleted.
#' See instead \code{\link{sftp_removedir}}.
#' @param sftp_connection A list object created by calling \link{sftp_connect}.
#' Default is 'sftp_con'.
#' @param verbose Logical. Turn on messages to console. Default is TRUE
#' @param curlPerformVerbose Logical. Turn on messages to console form
#'   curlPerform. Default is FALSE.
#' @return The function returns the number of files deleted.
#'
#' @examples
#'
#' \dontrun{
#' # minimal - delete single file and rely on defaults
#' sftp_delete("my_bad_file.csv")
#'
#' # explicit - delete several files in a vector
#' sftp_delete(file = files$name,
#'             sftp_connection = sftp_con,
#'             verbose = TRUE,
#'             curlPerformVerbose = FALSE)
#' }
#'
#' @seealso \link{sftp_download}, \link{sftp_upload}, \link{sftp_rename}
#'
#' @keywords internal
#'
#' @family sftp
#'
#' @export
#'
#' @author \href{https://github.com/stenevang/sftp}{Theodor Stenevang Klemming}
sftp_delete <- function(file,
                        sftp_connection = get("sftp_con"),
                        verbose = TRUE,
                        curlPerformVerbose = FALSE) {

  # This function does not support use of wildcard
  # for filenames to avoid the risk of massive
  # file deletion by mistake

  cond_message <- function(mess, v = verbose) {
    if (v) message(mess)
  }

  cond_message(paste("SFTP url:", sftp_connection$url))
  cond_message(paste(length(file), "file(s) to delete."))
  filecounter <- 0
  for (f in file) {
    deletepath <- paste0("'/", sftp_connection$folder, "/", f, "'")
    cond_message(deletepath)
    RCurl::curlPerform(url = sftp_connection$url,
                port = sftp_connection$port,
                userpwd = sftp_connection$userpass,
                verbose = curlPerformVerbose,
                quote = paste("rm", deletepath))
    filecounter <- filecounter + 1
    cond_message(paste(f, "deleted."))
  }
  cond_message(paste(filecounter, "file(s) deleted."))
  return(filecounter)
}



#' Rename a single file or single directory in an SFTP account
#'
#' Specify a current name and a new name, for a file or folder existing in an
#' SFTP account. While folders must be empty when using
#' \code{\link{sftp_removedir}}, that is not the case with \code{sftp_rename} -
#' non-empty folders can be renamed.
#'
#' @param from A single name of an existing file or folder. If a vector of names
#'   is supplied, only the first value will be used, and there will be a
#'   warning.
#' @param to A single name of an existing file or folder. If a vector of names
#'   is supplied, only the first value will be used, and there will be a
#'   warning.
#' @param sftp_connection A list object created by calling \link{sftp_connect}.
#' Default is \code{sftp_con}.
#' @param verbose Logical. Turn on messages to console. Default is TRUE
#' @param curlPerformVerbose Logical. Turn on messages to console form
#'   curlPerform. Default is FALSE.
#'
#' @return The function returns TRUE if successful.
#'
#' @examples
#'
#' \dontrun{
#' # minimal - rely on defaults
#' sftp_rename("oldname.csv", "newname.csv")
#'
#' # explicit - rename a folder
#' sftp_rename(from = "old_folder_name",
#'             to = "new_folder_name",
#'             sftp_connection = sftp_con,
#'             verbose = TRUE,
#'             curlPerformVerbose = FALSE)
#' }
#'
#' @seealso \link{sftp_download}, \link{sftp_upload}, \link{sftp_delete}
#'
#' @keywords internal
#'
#' @family sftp
#'
#' @export
#'
#' @author \href{https://github.com/stenevang/sftp}{Theodor Stenevang Klemming}
sftp_rename <- function(from,
                       to,
                       sftp_connection = get("sftp_con"),
                       verbose = TRUE,
                       curlPerformVerbose = FALSE) {

  if (missing(from)) {
    message("No value for argument 'from' supplied.")
    return(FALSE)
  }

  if (missing(to)) {
    message("No value for argument 'to' supplied.")
    return(FALSE)
  }

  if (!is.vector(from)) {
    message("Error: Value of argument 'from' is not a vector.")
    return(FALSE)
  }

  if (!is.vector(to)) {
    message("Error: Value of argument 'to' is not a vector.")
    return(FALSE)
  }

  if (length(from) > 1) {
    from <- from[1]
    message(
      "Warning: Argument 'from': several values supplied, ",
      "using only first value."
    )
  }

  if (length(to) > 1) {
    to <- to[1]
    message(
      "Warning: Argument 'to': several values supplied, using only first value."
    )
  }


  cond_message <- function(mess, v = verbose) {
    if (v) message(mess)
  }

  from <- paste0("'/", sftp_connection$folder, "/", from, "'")
  to   <- paste0("'/", sftp_connection$folder, "/", to, "'")
  argument <- paste("rename", from, to)
  RCurl::curlPerform(url = sftp_connection$url,
              port = sftp_connection$port,
              userpwd = sftp_connection$userpass,
              verbose = curlPerformVerbose,
              quote = argument)
  cond_message(paste("Old name:", from))
  cond_message(paste("New name:", to))

  return(TRUE)
}


#' Create a new directory in an SFTP account
#'
#' Create one or several directories (subdirectories) in an SFTP account.
#' The function uses connection credentials from a list object
#' created by calling \link{sftp_connect}.
#'
#' @param foldername A character vector of length 1 or more containing the names
#' of folders you want to create. The folder(s) will be created below the folder
#' where you are currently standing according to the sftp connection list
#' object you are using (default 'stfp_con'). A folder with subfolder and
#' subsubfolder etc can be created in one single operation by supplying a path,
#' like "folder1/folder2/folder3".
#' @param sftp_connection A list object created  by calling \link{sftp_connect}.
#' Default is \code{sftp_con}.
#' @param verbose Logical. Turn on messages to console. Default is TRUE
#' @param curlPerformVerbose Logical. Turn on messages to console form
#'   curlPerform. Default is FALSE.
#'
#' @return The function returns the number of new folders created.
#'
#' @examples
#'
#' \dontrun{
#' # minimal - create one folder, rely on defaults
#' sftp_makedir("great_folder_name")
#'
#' # explicit - create a multi-level path
#' sftp_makedir(foldername = "level1/folder2/directory3",
#'              sftp_connection = sftp_con,
#'              verbose = TRUE,
#'              curlPerformVerbose = FALSE)
#'
#'  # create new folders as specified in a vector with names
#'  sftp_makedir(newfolders$names)
#' }
#'
#' @seealso \link{sftp_removedir}, \link{sftp_rename}, \link{sftp_changedir}
#'
#' @keywords internal
#'
#' @family sftp
#'
#' @export
#'
#' @author \href{https://github.com/stenevang/sftp}{Theodor Stenevang Klemming}
sftp_makedir <- function(foldername,
                         sftp_connection = get("sftp_con"),
                         verbose = TRUE,
                         curlPerformVerbose = FALSE) {

  if (!is.vector(foldername)) {
    message("Error: Value of argument 'foldername' is not a vector.")
    return(FALSE)
  }

  # Important not to do tolower() on this value
  # since folder names are case sensitive on Unix/Linux
  foldername <- trim_slashes(foldername)

  cond_message <- function(mess, v = verbose) {
    if (v) message(mess)
  }

  cond_message(paste("SFTP url:", sftp_connection$url))
  cond_message(paste(length(foldername), "folder(s) to create."))
  filecounter <- 0
  for (f in foldername) {
    RCurl::curlPerform(
      url = sftp_connection$url,
      port = sftp_connection$port,
      userpwd = sftp_connection$userpass,
      verbose = curlPerformVerbose,
      quote = paste0("mkdir '/", sftp_connection$folder, "/", f, "'")
    )
    filecounter <- filecounter + 1
    cond_message(paste(f, "folder created."))
  }
  cond_message(paste(filecounter, "folder(s) created."))
  return(filecounter)
}


#' Remove a new directory in an SFTP account
#'
#' Remove one or several directories (subdirectories) in an SFTP account.
#' The function uses connection credentials from a list object
#' created by calling \link{sftp_connect}.
#'
#' @param foldername A character vector of length 1 or more, containing the
#'   names of folders you want to remove. The folder(s) will be looked for below
#'   the folder where you are currently standing according to the sftp
#'   connection list object you are using (default \code{"stfp_con"}). A
#'   subfolder (or a sub-subfolder, etc.) can be removed in one single operation
#'   by supplying a path, like "folder1/folder2/folder3". That will remove the
#'   last folder in the path, in this case "folder3". NOTE! Non-empty
#'   directories cannot be removed.
#' @param sftp_connection A list object created  by calling \link{sftp_connect}.
#' Default is \code{sftp_con}.
#' @param verbose Logical. Turn on messages to console. Default is TRUE
#' @param curlPerformVerbose Logical. Turn on messages to console form
#'   curlPerform. Default is FALSE.
#'
#' @return The function returns the number folders deleted.
#'
#' @examples
#'
#' \dontrun{
#' # minimal - remove one folder, rely on defaults
#' sftp_removedir("this_folder_is_bad")
#'
#' # explicit - remove a directory at the end of a multi-level path
#' sftp_removedir(foldername = "level1/folder2/directory3",
#'              sftp_connection = sftp_con,
#'              verbose = TRUE,
#'              curlPerformVerbose = FALSE)
#'
#' # remove several folders as specified in a vector of names
#' sftp_removedir(removefolders$names)
#' }
#'
#' @seealso \link{sftp_makedir}, \link{sftp_rename}, \link{sftp_changedir}
#'
#' @keywords internal
#'
#' @family sftp
#'
#' @export
#'
#' @author \href{https://github.com/stenevang/sftp}{Theodor Stenevang Klemming}
sftp_removedir <- function(foldername,
                           sftp_connection = get("sftp_con"),
                           verbose = TRUE,
                           curlPerformVerbose = FALSE) {

  if (!is.vector(foldername)) {
    message("Error: Value of argument 'foldername' is not a vector.")
    return(FALSE)
  }

  # Important not to do tolower() on this value
  # since folder names are case sensitive on Unix/Linux
  foldername <- trim_slashes(foldername)

  cond_message <- function(mess, v = verbose) {
    if (v) message(mess)
  }

  cond_message(paste("SFTP url:", sftp_connection$url))
  cond_message(paste(length(foldername), "folder(s) to remove."))
  filecounter <- 0
  for (f in foldername) {
    RCurl::curlPerform(
      url = sftp_connection$url,
      port = sftp_connection$port,
      userpwd = sftp_connection$userpass,
      verbose = curlPerformVerbose,
      quote = paste0("rmdir '/", sftp_connection$folder, "/", f, "'")
    )
    filecounter <- filecounter + 1
    cond_message(paste(f, "folder removed."))
  }
  cond_message(paste(filecounter, "folder(s) removed."))
  return(filecounter)
}



#' Move to another directory in an SFTP account
#'
#' Change your "current directory" used by the sftp functions.
#' The function uses and modifies the connection credentials in a list object
#' created by calling \link{sftp_connect}.
#'
#' @param tofolder A character vector of length 1 containing the name of a
#'   folder to where you want to go. The folder(s) will be looked for below the
#'   folder where you are currently standing according to the sftp connection
#'   list object you are using (default 'stfp_con'). A subfolder (or a
#'   sub-subfolder, etc.) can be specified in one single operation by supplying
#'   a path, like "folder1/folder2/folder3". To move up one level in the folder
#'   hierarchy, use \code{"../"}. To go to the root folder, use "root".
#' @param current_connection_name A string giving the name of the SFTP
#'   connection list object currently used, created  by calling
#'   \code{\link{sftp_connect}}. Default is \code{"sftp_con"}.
#' @param verbose Logical. Turn on messages to console. Default is TRUE
#' @param curlPerformVerbose Logical. Turn on messages to console form
#'   curlPerform. Default is FALSE.
#' @return The function returns TRUE after successful change of directory.
#'
#' @examples
#'
#' \dontrun{
#' # minimal - move up one level, rely on defaults
#' sftp_changedir("..")
#'
#' # minimal - to to root, rely on defaults
#' sftp_changedir("root")
#'
#' # explicit - change to a directory at the end of a multi-level path
#' sftp_changedir(tofolder = "level1/folder2/directory3",
#'                current_connection_name = "sftp_con",
#'                verbose = TRUE,
#'                curlPerformVerbose = FALSE)
#' }
#'
#' @seealso \link{sftp_makedir}, \link{sftp_removedir}, \link{sftp_rename}
#'
#' @keywords internal
#'
#' @family sftp
#'
#' @export
#'
#' @author \href{https://github.com/stenevang/sftp}{Theodor Stenevang Klemming}
sftp_changedir <- function(tofolder,
                           current_connection_name = "sftp_con",
                           verbose = TRUE,
                           curlPerformVerbose = FALSE) {

  if (!is.vector(tofolder)) {
    message("Error: Value of argument 'tofolder' is not a vector.")
    return(FALSE)
  }

  if (length(tofolder) > 1) {
    tofolder <- tofolder[1]
    message(
      "Warning: Argument 'tofolder': several values supplied,",
      " using only first value."
    )
  }

  # Important not to do tolower() on this value
  # since folder names are case sensitive on Unix/Linux
  tofolder <- trim_slashes(tofolder)

  tofolder <- unlist(strsplit(tofolder, split = "/"))

  cond_message <- function(mess, v = verbose) {
    if (v) message(mess)
  }

  parent_frame <- parent.frame()
  sftp_connection <- parent_frame[[current_connection_name]]

  cond_message(paste("SFTP url:", sftp_connection$url))

  for (i in seq_along(tofolder)) {
    current_folder <- sftp_connection$folder
    # backing up one level
    if (tofolder[i] == "..") {
      new_folder <- gsub("(^.*?)/.[^/].*?$", "\\1", current_folder)
      cond_message(paste("Moving up one level.", ""))
    } else if (tofolder[i] == "root" | tofolder[i] == "") {
      new_folder <- ""
      cond_message(paste("Moving to root folder.", ""))
    } else {
      new_folder <- paste0(current_folder, "/", tofolder[i])
      cond_message(paste("Moving down into", new_folder))
    }


    sftp_connection <- sftp_connect(server = sftp_connection$server,
                                    folder = new_folder,
                                    username = sftp_connection$username,
                                    password = sftp_connection$password,
                                    protocol = sftp_connection$protocol,
                                    port = sftp_connection$port)
  }

  assign(
    x = current_connection_name,
    envir = parent.frame(),
    value = sftp_connection
  )

  return(TRUE)
}

#' Write to SFTP Log File
#'
#' @param message Character. Text to write to the file. It is passed to
#'   `readr::write_lines()`.
#'
#' @param log_file Character. The path to the log file
#'
#' @return `message` (invisibly)
#'
#' @family sftp
#'
#' @author \href{https://github.com/stenevang/sftp}{Theodor Stenevang Klemming}
#'
#' @noRd
sftp_log <- function(message,
                     log_file) {
  readr::write_lines(
    x = paste(Sys.time(), message, "\r"),
    path = log_file,
    append = TRUE
  )
}

#' Remove Trailing Forward Slashes from File Path
#'
#' @param string Character. One or more file paths
#'
#' @return `string` with trailing forward slashes removed
#'
#' @family sftp
#'
#' @author \href{https://github.com/stenevang/sftp}{Theodor Stenevang Klemming}
#'
#' @noRd
trim_slashes <- function(string) {
  string <- gsub("/{1,2}$", "", string) # remove any trailing slash
  return(string)
}
