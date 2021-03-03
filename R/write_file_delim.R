#' Efficiently Write Delimited Files
#'
#' `write_file_delim()` writes delimited files using
#' \code{\link[vroom:vroom_write]{vroom_write()}}.
#'
#' By default, files are comma-delimited with missing values represented as
#' blank strings (`""`).
#'
#' @param x Data frame or data frame extension to write to disk
#'
#' @param path Path or connection to write to
#'
#' @param delim Delimiter used to separate values. Defaults to `","` to write
#'   comma-separated value (CSV) files.
#'
#' @param na String used for missing values. Defaults to blank (`""`).
#'
#' @param force Should any existing files be overwritten?
#'
#' @param ... Additional arguments to pass to
#'   \code{\link[vroom:vroom_write]{vroom_write()}}
#'
#' @return THe input data
#'
#' @export
write_file_delim <- function(
  x,
  path,
  delim = ",",
  na = "",
  force = FALSE,
  ...
) {
  path <- path_create(path)

  assert_any(
    force,
    !fs::file_exists(path),
    message = paste(
      "A file already exists at this location; to overwrite, set `force = TRUE`"
    )
  )

  vroom::vroom_write(
    x,
    path = path,
    delim = delim,
    na = na,
    ...
  )
}
