# Main `covidata` Class ########################################################

#' Construct or Validate `covidata` Subclasses
#'
#' @description
#' Creates or validates a subclass of `covidata`, which is just a
#' \code{\link[tibble:tbl_df-class]{tibble}} with a `date_stamp` attribute.
#'
#' `new_covidata()` creates a new object as a subclass of `covidata`,
#' `tbl_df`, `tbl`, and `data.frame`. This function is optimized for
#' performance; checks are reduced to a minimum.
#'
#' `validate_covidata()` checks a `covidata` object for internal consistency.
#' Correct behavior can be guaranteed only if this function runs without raising
#' an error.
#'
#' @inheritParams tibble::new_tibble
#'
#' @param date_stamp A \code{\link[base:Dates]{Date}} object indicating the date
#'   that the data was made available
#'
#' @seealso \code{\link[tibble]{new_tibble}}
#'
#' @name covidata-class
#'
#' @aliases new_covidata validate_covidata
NULL

#' @rdname covidata-class
#'
#' @export
new_covidata <- function(
  x,
  ...,
  date_stamp,
  nrow,
  class = NULL
) {
  tibble::new_tibble(
    x,
    ...,
    date_stamp = date_stamp,
    nrow = nrow,
    class = c(class, "covidata")
  )
}

#' @rdname covidata-class
#'
#' @export
validate_covidata <- function(x) {

  object_class <- attr(x, "class")
  covidata_class <- c("covidata", "tbl_df", "tbl", "data.frame")
  str_object_class <- stringr::str_flatten(object_class, ", ")
  str_covid_tbl_class <- stringr::str_flatten(covidata_class, ", ")



  assert(
    stringr::str_ends(str_object_class, pattern = str_covid_tbl_class),
    message = paste0(
      "This object's class is not 'covidata' or a subclass of 'covidata'; ",
      "see below for a comparison of class attributes.\n\n",
      if (requireNamespace("waldo", quietly = TRUE)) {
        waldo::compare(object_class, covidata_class)
      } else {
        "Sorry, the waldo package must be installed to view this comparision."
      }
    )
  )

  assert(
    !is.null(x, "date_stamp"),
    message = "`x` must have a `date_stamp` attribute"
  )

  date_stamp <- attr(x, "date_stamp")

  assert(
    lubridate::is.Date(date_stamp),
    message = "The 'date_stamp' attribute of `covidata` must be a `Date` object"
  )

  assert(
    date_stamp >= lubridate::as_date("2020-01-01") | is.na(date_stamp),
    message = paste0(
      "The 'date_stamp' attribute of `covidata` cannot be a date ",
      "earlier than January 01, 2020, when the WHO was notified of COVID-19."
    )
  )

  today <- lubridate::as_date(lubridate::now("UTC") + lubridate::hours(14))

  assert(
    attr(x, "date_stamp") <= today | is.na(date_stamp),
    message = paste0(
      "The 'date_stamp' attribute of `covidata` cannot be a date ",
      "later than the current date in the UTC+14 timezone."
    )
  )

  tibble::validate_tibble(x)
}

# `covidata` Subclasses ########################################################

#' Construct or Validate New Objects of Class `ael`, `nbs`, or `pcr`
#'
#' @description
#' The \code{new_*()} functions create a new object of class `ael`, `nbs`, or
#' `pcr` as a subclass of `covidata`, `tbl_df`, `tbl`, and `data.frame`. They
#' are optimized for performance; checks are reduced to a minimum.
#'
#' The \code{validate_*()} functions check an `ael`, `nbs`, or `pcr` object for
#' internal consistency. Correct behavior can be guaranteed if and only if this
#' function runs without raising an error.
#'
#' @inheritParams tibble::new_tibble
#'
#' @seealso new_covid_tbl validate_covid_tbl
#'
#' @name covidata-subclasses
#'
#' @aliases new_ael validate_ael new_nbs validate_nbs new_pcr validate_pcr
#'
#' @keywords internal
NULL

#' Create \code{new_subclass()} or \code{validate_subclass()} Functions
#'
#' @description
#' `new_covidata_subclass()` creates a function for constructing a `class`
#' object.
#'
#' `validate_covidata_subclass()` creates a function for validating a `class`
#' object.
#'
#' See `covidata-subclasses` for more.
#'
#' @param class A new `covidata` subclass
#'
#' @return A function for creating or validating a `covidata` subclass
#'
#' @keywords internal
#'
#' @name covidata-subclass-creators
#'
#' @aliases new_subclass_fn validate_subclass_fn
#'
#' @keywords internal
NULL

#' @rdname covidata-subclass-creators
new_covidata_subclass <- function(class) {
  function(x, date_stamp, nrow) {
    new_covidata(x, date_stamp = date_stamp, nrow = nrow, class = class)
  }
}

#' @rdname covidata-subclass-creators
validate_covidata_subclass <- function(class) {
  function(x) {
    assert(
      class == attr(x, "class")[[1]],
      msg = paste0("This is not an object of class ", class)
    )

    validate_covidata(x)
  }
}

#' @rdname covidata-subclasses
new_ael <- new_covidata_subclass("ael")

#' @rdname covidata-subclasses
new_nbs <- new_covidata_subclass("nbs")

#' @rdname covidata-subclasses
new_pcr <- new_covidata_subclass("pcr")

#' @rdname covidata-subclasses
validate_ael <- validate_covidata_subclass("ael")

#' @rdname covidata-subclasses
validate_nbs <- validate_covidata_subclass("nbs")

#' @rdname covidata-subclasses
validate_pcr <- validate_covidata_subclass("pcr")


# User-Friendly Subclass Helpers ###############################################

#' Build a Source-Specific Data Frame
#'
#' @description
#' `ael()`, `nbs()`, and `pcr()` construct data frames. They are used like
#' \code{\link[tibble:tibble]{tibble()}}, except an additional `.date` argument
#' is included.
#'
#' @details
#' These function create data frames of class `ael`, `nbs`, or `pcr`. The data
#' behave identically to a `tibble` in most functions, but coviData functions
#' uses these classes to treat each data source appropriately. Additionally, the
#' `.date` argument tells coviData functions when the data was made
#' available, which facilitates automatic handling of data reported at
#' different times.
#'
#' @param .date A `Date` indicating when the data was made available. This
#'   will usually come from name of the data file; the \code{coviData::load_*}
#'   family detects this automatically when reading the data. This argument is
#'   optional, but omitting it limits the ability of coviData functions to
#'   handle data from several reporting dates.
#'
#' @return An object of class `ael`, `nbs`, or `pcr`
#'
#' @seealso \code{\link[tibble:tibble]{tibble()}}
#'
#' @name covidata-build
#'
#' @aliases ael nbs pcr
NULL

#' Create a Helper Function for a `covidata` Subclass
#'
#' `covidata_builder` creates helper functions for a `covidata` subclass. It
#' requires \code{new_mysubclass()} and \code{validate_mysubclass()} functions
#' to be defined and available for the class \code{mysubclass}.
#'
#' @param class A `covidata` subclass
#'
#' @return A user-friendly helper function for constructing objects of `class`
#'
#' @keywords internal
covidata_builder <- function(class) {
  function(
    ...,
    .date = NULL,
    .rows = NULL,
    .name_repair = c("check_unique", "unique", "universal", "minimal")
  ) {

    # The new_* and validate_* functions for the class will be dynamically
    # assigned to these functions
    new_subclass <- paste0("new_", class) %>% rlang::sym() %>% eval()
    validate_subclass <- paste0("validate_", class) %>% rlang::sym() %>% eval()

    if (!lubridate::is.Date(.date)) {
      .date <- lubridate::as_date(.date)
    }

    tibble::tibble(..., .rows = .rows, .name_repair = .name_repair) %>%
      new_subclass(date_stamp = .date, nrow = NROW(.)) %>%
      validate_subclass()
  }
}

#' @rdname covidata-build
#'
#' @inheritParams tibble::tibble
#'
#' @export
ael <- covidata_builder("ael")


#' @rdname covidata-build
#'
#' @inheritParams tibble::tibble
#'
#' @export
nbs <- covidata_builder("nbs")

#' @rdname covidata-build
#'
#' @inheritParams tibble::tibble
#'
#' @export
pcr <- covidata_builder("pcr")

# `as_class` Functions #########################################################

#' Cast Lists, Matrices, and More to Source-Specific Data Frames
#'
#' The \code{as_*} family of functions turns existing objects, such as a data
#' frame or matrix, into a \code{\link[tibble:tbl_df-class]{tibble}} of class
#' `ael`, `nbs`, or `pcr`. This is in contrast to the `ael()`, `nbs()`, and
#' `pcr()` functions, which build tibbles from individual columns. Recall that
#' `tibble` subclasses in coviData have a `.date` attribute, which assists in
#' handling data made available at different times; you'll need to supply this
#' when using `as_ael()`, `as_nbs()`, or `as_pcr()`.
#'
#' These functions are thin wrappers around the \code{\link[tibble]{as_tibble}}
#' function; see there for details.
#'
#' @inheritParams tibble::as_tibble
#'
#' @inheritParams ael
#'
#' @seealso \code{\link[tibble]{as_tibble}}; \code{\link{ael}{ael()}},
#'   \code{\link{nbs}{nbs()}}, and \code{\link{pcr}{pcr()}}
#'
#' @name covidata-cast
#'
#' @aliases as_ael as_nbs as_pcr
NULL

#' Create `as_covidata` Functions
#'
#' Create a coercion function for a `covidata` subclass.
#'
#' @inheritParams covidata-subclass-creators
#'
#' @keywords internal
coerce_covidata <- function(class) {

  # The new_* and validate_* functions for the class will be dynamically
  # assigned to these functions
  new_subclass <- paste0("new_", class) %>% rlang::sym() %>% eval()
  validate_subclass <- paste0("validate_", class) %>% rlang::sym() %>% eval()

  function(
    x,
    ...,
    .date = as.Date(NA),
    .rows = NULL,
    .name_repair = c("check_unique", "unique", "universal", "minimal"),
    rownames = pkgconfig::get_config("tibble::rownames", NULL)
  ) {

    if (!lubridate::is.Date(.date)) {
      .date <- lubridate::as_date(.date)
    }

    x %>%
      tibble::as_tibble(
        ...,
        .rows = .rows,
        .name_repair = .name_repair,
        rownames = rownames
      ) %>%
      new_subclass(date_stamp = .date, nrow = NROW(.)) %>%
      validate_subclass()
  }
}

#' @rdname covidata-cast
#'
#' @export
as_ael <- coerce_covidata("ael")

#' @rdname covidata-cast
#'
#' @export
as_nbs <- coerce_covidata("nbs")

#' @rdname covidata-cast
#'
#' @export
as_pcr <- coerce_covidata("pcr")
