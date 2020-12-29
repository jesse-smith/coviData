#' Assert that an Object Is a Data Frame
#'
#' `assert_dataframe()` checks whether the input object is a data frame. If so,
#' it returns the object; if not, it throws an error.
#'
#' @param x An object to check
#'
#' @param arg The name of the object; will be set to the name of the input if
#'   left `NULL`. This is useful for creating more informative errors.
#'
#' @return The input, `x`
#'
#' @keywords internal
#'
#' @export
assert_dataframe <- function(x, arg = NULL) {

  if (is.null(arg)) {
    arg <- rlang::ensym(x) %>% rlang::as_label()
  }

  if (is.data.frame(x)) {
    x
  } else {
    rlang::abort(
      paste(arg, "must be a data frame or data frame extension"),
      class = "error_assert_dataframe"
    )
  }
}

#' Assert That a Data Frame Selection Matches the Specified Type and Length
#'
#' `assert_cols()` checks whether columns selected using `...` match the given
#' prototype and/or number of columns. If so, it returns the data frame;
#' otherwise, it throws an error.
#'
#' @param .data A data frame or data frame extension (e.g. a `tibble`)
#'
#' @param ... `<tidy-select>` One or more selection specifications for a set of
#'   columns in the dataframe
#'
#' @param ptype The prototype that the selected columns should match
#'
#' @param n The number of columns that should be selected
#'
#' @return The input object, `.data`
#'
#' @keywords internal
#'
#' @export
assert_cols <- function(.data, ..., ptype = NULL, n = NULL) {

  assert_dataframe(.data)

  data_ptype <- vec_ptype(.data)

  remove(.data)

  selection <- dplyr::select(data_ptype, ...)

  dplyr::mutate(
    selection,
    dplyr::across(.fns = ~ vec_assert(.x, ptype = ptype))
  )

  if (!is.null(n) & length(selection) != n) {
    rlang::abort(
      paste(n, "columns must be selected", class = "assert_cols")
    )
  }
}

#' Flexible Error Assertion
#'
#' `assert_all()` and `assert_any()` are functions that require all or any of
#' the expressions in `...` to be `TRUE`. They are more flexible versions of
#' \code{\link[base:stopifnot]{stopifnot()}} that also provide the conditioning
#' system benefits of \code{\link[rlang:abort]{abort()}}. They are powered
#' (and generalized) by `assert()`.
#'
#' `assert()` evaluates logical expressions and throws an error if the
#' conditions specified by `reduce` are not met. It allows any combination of
#' logical conditions (not just "all `TRUE`" or "any `TRUE`") and provides the
#' \conditioning system benefits of \code{\link[rlang:abort]{abort()}}.
#'
#' @inheritParams rlang::abort
#'
#' @param ... Unnamed expressions that describe the conditions to be tested
#'
#' @param data Additional data to be stored in the condition object
#'
#' @param reduce A function that takes a vector of logical values as an argument
#'   and returns a single logical value. `reduce` is given the results of the
#'     expressions in `...` and decides whether the assertion passed or failed.
#'     The default is \code{\link[base:all]{all()}} (i.e. all expressions must
#'     be `TRUE`), but any function that meets the criteria above is acceptable.
#'
#' @return `TRUE` if the assertions evaluate to `TRUE`, otherwise an error
#'   condition
#'
#' @aliases assert_any assert_all
#'
#' @export
assert <- function(
  ...,
  message = NULL,
  class = NULL,
  data = NULL,
  trace = NULL,
  parent = NULL,
  reduce = base::all
) {
  # Store dots in list
  dots <- rlang::list2(...)

  # Check that dots are all scalar logical
  dots_are_lgl <- vapply(
    dots,
    FUN = is.logical,
    FUN.VALUE = logical(1L)
  )
  if (!all(dots_are_lgl)) {
    rlang::abort("All expressions in `...` must evaluate to `TRUE` or `FALSE`")
  }

  # Reduce dots to one scalar logical
  success <- reduce(unlist(dots))

  # Check that `success` is scalar logical
  if (!rlang::is_scalar_logical(success)) {
    rlang::abort(
      "`reduce` must be a function that accepts a logical vector returns a single logical value"
    )
  }

  # If successful, return `success`
  # Else throw an error
  if (success) {
    invisible(success)
  } else {
    msg <- create_assert_msg(
      ...,
      message = message,
      class = class,
      reduce = reduce
    )
    rlang::abort(
      message = msg,
      class = class,
      !!!data,
      trace = trace,
      parent = parent
    )
  }
}

#' @rdname assert
#'
#' @export
assert_all <- function(
  ...,
  message = NULL,
  class = NULL,
  data = NULL,
  trace = NULL,
  parent = NULL
) {

  if (is.null(trace)) {
    trace <- rlang::trace_back()
  }

  assert(
    ...,
    message = message,
    class = class,
    data = data,
    trace = trace,
    parent = parent,
    reduce = base::all
  )
}

#' @rdname assert
#'
#' @export
assert_any <- function(
  ...,
  message = NULL,
  class = NULL,
  data = NULL,
  trace = NULL,
  parent = NULL
) {

  if (is.null(trace)) {
    trace <- rlang::trace_back()
  }

  assert(
    ...,
    message = message,
    class = class,
    data = data,
    trace = trace,
    parent = parent,
    reduce = base::any
  )

}

#' Create an Error Message for `assert()`
#'
#' `create_assert_msg()` creates an error message for a failure in
#' \code{\link[coviData:assert]{assert()}} if both `message` and `class` are
#' `NULL`.
#'
#' @inheritParams assert
#'
#' @return An error message as a character string. If `message` is provided,
#' then `message`; if `class` is provided, then `""`. Otherwise, a string
#' listing the failed assertions in `...`
#'
#' @noRd
create_assert_msg <- function(..., message, class, reduce) {

  if (!is.null(message)) {
    return(message)
  } else if (!is.null(class)) {
    return("")
  }

  dots <- rlang::list2(...)

  exprs <- rlang::enexprs(...)

  labels <- vapply(
    exprs,
    FUN = rlang::expr_label,
    FUN.VALUE = character(1L)
  )

  failures <- labels[!unlist(dots)]

  names(failures) <- rep("x", length(failures))

  bullets <- rlang::format_error_bullets(failures)

  if (identical(reduce, base::any)) {
    one_of <- " one of "
  } else {
    one_of <- ""
  }

  paste0(
    "An assertion in this code has failed. To pass this assertion, ",
    one_of,
    "the following must be `TRUE` (but is not):\n",
    bullets,
    "\n"
  )
}
