#' Create Column-Wise Randomized Test Data for Non-Statistical Validation
#'
#' `randomize()` draws `n` samples from the unique values in each column
#' of a data frame and returns the randomized data. This destroys all
#' statistical information in the dataset, both univariate and multivariate.
#' However, since the set of possible output values is the same as the input
#' values, the minimum and maximum of numeric columns will be the same, as will
#' the unique values of all columns (if `n` is larger than the number of
#' observations).
#'
#' `randomize()` can perform up- and down-sampling of the input data.
#' Downsampling is simple random sampling without replacement. Upsampling
#' samples without replacement up to the size of the input data, then samples
#' with replacement for the remaining observations. This approach ensures that
#' all values from the input data appear at least once if `n` is greater than or
#' equal to the number of observations in the data.
#'
#' A stratified version that restricts randomization to occur *within* strata
#' can be obtained by grouping the data using
#' \code{\link[dplyr:group_by]{group_by()}} prior to calling `randomize()`. In
#' this case, the relative proportions of the groups within the dataset remain
#' the same; this allows the user to retain portions of the data's structure
#' while destroying the remaining information.
#'
#' Note that the above only provides anonymity when the number of unique values for
#' quasi-identifiers (within each group) is large and unique idenfiers are handled separately.
#' Also note that when groups are defined, information both *within and between* grouping
#' variables *will not be modified*.
#'
#' @param .data A data frame or data frame extension (e.g. a
#'   \code{\link[tibble:tbl_df-class]{tibble}})
#'
#' @param n The desired number of observations in the returned dataset; the
#'   default is the number of observations in the input
#'
#' @param .groups How to handle grouping variables; see the `.groups` parameter
#'   documentation in \code{\link[dplyr:summarise]{summarize()}} for more
#'   information
#'
#' @return A `tibble` containing the randomized test data
#'
#' @export
randomize <- function(.data, n = NULL, .groups = NULL) {

  assertthat::assert_that(
    is.data.frame(.data),
    msg = paste0(
      "`.data` must be a data frame or data frame extension"
    )
  )

  n_data <- vctrs::vec_size(.data)

  if (rlang::is_empty(n)) {
    n <- n_data
  }

  assertthat::assert_that(
    n[[1]] >= 0 & rlang::is_scalar_integerish(n),
    msg = "`n` must be a non-negative scalar integer"
  )

  .data %>%
    dplyr::summarize(
      dplyr::across(
        .fns = ~ sample_unique(.x, n = round(n * dplyr::n() / n_data))
      ),
      .groups = .groups
    ) %>%
    purrr::when(
      NROW(.) > n ~ vec_sample(., n = n),
      NROW(.) < n ~ dplyr::bind_rows(., vec_sample(., n = n - NROW(.)))
      ~ .
    ) %>%
    dplyr::as_tibble()

}

#' Sample from Unique Values of Data
#'
#' `sample_unique()` samples from the unique values in the input. If
#' `size` is greater than or equal to the number of input observations, it
#' ensures that each observations is present at least once. All sampling steps
#' are uniform random, but the output will only be uniform random for `n` <=
#' number of observations, or `n` >> number of observations. This is due to the
#' switch from sampling with replacement to sampling without replacement once
#' the sample size is larger than the number of observations.
#'
#' @param .x The data to sample
#'
#' @param n The number of samples to return
#'
#' @return The shuffled input and additional observations
#'
#' @export
sample_unique <- function(.x, n) {

  assertthat::assert_that(
    n[[1]] >= 0 & rlang::is_scalar_integerish(n),
    msg = "`n` must be a non-negative scalar integer"
  )

  unique_x <- vctrs::vec_unique(.x)
  unique_n <- vctrs::vec_unique_count(.x)

  # The input may be very large, so free up for garbage collection ASAP
  remove(.x)

  if (n <= unique_n) {
    vec_sample(unique_x, n = n, replace = FALSE)
  } else {
    unique_x %>%
      vctrs::vec_c(vec_sample(., n = n - unique_n, replace = TRUE)) %>%
      vec_sample(n = n, replace = FALSE)
  }
}

#' Re-implementation of `base::sample()` Using the vctrs Package
#'
#' \code{\link[base:sample]{sample()}} behaves unexpectedly when called on
#' objects other than atomic vectors. This re-implementation has identical logic
#' but uses the \href{https://vctrs.r-lib.org}{vctrs} package to generalize to
#' other inputs. Parameters are modeled after
#' \code{\link[dplyr:slice]{slice_sample()}}.
#'
#' @param .x The data to sample
#'
#' @param n The number of samples to return
#'
#' @param replace Should sampling be performed with (`TRUE`) or without
#'   (`FALSE`, the default) replacement?
#'
#' @param weight_by Sampling weights. This must evaluate to a vector of
#'   non-negative numbers the same length as the input. Weights are
#'   automatically standardised to sum to 1.
#'
#' @export
vec_sample <- function(.x, n, replace = FALSE, weight_by = NULL) {

  assertthat::assert_that(
    n[[1]] >= 0 & rlang::is_scalar_integerish(n),
    msg = "`n` must be a non-negative scalar integer"
  )

  wt_null <- is.null(weight_by)
  wt_empty <- vctrs::vec_is_empty(weight_by)
  if (wt_empty & !wt_null) {
    weight_by <- NULL
  } else if (!wt_empty) {
    weight_by <- weight_by / sum(weight_by)
  }

  i <- sample(
    vctrs::vec_seq_along(.x),
    size = n,
    replace = replace,
    prob = weight_by
  )

  vctrs::vec_slice(.x, i = i)
}
