# `str_replace_brackets()`
test_that("square brackets are replaced with parentheses", {
  expect_equal(
    str_replace_brackets(
      "This ][sentence][ contains a lot of ']' and '[' [brackets]"
    ),
    "This )(sentence)( contains a lot of ')' and '(' (brackets)"
  )
})

# `str_replace_braces()`
test_that("curly braces are replaced with parentheses", {
  expect_equal(
    str_replace_braces(
      "This }{sentence}{ contains a lot of '}' and '{' {braces}"
    ),
    "This )(sentence)( contains a lot of ')' and '(' (braces)"
  )
})

# `str_remove_parenthetic()`
test_that("parenthetic expressions are removed", {
  expect_equal(
    str_remove_parenthetic(
      "Hi, Alice (and Bob)!"
    ),
    "Hi, Alice !"
  )

  expect_equal(
    str_remove_parenthetic(
      "(This )(sentence)( contains a lot of ')' and '(' (parentheses)"
    ),
    ""
  )
})

# `str_remove_ordinal()`
test_that("ordinal numbers are removed", {
  expect_equal(
    str_remove_ordinal(
      "This is her 2nd birthday; here's to a90th!"
    ),
    "This is her  birthday; here's to a90th!"
  )
})

# `str_remove_numeric()`
test_that("numeric expressions are removed", {
  expect_equal(
    str_remove_numeric(
      "Pi isn't equal to 3.14, but it's within a few 1,000.ths."
    ),
    "Pi isn't equal to , but it's within a few .ths."
  )
})

# `str_trim_dashes()`
test_that("leading and trailing dashes are removed", {
  expect_equal(
    str_trim_dashes("- wait- what happened? I missed it--"),
    " wait- what happened? I missed it"
  )
})

# `str_invert_na()`
test_that("NA replacements are inverted", {
  expect_equal(
    str_invert_na(c("na", "NA_2", "nA_30", "Na_025", "Nate")),
    c(NA_character_, NA_character_, NA_character_, NA_character_, "Nate")
  )
})
