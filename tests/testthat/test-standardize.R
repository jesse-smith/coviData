context("Standardizing Vectors")


test_that("the default is title case", {
  expect_equal(standardize_string("jesse smith"), "Jesse Smith")
})

test_that("spacing is respected", {
  expect_equal(standardize_string("J Esse Smith"), "J Esse Smith")
})

test_that("numbers are replaced with space", {
  expect_equal(standardize_string("J3ss3 Sm1th"), "J Ss Sm Th")
})

test_that("default is vectorized", {
  expect_length(standardize_string(c("Bob", "Joe")), n = 2)
})
