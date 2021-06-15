context("util")

test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})

test_that("binrary serialisation is transitive", {
  f <- function(x, identical = TRUE) {
    y <- bin_to_object(object_to_bin(x))
    expect_equal(x, y, check.attributes = FALSE)
  }
  f(NULL)
  f(1)
  f(f, identical = FALSE)
  f(1:10)

  ## In contrast with string serialization above, binary serialization
  ## is exact (as well as being about 10x faster which is nice).
  set.seed(1)
  x <- runif(100)
  f(x)
})
