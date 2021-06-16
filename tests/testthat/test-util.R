test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})

test_that("binary serialisation is transitive", {
  f <- function(x, ignore_function_env = FALSE) {
    y <- bin_to_object(object_to_bin(x))
    expect_equal(x, y, ignore_attr = FALSE,
                 ignore_function_env = ignore_function_env)
  }
  f(NULL)
  f(1)
  f(f, ignore_function_env = TRUE)
  f(1:10)

  set.seed(1)
  x <- runif(100)
  f(x)
})
