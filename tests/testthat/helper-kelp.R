test_seaweed_available <- function(url = "http://localhost:9333") {
  client <- seaweed_client$new(url)
  available <- tryCatch({
    res <- client$GET("cluster/status")
    res$IsLeader
  }, error = function(e) FALSE)
  if (!available) {
    testthat::skip("Skipping test as seaweed is not available")
  }
  invisible(available)
}

seaweed_master_url <- "http://localhost:9333"

expect_no_error <- function(object) {
  expect_error(object, regexp = NA)
}
