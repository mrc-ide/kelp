test_seaweed_available <- function(url = "localhost:9333") {
  client <- seaweed_client$new(url)
  available <- tryCatch({
    res <- client$GET("cluster/status")
    identical(httr::status_code(res), 200L)
  }, error = function(e) FALSE)
  if (!available) {
    testthat::skip("Skipping test as seaweed is not available")
  }
  invisible(available)
}

seaweed_master_url <- "localhost:9333"
