test_seaweed_available <- function(url = "localhost:9333") {
  client <- seaweed_client$new(url)
  res <- client$GET("cluster/status")
  available <- identical(httr::status_code(res), 200L)
  if (!available) {
    testthat::skip("Skipping test as seaweed is not available")
  }
  invisible(available)
}

seaweed_master_url <- "localhost:9333"
