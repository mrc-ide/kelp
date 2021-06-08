test_that("low level client can send GET requests", {
  test_seaweed_available()
  client <- seaweed_client$new(seaweed_master_url)
  res <- client$GET("cluster/status")
  expect_equal(res$request$method, "GET")
  expect_equal(httr::status_code(res), 200)
  content <- httr::content(res)
  expect_equal(content, list(
    IsLeader = TRUE,
    Leader = "master:9333"
  ))
})

test_that("low level client can send POST requests", {
  test_seaweed_available()
  client <- seaweed_client$new("httpbin.org")
  res <- client$POST("post", body = list(test = "example"))
  expect_equal(res$request$method, "POST")
  expect_equal(httr::status_code(res), 200)
  content <- httr::content(res)
  expect_equal(content$form, list(
    test = "example"
  ))
})

test_that("low level client can send DELETE requests", {
  test_seaweed_available()
  client <- seaweed_client$new("httpbin.org")
  res <- client$DELETE("delete")
  expect_equal(res$request$method, "DELETE")
  expect_equal(httr::status_code(res), 200)
  content <- httr::content(res)
})
