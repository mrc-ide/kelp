test_that("low level client can send GET requests", {
  test_seaweed_available()
  client <- seaweed_client$new(seaweed_master_url)
  res <- client$GET("cluster/status")
  expect_equal(res$request$method, "GET")
  expect_equal(httr::status_code(res), 200)
  content <- httr::content(res)
  ## Note MaxVolumeId not always returned
  expect_true(all(names(content %in% c("IsLeader", "Leader", "MaxVolumeId"))))
  expect_equal(content$IsLeader, TRUE)
  expect_equal(content$Leader, "seaweed_master:9333")
})

test_that("low level client can send POST requests", {
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
  client <- seaweed_client$new("httpbin.org")
  res <- client$DELETE("delete")
  expect_equal(res$request$method, "DELETE")
  expect_equal(httr::status_code(res), 200)
  content <- httr::content(res)
})

test_that("low level client can send generic requests", {
  client <- seaweed_client$new("test")
  res <- client$request(httr::GET, "httpbin.org/get")
  expect_equal(httr::status_code(res), 200)
})
