test_that("low level client can send GET requests", {
  test_seaweed_available()
  client <- seaweed_client$new(seaweed_master_url)
  res <- client$GET("cluster/status")
  ## Note MaxVolumeId not always returned
  expect_true(all(names(res %in% c("IsLeader", "Leader", "MaxVolumeId"))))
  expect_equal(res$IsLeader, TRUE)
  expect_equal(res$Leader, "seaweed_master:9333")
})

test_that("low level client can send POST requests", {
  client <- seaweed_client$new("http://httpbin.org")
  res <- client$POST("post", body = list(test = "example"))
  expect_equal(res$form, list(
    test = "example"
  ))
})

test_that("low level client can send DELETE requests", {
  client <- seaweed_client$new("http://httpbin.org")
  res <- client$DELETE("delete")
  expect_equal(res$url, "http://httpbin.org/delete")
})

test_that("low level client can send generic requests", {
  client <- seaweed_client$new("test")
  res <- client$request(httr::GET, "http://httpbin.org/get")
  expect_equal(res$url, "http://httpbin.org/get")
})
