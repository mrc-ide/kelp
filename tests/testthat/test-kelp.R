test_that("file can be uploaded, read and deleted from SeaweedFS", {
  test_seaweed_available()
  fs <- kelp$new(seaweed_master_url)

  ## File can be written
  t <- tempfile()
  writeLines("test file", t)
  res <- fs$upload(t)
  expect_match(res, "\\d+,[A-Za-z0-9]{10}")

  ## File can be downloaded
  download <- fs$download(res)
  expect_true(file.exists(download))
  expect_equal(readLines(download), "test file")

  ## File can be deleted
  fs$delete(res)

  ## File no longer exists
  error <- expect_error(fs$download(res), "Client error: (404) Not Found",
                        fixed = TRUE)
})

test_that("collection can be deleted", {
  test_seaweed_available()
  fs <- kelp$new(seaweed_master_url)

  ## Write files
  t <- tempfile()
  writeLines("test file", t)
  res1 <- fs$upload(t, collection = "example")
  res2 <- fs$upload(t, collection = "example")

  expect_no_error(fs$download(res1))
  expect_no_error(fs$download(res2))

  ## Delete
  fs$delete_collection("example")

  expect_error(fs$download(res1))
  expect_error(fs$download(res2))
})
