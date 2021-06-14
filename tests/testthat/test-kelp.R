test_that("file can be uploaded, read and deleted from SeaweedFS", {
  test_seaweed_available()
  fs <- kelp$new(seaweed_master_url)

  ## File can be written
  t <- tempfile()
  writeLines("test file", t)
  res <- fs$upload_file(t)
  expect_match(res, "\\d+,[A-Za-z0-9]{10}")

  ## File can be downloaded
  download <- fs$download_file(res)
  expect_true(file.exists(download))
  expect_equal(readLines(download), "test file")

  ## File can be deleted
  fs$delete(res)

  ## File no longer exists
  error <- expect_error(fs$download_file(res), "Client error: (404) Not Found",
                        fixed = TRUE)
})

test_that("collection can be deleted", {
  test_seaweed_available()
  fs <- kelp$new(seaweed_master_url)

  ## Write files
  t <- tempfile()
  writeLines("test file", t)
  res1 <- fs$upload_file(t, collection = "example")
  res2 <- fs$upload_file(t, collection = "example")

  expect_no_error(fs$download_file(res1))
  expect_no_error(fs$download_file(res2))

  ## Delete
  fs$delete_collection("example")

  expect_error(fs$download_file(res1))
  expect_error(fs$download_file(res2))
})

test_that("arbitrary R object can be stored and retrieved", {
  test_seaweed_available()
  fs <- kelp$new(seaweed_master_url)

  obj <- list(x = 1, y = 2)
  fid <- fs$upload_object(obj)
  out <- fs$download_object(fid)
  expect_equal(out, obj)

  fid <- fs$upload_object(mtcars)
  out <- fs$download_object(fid)
  expect_equal(out, mtcars)
})
