test_that("file can be uploaded, read and deleted from SeaweedFS", {
  test_seaweed_available()
  fs <- kelp$new(seaweed_master_url)

  ## File can be written
  t <- tempfile()
  writeLines("test file", t)
  res <- fs$upload(t)
  expect_setequal(names(res),
                  c("eTag", "fid", "fileName", "fileUrl", "size"))
  expect_match(res$fid, "\\d,[A-Za-z0-9]{10}")
  expect_equal(res$fileName, basename(t))
  ## fileUrl is <ip-address>:<port>/<fid>
  expect_match(res$fileUrl, "[\\d\\.]+:\\d{4}/\\d,[A-Za-z0-9]{10}",
               perl = TRUE)
  expect_equal(res$size, 10)

  ## File URL can be retrieved from fid
  file_url <- fs$file_url(res$fid)
  expect_equal(file_url, res$fileUrl)

  ## File can be read
  read <- fs$read(res$fid)
  expect_equal(read, "test file\n")

  ## File can be deleted
  del <- fs$delete(res$fid)
  expect_true(!is.null(del$size))

  ## File no longer exists
  error <- expect_error(fs$read(res$fid), "Empty error message")
  expect_equal(error$code, 404L)
})

test_that("json errors are handled", {
  test_seaweed_available()
  fs <- kelp$new(seaweed_master_url)
  error <- expect_error(fs$read("123"), "Unknown volume id")
  expect_equal(error$code, 404)
})

test_that("Upload throws error if file does not exist", {
  test_seaweed_available()
  fs <- kelp$new(seaweed_master_url)
  expect_error(fs$upload("not a path"),
               "File at not a path doesn't exist. Cannot upload.")
})
