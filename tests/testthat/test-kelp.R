test_that("file can be created, read and deleted from SeaweedFS", {
  test_seaweed_available()
  fs <- kelp$new(seaweed_master_url)

  ## File can be written
  t <- tempfile()
  writeLines("test file", t)
  res <- fs$create(t)
  expect_equal(httr::status_code(res), 201L)
  content <- httr::content(res)
  expect_setequal(names(content),
                  c("eTag", "fid", "fileName", "fileUrl", "size"))
  expect_match(content$fid, "\\d,[A-Za-z0-9]{10}")
  expect_equal(content$fileName, basename(t))
  ## fileUrl is <ip-address>:<port>/<fid>
  expect_match(content$fileUrl, "[\\d\\.]+:\\d{4}/\\d,[A-Za-z0-9]{10}",
               perl = TRUE)
  expect_equal(content$size, 10)

  ## File can be read
  read <- fs$read(content$fileUrl)
  expect_equal(httr::status_code(read), 200L)
  read_content <- httr::content(read, encoding = "UTF-8")
  expect_equal(read_content, "test file\n")

  ## File can be deleted
  del <- fs$delete(content$fileUrl)
  expect_equal(httr::status_code(del), 202L)

  ## File no longer exists
  read <- fs$read(content$fileUrl)
  expect_equal(httr::status_code(read), 404L)
  expect_equal(httr::content(read), raw(0))
})
