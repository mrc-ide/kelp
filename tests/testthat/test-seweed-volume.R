test_that("file can be written and read from volume", {
  test_seaweed_available()
  master <- seaweed_master$new(seaweed_master_url)
  key <- master$assign()
  volume <- seaweed_volume$new(seaweed_volume_url)

  ## File can be written
  t <- tempfile()
  writeLines("test file", t)
  res <- volume$upload_file(key$fid, t)
  expect_setequal(names(res), c("eTag", "name", "mime", "size"))
  expect_equal(res$name, basename(t))
  expect_equal(res$size, 10)
  expect_equal(res$mime, "text/plain")

  read <- volume$read(key$fid)
  expect_equal(read, "test file\n")
})

test_that("file in SeaweedFS can be downloaded", {
  test_seaweed_available()
  master <- seaweed_master$new(seaweed_master_url)
  key <- master$assign()
  volume <- seaweed_volume$new(seaweed_volume_url)

  t <- tempfile()
  writeLines("test file", t)
  res <- volume$upload_file(key$fid, t)
  expect_setequal(names(res), c("eTag", "name", "mime", "size"))

  file <- volume$download_file(key$fid)
  expect_true(file.exists(file))
  expect_equal(readLines(file), "test file")
})

test_that("trying to upload non-exitent file errors", {
  test_seaweed_available()
  master <- seaweed_master$new(seaweed_master_url)
  key <- master$assign()
  volume <- seaweed_volume$new(seaweed_volume_url)

  expect_error(volume$upload_file(key$fid, "/path/to/file"),
               "File at /path/to/file doesn't exist. Cannot upload.")
})

test_that("read non-exitent file throws error", {
  test_seaweed_available()
  master <- seaweed_master$new(seaweed_master_url)
  key <- master$assign()
  volume <- seaweed_volume$new(seaweed_volume_url)

  expect_error(volume$read(key$fid), "Client error: (404) Not Found",
               fixed = TRUE)
})

test_that("download non-exitent file throws error", {
  test_seaweed_available()
  master <- seaweed_master$new(seaweed_master_url)
  key <- master$assign()
  volume <- seaweed_volume$new(seaweed_volume_url)

  expect_error(volume$download_file(key$fid), "Client error: (404) Not Found",
               fixed = TRUE)
})

test_that("file can be deleted", {
  test_seaweed_available()
  master <- seaweed_master$new(seaweed_master_url)
  key <- master$assign()
  volume <- seaweed_volume$new(seaweed_volume_url)

  t <- tempfile()
  writeLines("test file", t)
  res <- volume$upload_file(key$fid, t)

  read <- volume$read(key$fid)
  expect_equal(read, "test file\n")

  volume$delete(key$fid)
  expect_error(volume$read(key$fid), "Client error: (404) Not Found",
               fixed = TRUE)
})

test_that("arbitrary R object can be stored and retrieved", {
  test_seaweed_available()
  master <- seaweed_master$new(seaweed_master_url)
  key <- master$assign()
  volume <- seaweed_volume$new(seaweed_volume_url)

  obj <- list(x = 1, y = 2)
  volume$upload_object(key$fid, obj)
  out <- volume$download_object(key$fid)
  expect_equal(out, obj)

  fid <- volume$upload_object(key$fid, mtcars)
  out <- volume$download_object(key$fid)
  expect_equal(out, mtcars)
})
