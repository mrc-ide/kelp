test_that("test kelp fake and kelp advertise same interface", {
  fake <- kelp_fake$new(seaweed_master_url)
  real <- kelp$new(seaweed_master_url)

  fake_funcs <- ls(fake)
  kelp_funcs <- ls(real)
  expect_equal(fake_funcs, kelp_funcs)
  for (name in fake_funcs) {
    fake_memer <- fake[[name]]
    kelp_member <- real[[name]]
    if (is.function(fake_memer)) {
      expect_equal(formals(fake_memer), formals(kelp_member))
    } else {
      expect_equal(fake_memer, kelp_member)
    }
  }
})

test_that("test fake: can upload, read and delete a file", {
  test_seaweed_available()
  fs <- kelp_fake$new(seaweed_master_url)

  ## File can be written
  t <- tempfile()
  writeLines("test file", t)
  res <- fs$upload_file(t)
  expect_match(res, "^\\d+,[A-Za-z0-9]{10}$")

  ## File can be downloaded
  download <- fs$download_file(res)
  expect_true(file.exists(download))
  expect_equal(readLines(download), "test file")

  ## File can be deleted
  fs$delete(res)

  ## File no longer exists
  error <- expect_error(fs$download_file(res))
})

test_that("test fake: collection can be deleted", {
  test_seaweed_available()
  fs <- kelp_fake$new(seaweed_master_url)

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

test_that("test fake: arbitrary R object can be stored and retrieved", {
  test_seaweed_available()
  fs <- kelp_fake$new(seaweed_master_url)

  obj <- list(x = 1, y = 2)
  fid <- fs$upload_object(obj)
  out <- fs$download_object(fid)
  expect_equal(out, obj)

  fid <- fs$upload_object(mtcars)
  out <- fs$download_object(fid)
  expect_equal(out, mtcars)
})

test_that("test fake: raw bytes can be uploaded and downloaded", {
  test_seaweed_available()
  fs <- kelp_fake$new(seaweed_master_url)
  bytes <- object_to_bin(list(x = 1, y = 2))

  fid <- fs$upload_raw(bytes)
  out <- fs$download_raw(fid)
  expect_equal(out, bytes)
})

test_that("test fake: uploaded raw bytes can be downloaded as object", {
  test_seaweed_available()
  fs <- kelp_fake$new(seaweed_master_url)
  bytes <- object_to_bin(list(x = 1, y = 2))

  fid <- fs$upload_raw(bytes)
  out <- fs$download_object(fid)
  expect_equal(out, list(x = 1, y = 2))
})
