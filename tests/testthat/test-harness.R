test_that("test harness and kelp advertise same interface", {
  harness <- kelp_harness$new(seaweed_master_url)
  real <- kelp$new(seaweed_master_url)

  harness_funcs <- ls(harness)
  kelp_funcs <- ls(real)
  expect_equal(harness_funcs, kelp_funcs)
  for (name in harness_funcs) {
    harness_member <- harness[[name]]
    kelp_member <- real[[name]]
    if (is.function(harness_member)) {
      expect_equal(args(harness_member), args(kelp_member))
    } else {
      expect_equal(harness_member, kelp_member)
    }
  }
})

test_that("test harness: can upload, read and delete a file", {
  test_seaweed_available()
  fs <- kelp_harness$new(seaweed_master_url)

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
  error <- expect_error(fs$download_file(res))
})

test_that("test harness: collection can be deleted", {
  test_seaweed_available()
  fs <- kelp_harness$new(seaweed_master_url)

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

test_that("test harness: arbitrary R object can be stored and retrieved", {
  test_seaweed_available()
  fs <- kelp_harness$new(seaweed_master_url)

  obj <- list(x = 1, y = 2)
  fid <- fs$upload_object(obj)
  out <- fs$download_object(fid)
  expect_equal(out, obj)

  fid <- fs$upload_object(mtcars)
  out <- fs$download_object(fid)
  expect_equal(out, mtcars)
})

test_that("test harness: raw bytes can be uploaded and downloaded", {
  test_seaweed_available()
  fs <- kelp_harness$new(seaweed_master_url)
  bytes <- object_to_bin(list(x = 1, y = 2))

  fid <- fs$upload_raw(bytes)
  out <- fs$download_raw(fid)
  expect_equal(out, bytes)
})

test_that("test harness: uploaded raw bytes can be downloaded as object", {
  test_seaweed_available()
  fs <- kelp_harness$new(seaweed_master_url)
  bytes <- object_to_bin(list(x = 1, y = 2))

  fid <- fs$upload_raw(bytes)
  out <- fs$download_object(fid)
  expect_equal(out, list(x = 1, y = 2))
})
