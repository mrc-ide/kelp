test_that("file key can be assigned", {
  test_seaweed_available()
  master <- seaweed_master$new(seaweed_master_url)
  res <- master$assign()
  expect_setequal(names(res), c("fid", "url", "publicUrl", "count"))
  expect_match(res$fid, "^\\d+,[A-Za-z0-9]{10}$")
})

test_that("file key can be assigned with a collection", {
  test_seaweed_available()
  master <- seaweed_master$new(seaweed_master_url)
  res <- master$assign(collection = "test_collection")
  expect_setequal(names(res), c("fid", "url", "publicUrl", "count"))
  expect_match(res$fid, "^\\d+,[A-Za-z0-9]{10}$")
})

test_that("volumes can be located with fid", {
  test_seaweed_available()
  master <- seaweed_master$new(seaweed_master_url)
  key <- master$assign(collection = "test_collection")

  res <- master$lookup(key$fid)
  expect_length(res, 1)
  expect_s3_class(res[[1]], "seaweed_volume")

  ## can be located with collection
  collection_res <- master$lookup(key$fid, collection = "test_collection")
  expect_length(collection_res, 1)
  expect_s3_class(collection_res[[1]], "seaweed_volume")

  expect_equal(res[[1]]$client$seaweed_url,
               collection_res[[1]]$client$seaweed_url)

  ## errors if unknown collection
  expect_error(master$lookup(key$fid, collection = "test"),
               "volume id \\d+ not found")
})

test_that("volumes can be located with volume id", {
  test_seaweed_available()
  master <- seaweed_master$new(seaweed_master_url)
  key <- master$assign(collection = "test_collection")
  ## File id looks like <volume>,<file key><file cookie>
  volume_id <- strsplit(key$fid, ",")[[1]][1]

  res <- master$lookup(volume_id)
  expect_length(res, 1)
  expect_s3_class(res[[1]], "seaweed_volume")
})

test_that("api errors if volume cannot be found", {
  test_seaweed_available()
  master <- seaweed_master$new(seaweed_master_url)

  expect_error(master$lookup("100"), "volume id 100 not found")
})

test_that("collection can be deleted", {
  test_seaweed_available()
  master <- seaweed_master$new(seaweed_master_url)
  key1 <- master$assign(collection = "example")
  key2 <- master$assign(collection = "example")

  expect_no_error(master$lookup(key1$fid))
  expect_no_error(master$lookup(key2$fid))

  master$delete_collection("example")
  expect_error(master$lookup(key1$fid))
  expect_error(master$lookup(key2$fid))
})

test_that("trying to delete non-exitent collection throws error", {
  test_seaweed_available()
  master <- seaweed_master$new(seaweed_master_url)

  expect_error(master$delete_collection("not a collection"),
               "collection not a collection does not exist")
})

test_that("file can be directly uploaded to master", {
  test_seaweed_available()
  master <- seaweed_master$new(seaweed_master_url)

  ## File can be written
  t <- tempfile()
  writeLines("test file", t)
  res <- master$upload(t)
  expect_setequal(names(res),
                  c("eTag", "fid", "fileName", "fileUrl", "size"))
  expect_match(res$fid, "^\\d+,[A-Za-z0-9]{10}$")
  expect_equal(res$fileName, basename(t))
  ## fileUrl is <ip-address>:<port>/<fid>
  expect_match(res$fileUrl, "^[\\d\\.]+:\\d{4}/\\d+,[A-Za-z0-9]{10}$",
               perl = TRUE)
  expect_equal(res$size, 10)
})

test_that("uploading non-exitent file directly throws error", {
  test_seaweed_available()
  master <- seaweed_master$new(seaweed_master_url)
  expect_error(master$upload("/path/to/file"),
               "File at /path/to/file doesn't exist. Cannot upload.")
})
