#' Seaweed Volume API
#'
#' @description
#' Create a `seaweed_volume` object for interacting with a SeaweedFS volume
#'
#' See <https://github.com/chrislusf/seaweedfs/wiki/Volume-Server-API> for
#' details
#'
#' @keywords export
seaweed_volume <- R6::R6Class(
  "seaweed_volume",
  cloneable = FALSE,

  public = list(
    #' @field client A `seaweed_client` object for sending requests
    client = NULL,

    #' @description
    #' Create client object for sending http requests to seaweed volume
    #'
    #' @param seaweed_url Root URL of Seaweed volume
    #'
    #' @return A new `seaweed_volume` object
    initialize = function(seaweed_url) {
      self$client <- seaweed_client$new(seaweed_url)
    },

    #' @description
    #' Upload a file to SeaweedFS
    #'
    #' @param path Path to file to upload
    #' @param fid The file ID from SeaweedFS. See
    #' \href{../../kelp/html/seaweed_master.html#method-assign}{
    #' \code{seaweed_master$assign()}}
    #' to get a file ID. Or use
    #' \href{../../kelp/html/seaweed_master.html#method-upload_file}{
    #' \code{seaweed_master$upload_file()}} to upload
    #' directly.
    #'
    #' @return Size of uploaded file
    upload_file = function(fid, path) {
      if (!file.exists(path)) {
        stop(sprintf("File at %s doesn't exist. Cannot upload.", path))
      }
      self$client$POST(fid, body = list(
        file = httr::upload_file(path)))
    },

    #' @description
    #' Upload an arbitrary R object to SeaweedFS
    #'
    #' @param object The object to upload
    #' @param fid The file ID from SeaweedFS. See
    #' \href{../../kelp/html/seaweed_master.html#method-assign}{
    #' \code{seaweed_master$assign()}}
    #' to get a file ID. Or use
    #' \href{../../kelp/html/seaweed_master.html#method-upload_file}{
    #' \code{seaweed_master$upload_file()}} to upload
    #' directly.
    #'
    #' @return Size of uploaded object
    upload_object = function(fid, object) {
      bin <- object_to_bin(object)
      self$client$POST(fid, body = list(
        file = bin))
    },

    #' @description
    #' Read file from SeaweedFS into R
    #'
    #' @param fid SeaweedFS file ID to read
    #'
    #' @return The file contents
    read = function(fid) {
      self$client$GET(fid)
    },

    #' @description
    #' Download file from SeaweedFS to a local path
    #'
    #' @param fid SeaweedFS file ID to download
    #' @param path Local file path to save to
    #'
    #' @return The file path written to
    download_file = function(fid, path = tempfile()) {
      self$client$GET(fid, httr::write_disk(path))
      path
    },

    #' @description
    #' Download and deserialize R object from SeaweedFS
    #'
    #' @param fid SeaweedFS file ID to download
    #'
    #' @return The deserialized object
    download_object = function(fid) {
      data <- self$read(fid)
      bin_to_object(data)
    },

    #' @description
    #' Delete a file from SeaweedFS
    #'
    #' @param fid SeaweedFS file ID to delete
    #'
    #' @return Nothing, called for side effects
    delete = function(fid) {
      self$client$DELETE(fid)
      invisible(TRUE)
    }
  )
)
