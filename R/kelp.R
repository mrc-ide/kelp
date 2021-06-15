#' Create a kelp object for interacting with SeaweedFS.
#'
#' This is a slightly higher level abstraction than [seaweed_master()] and
#' [seaweed_volume()].
#'
#' @export
#' @importFrom R6 R6Class
kelp <- R6::R6Class(
  "kelp",
  cloneable = FALSE,

  public = list(
    #' @description
    #' Create client object for sending http requests to seaweed
    #'
    #' @param seaweed_url Root URL of Seaweed
    #'
    #' @return A new `kelp` object
    initialize = function(seaweed_url) {
      private$master <- seaweed_master$new(seaweed_url)
    },

    #' @description
    #' Upload file to SeaweedFS
    #'
    #' @param path Path to file to be uploaded
    #' @param collection Collection name, acts as a namespace for files.
    #'
    #' @return The uploaded file ID.
    upload_file = function(path, collection = NULL) {
      key <- private$master$assign(collection = collection)
      volume <- seaweed_volume$new(key$publicUrl)
      volume$upload_file(key$fid, path)
      key$fid
    },

    #' @description
    #' Download file from SeaweedFS
    #'
    #' If called with an `fid` matching an R object uploaded via
    #' \href{#method-upload_object}{\code{kelp$upload_object()}}
    #' then this will download raw bytes from SeaweedFS which can
    #' be converted back to R object at later point using `unserialize`
    #' or `readRDS`.
    #'
    #' @param fid SeaweedFS file ID to read
    #' @param path Path to download file to
    #' @param collection Optional collection name this file belongs to.
    #'
    #' @return The file contents
    download_file = function(fid, path = tempfile(), collection = NULL) {
      volumes <- private$master$lookup(fid, collection)
      ## Download from 1st returned volume for now
      volumes[[1]]$download_file(fid, path)
    },

    #' @description
    #' Upload arbitrary R object to SeaweedFS.
    #'
    #' This serializes R object to raw vector of bytes and then
    #' saves bytes to SeaweedFS. To recover R object as saved use
    #' \href{#method-download_object}{\code{seaweed_volume$download_object()}}
    #'
    #' @param object Object to be uploaded
    #' @param collection Collection name, acts as a namespace.
    #'
    #' @return The uploaded file ID.
    upload_object = function(object, collection = NULL) {
      key <- private$master$assign(collection = collection)
      volume <- seaweed_volume$new(key$publicUrl)
      volume$upload_object(key$fid, object)
      key$fid
    },

    #' @description
    #' Download arbitrary R object from SeaweedFS
    #'
    #' If data downloaded from SeaweedFS is not unserializable (i.e. it was
    #' written using \href{#method-upload_file}{
    #'   \code{kelp$upload_file()}}) then
    #' this will throw an error. See \href{#method-download_file}{
    #'   \code{kelp$download_file()}} to download as a file.
    #'
    #' @param fid SeaweedFS file ID to download
    #' @param collection Optional collection name this file belongs to.
    #'
    #' @return The R object.
    download_object = function(fid, collection = NULL) {
      volumes <- private$master$lookup(fid, collection)
      volumes[[1]]$download_object(fid)
    },

    #' @description
    #' Delete file from SeaweedFS
    #'
    #' @param fid SeaweedFS file ID to delete
    #' @param collection Optional collection name this file belongs to.
    #'   This helps speedup lookup - only the single fid will be deleted.
    #'   See \href{#method-delete_collection}{
    #'   \code{kelp$delete_collection()}} to remove an entire
    #'   collection.
    #'
    #' @return Nothing, called for side effects
    delete = function(fid, collection = NULL) {
      volumes <- private$master$lookup(fid, collection)
      for (volume in volumes) {
        volume$delete(fid)
      }
      invisible(TRUE)
    },

    #' @description
    #' Delete a collection of files
    #'
    #' @param collection Collection name.
    #'
    #' @return Nothing, called for side effects
    delete_collection = function(collection) {
      private$master$delete_collection(collection)
    }
  ),

  private = list(
    # A `seaweed_master` object for sending requests
    master = NULL
  )
)
