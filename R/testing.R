#' Create a kelp test harness object.
#'
#' Exposes same interface as [kelp()] but with mock responses, can be used
#' in place of a real Kelp object for testing when it might be quicker to
#' not have SeaweedFS itself running. Just backed by a local temp directory.
#'
#' @export
#' @importFrom R6 R6Class
kelp_harness <- R6::R6Class(
  "kelp_harness",
  cloneable = FALSE,

  public = list(
    #' @description
    #' Create a mock kelp client object for testing which doesn't require
    #' a Seaweed instance to be running.
    #'
    #' @param seaweed_url Root URL of Seaweed (can be anything for testing)
    #'
    #' @return A new `kelp_harness` object
    initialize = function(seaweed_url) {
      private$dir <- tempfile()
      dir.create(private$dir)
    },

    #' @description
    #' Upload file
    #'
    #' @param path Path to file to be uploaded
    #' @param collection Collection name, acts as a namespace for files.
    #'
    #' @return The uploaded file ID.
    upload_file = function(path, collection = NULL) {
      id <- sprintf("%s,%s", sample.int(9, 1), ids::random_id(bytes = 5))
      if (!is.null(collection)) {
        private$collections[[collection]] <-
          c(private$collections[[collection]], id)
      }
      file.copy(path, file.path(private$dir, id), overwrite = TRUE)
      id
    },

    #' @description
    #' Download file
    #'
    #' If called with an `id` matching an R object uploaded via
    #' \href{#method-upload_object}{\code{kelp$upload_object()}}
    #' then this will download raw bytes which can
    #' be converted back to R object at later point using `unserialize`
    #' or `readRDS`.
    #'
    #' @param id File ID to read
    #' @param path Path to download file to
    #' @param collection Optional collection name this file belongs to.
    #'
    #' @return The file contents
    download_file = function(id, path = tempfile(), collection = NULL) {
      if (!file.exists(file.path(private$dir, id))) {
        stop("File doesn't exist")
      }
      file.copy(file.path(private$dir, id), path)
      path
    },

    #' @description
    #' Upload arbitrary R object
    #'
    #' This serializes R object to raw vector of bytes and then
    #' saves bytes.
    #'
    #' @param object Object to be uploaded
    #' @param collection Collection name, acts as a namespace.
    #'
    #' @return The uploaded file ID.
    upload_object = function(object, collection = NULL) {
      id <- sprintf("%s,%s", sample.int(9, 1), ids::random_id(bytes = 5))
      if (!is.null(collection)) {
        private$collections[[collection]] <-
          c(private$collection[[collection]], id)
      }
      saveRDS(object, file.path(private$dir, id))
      id
    },

    #' @description
    #' Download arbitrary R object
    #'
    #' If data downloaded is not unserializable (i.e. it was
    #' written using \href{#method-upload_file}{
    #'   \code{kelp$upload_file()}}) then
    #' this will throw an error. See \href{#method-download_file}{
    #'   \code{kelp$download_file()}} to download as a file.
    #'
    #' @param id File ID to download
    #' @param collection Optional collection name this file belongs to.
    #'
    #' @return The R object.
    download_object = function(id, collection = NULL) {
      readRDS(file.path(private$dir, id))
    },

    #' @description
    #' Upload vector of raw bytes.
    #'
    #' @param raw Object to be uploaded
    #' @param collection Collection name, acts as a namespace.
    #'
    #' @return The uploaded file ID.
    upload_raw = function(raw, collection = NULL) {
      id <- sprintf("%s,%s", sample.int(9, 1), ids::random_id(bytes = 5))
      if (!is.null(collection)) {
        private$collections[[collection]] <-
          c(private$collection[[collection]], id)
      }
      writeBin(raw, file.path(private$dir, id))
      id
    },

    #' @description
    #' Download vector of raw bytes.
    #'
    #' @param id File ID to download
    #' @param collection Optional collection name this file belongs to.
    #'
    #' @return Vector of raw bytes.
    download_raw = function(id, collection = NULL) {
      readBin(file.path(private$dir, id), "raw", n = 200)
    },

    #' @description
    #' Delete file
    #'
    #' @param id File ID to delete
    #' @param collection Optional collection name this file belongs to.
    #'   This helps speedup lookup - only the single id will be deleted.
    #'   See \href{#method-delete_collection}{
    #'   \code{kelp$delete_collection()}} to remove an entire
    #'   collection.
    #'
    #' @return Nothing, called for side effects
    delete = function(id, collection = NULL) {
      unlink(file.path(private$dir, id))
      if (!is.null(collection)) {
        private$collections[[collection]] <-
          private$collections[[collection]][
            private$collections[[collection]] != id]
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
      ids <- private$collections[[collection]]
      unlink(file.path(private$dir, ids))
      private$collections[[collection]] <- NULL
      invisible(NULL)
    }
  ),

  private = list(
    # Temp directory to store files in
    dir = NULL,
    collections = list()
  )
)
