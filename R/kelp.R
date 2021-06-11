#' Create a kelp object for interacting with SeaweedFS
#'
#' @keywords export
kelp <- R6::R6Class(
  "kelp",
  cloneable = FALSE,

  public = list(
    #' @field master A `seaweed_master` object for sending requests
    master = NULL,

    #' @description
    #' Create client object for sending http requests to seaweed
    #'
    #' @param seaweed_url Root URL of Seaweed
    #'
    #' @return A new `kelp` object
    initialize = function(seaweed_url) {
      self$master <- seaweed_master$new(seaweed_url)
    },

    #' @description
    #' Upload file to SeaweedFS
    #'
    #' @param path Path to file to be uploaded
    #' @param collection Collection name, acts as a namespace for files.
    #'
    #' @return The uploaded file ID.
    upload = function(path, collection = NULL) {
      key <- self$master$assign(collection = collection)
      volume <- seaweed_volume$new(key$publicUrl)
      volume$upload(key$fid, path)
      key$fid
    },

    #' @description
    #' Download file from SeaweedFS
    #'
    #' @param fid SeaweedFS file ID to read
    #' @param path Path to download file to
    #' @param collection Optional collection name this file belongs to.
    #'
    #' @return The file contents
    download = function(fid, path = tempfile(), collection = NULL) {
      volumes <- self$master$lookup(fid, collection)
      ## Download from 1st returned volume for now
      volumes[[1]]$download(fid, path)
    },

    #' @description
    #' Delete file from SeaweedFS
    #'
    #' @param fid SeaweedFS file ID to delete
    #' @param collection Optional collection name this file belongs to.
    #'   This helps speedup lookup - only the single fid will be deleted.
    #'   See `delete_collection` to remove an entire collection.
    #'
    #' @return Nothing, called for side effects
    delete = function(fid, collection = NULL) {
      volumes <- self$master$lookup(fid, collection)
      ## TODO: Do we have to delete from every volume?
      lapply(volumes, function(volume) {
        volume$delete(fid)
      })
      invisible(TRUE)
    },

    #' @description
    #' Delete a collection of files
    #'
    #' @param collection Collection name.
    #'
    #' @return Nothing, called for side effects
    delete_collection = function(collection) {
      self$master$delete_collection(collection)
    }
  )
)
