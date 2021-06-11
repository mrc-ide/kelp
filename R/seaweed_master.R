#' Create a seaweed_master object for interacting with SeaweedFS master
#'
#' See https://github.com/chrislusf/seaweedfs/wiki/Master-Server-API for details
#'
#' @keywords export
seaweed_master <- R6::R6Class(
  "seaweed_master",
  cloneable = FALSE,

  public = list(
    #' @field client A `seaweed_client` object for sending requests
    client = NULL,

    #' @description
    #' Create client object for sending http requests to seaweed master
    #'
    #' @param seaweed_url Root URL of Seaweed master
    #'
    #' @return A new `seaweed_master` object
    initialize = function(seaweed_url) {
      self$client <- seaweed_client$new(seaweed_url)
    },

    #' @description
    #' Assign a file key.
    #'
    #' Increase a number in master server's memory and return fid, volume
    #' server url and volume server public URL. Can be used to to upload
    #' a file to `<publicUrl>/<fid>`.
    #'
    #' @param collection Collection name, acts as a namespace for files.
    #'
    #' @return fid, volume server URL and volume server public URL
    assign = function(collection = NULL) {
      query <- NULL
      if (!is.null(collection)) {
        query[["collection"]] <- collection
      }
      self$client$GET("dir/assign", query = query)
    },

    #' @description
    #' Lookup volume
    #'
    #' Look up volumes by ID or by file ID to get volume URL. Volumes
    #' might move so we need to lookup their URL to get up to date location.
    #'
    #' @param id volume ID or file ID to locate volume for
    #' @param collection Optional collection name this volume belongs to.
    #'
    #' @return List of `seaweed_volume` objects representing the volumes
    lookup = function(id, collection = NULL) {
      query <- list(volumeId = id)
      if (!is.null(collection)) {
        query[["collection"]] <- collection
      }
      res <- self$client$GET("dir/lookup", query = query)
      lapply(res$locations, function(location) {
        seaweed_volume$new(location$publicUrl)
      })
    },

    #' @description
    #' Delete a collection of files
    #'
    #' @param collection Collection name.
    #'
    #' @return Nothing, called for side effects
    delete_collection = function(collection) {
      query <- list(collection = collection)
      self$client$GET("col/delete", query = query)
      invisible(TRUE)
    }
  )
)
