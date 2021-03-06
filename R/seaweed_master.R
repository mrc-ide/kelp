#' Seaweed Master API
#'
#' @description
#' Create a `seaweed_master` object for interacting with SeaweedFS master API
#'
#' See <https://github.com/chrislusf/seaweedfs/wiki/Master-Server-API> for
#' details
#'
#' @keywords export
seaweed_master <- R6::R6Class(
  "seaweed_master",
  cloneable = FALSE,

  public = list(
    #' @description
    #' Create client object for sending http requests to seaweed master
    #'
    #' @param seaweed_url Root URL of Seaweed master
    #'
    #' @return A new `seaweed_master` object
    initialize = function(seaweed_url) {
      private$client <- seaweed_client$new(seaweed_url)
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
      private$client$POST("dir/assign", query = query)
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
    #' @return List of [seaweed_volume()] objects representing the volumes
    lookup = function(id, collection = NULL) {
      query <- list(volumeId = id)
      if (!is.null(collection)) {
        query[["collection"]] <- collection
      }
      res <- private$client$GET("dir/lookup", query = query)
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
      private$client$DELETE("col/delete", as = "raw", query = query)
      invisible(TRUE)
    },

    #' @description
    #' Upload file to SeaweedFS
    #'
    #' @param path Path to file to be uploaded
    #'
    #' @return The uploaded location, name, URL and size
    upload = function(path) {
      if (!file.exists(path)) {
        stop(sprintf("File at %s doesn't exist. Cannot upload.", path))
      }
      private$client$POST("submit", body = list(
        file = httr::upload_file(path)))
    }
  ),

  private = list(
    # A `seaweed_client` object for sending requests
    client = NULL
  )
)
